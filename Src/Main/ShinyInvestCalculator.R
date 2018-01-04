#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# To-Do: Total pay is not correct
# To-Do: Need to account for fail withdrawal 
# To-Do: Add warning message when componding rate period unit does not match other inputs, such as withdrawal amount period
# To-Do: Add a smart way to help user to figure how to reach target
# To-Do: Always use monthly componding rate

library(shiny)
library(stats)

num2CommasNumString <- function (x,tail0 = 2) {
  parts <- strsplit (sprintf(paste0("%.",tail0,"f"),x),"\\.")
  parts[[1]][1] = gsub("\\B(?=(\\d{3})+(?!\\d))", ",", parts[[1]][1], perl=TRUE)
  paste0(parts[[1]],collapse = '.')
}

commasNumString2num <- function (x) {
  as.numeric(gsub(",","", x))
}


cal.compound <- function (f, v, r, b = 0, d = 0, ur = 12, ub = 1, ud = 1) { # ur = 1, 3, 6, 12
  fArr <- matrix(vector(mode = "numeric", length = (v + 1)*2),nrow = 2) # Initiate
  fArr[,1] <- f
  for (inc in 1:v) {
    # Compounding rate
    fArr[1,inc+1] <- fArr[2,inc] * (1+r)
    # Increase/decrese fund
    if ((ub == ud) & (ub == TRUE)) { #case1: yearly,yearly
      if (inc%%12 == 0) { # Check at every year end for yearly change
        fArr[2,inc+1] <- max((fArr[1,inc+1] - d),0) + b # Withdrawal takes place first then add new fund
      }
      else {
        fArr[2,inc+1] <- fArr[1,inc+1]
      }
    }
    else if (ub & !ud) { #case2: yearly,monthly
      if (inc%%12 == 0) { # Check at every year end for yearly change
        fArr[2,inc+1] <- max(fArr[1,inc+1] - d,0) + b # Add new fund yearly
      }
      else {
        fArr[2,inc+1] = max(fArr[1,inc+1] - d,0) # Withdrawal takes place every month
      }
    }
    else if(!ub & ud) { #case3: monthly,yearly
      if (inc%%12 == 0) { # Check at every year end for yearly change
        fArr[2,inc+1] <- max(fArr[1,inc+1] - d,0) + b # Withdrawal takes place every year
      }
      else {
        fArr[2,inc+1] = fArr[1,inc+1] + b # New fund adds every month
      }
    }
    else { #case4: monthly,monthly
      fArr[2,inc+1] <- max(fArr[1,inc+1] - d,0) + b # Withdrawal takes place every year
    }
  }
  return(fArr)
}

# optim(c(400,800),fn = opt.compound.target,method = "L-BFGS-B", lower = c(800,400), upper = c(965+400,800),t = 20000, f = f, v = v, r = r, ur = ur, ub = ub, ud = ud)
opt.compound.target <- function(x,t,f,v,r,ur,ub,ud) {
  b <- x[1]
  d <- x[2]
  fArr <- cal.compound(f,v,r,b,d,ur,ub,ud)
  finalF <- fArr[1,length(fArr[1,])]
  (t+0.05 - finalF)^2 # magin number 0.05 to make sure finalF > t. Use square error to avoid the singularity
}


# Define UI for application that calculate investment return
defaultFund <- 10000

ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("EZ Investment Calculator"),
  
  # Sidebar with a slider input for number of bins 
  tags$style(type="text/css", 
             '.textGroup { display:inline-block ; margin-right:10px ; vertical-align: top;}',
             '.shiny-input-container label {display:hidden}',
             '.form-group {margin:0}',
             '.inputText {margin-bottom:10px}',
             '.selectize-control {margin:0px}',
             '.rowWrapper {margin-bottom:10px}'
  ),
  
  sidebarPanel(
    div(class = "rowWrapper", style = "margin-bottom:15px" ,div(class = "inputText", strong("Initial Fund ($)")),
        numericInput("niFund", NULL, 10000, min = 0, step = 100, width = "100%")),
    div(class = "rowWrapper",div(class = "inputText", strong("Compounding Rate (%)",br("(Note: Not APR)"))),
        div(class = "textGroup", style="width:40%", numericInput("niRate", NULL, 3, min = 0, step = 0.01, width = "100%")),
        div(class = "textGroup", style="width:40%", selectInput("siRateUnit",NULL
                                                  ,c("monthly","quartly","semi-annually","annually"), selected = "annually", width = "100%"))),
    div(class = "rowWrapper",div(class = "inputText", strong("Contribution ($)",br("(Incurred after end of each term)"))),
        div(class = "textGroup", style="width:40%", numericInput("niCont",NULL, 0, min = 0, step = 10, width = "100%")),
        div(class = "textGroup", style="width:40%", selectInput("siContUnit",NULL,c("monthly","yearly"), width = "100%"))),
    div(class = "rowWrapper",div(class = "inputText", strong("Investment Period")),
        div(class = "textGroup", style="width:40%", numericInput("niInvest",NULL, 10, min = 0, width = "100%")),
        div(class = "textGroup", style="width:40%", selectInput("siInvestUnit",NULL,c("month","year"), selected = "year", width = "100%"))),
    div(class = "rowWrapper", div(class = "inputText", strong("Withdrawal amount ($)",br("(End of each term)"))),
        div(class = "textGroup", style="width:40%", numericInput("niWithdraw",NULL, 0, min = 0, step = 10, width = "100%")),
        div(class = "textGroup", style="width:40%", selectInput("siWithdrawUnit",NULL,c("monthly","yearly"), selected = "yearly", width = "100%"))),
    div(class = "rowWrapper", style = "margin-bottom:15px" ,div(class = "inputText", strong("Target ($)")),
        numericInput("niTarget", NULL, NA, min = 0, step = 100, width = "100%"))
  ),
  mainPanel(
    verbatimTextOutput("reportSummary"),
    tableOutput("reportTable"),
    plotOutput("growthPlot")
  )
))

# Define server logic required to calculate investment return

server <- shinyServer(function(input, output,session) {
  
  # Extract input and convert from string to numeric
  xNum <- reactive({
    req((input$niFund >= 0) & is.numeric(input$niFund))
    req((input$niRate >= 0) & is.numeric(input$niRate)) 
    req(input$niInvest > 0 & (floor(input$niInvest) == input$niInvest) & is.numeric(input$niInvest))
    req(is.numeric(input$niCont) & (input$niCont >= 0))
    req(is.numeric(input$niWithdraw) & (input$niWithdraw >=0))
    req(is.na(input$niTarget) | (is.numeric(input$niTarget) & (input$niTarget >= 0)))
    
    list(
    f = input$niFund,
    r = input$niRate,
    b = input$niCont,
    v = input$niInvest,
    d = input$niWithdraw,
    t = input$niTarget
    )
  })
  
  # Get unit for each input
  xUnit <- reactive({
    ur <- switch(input$siRateUnit, monthly = 1, quartly = 3, "semi-annually" = 6, annually = 12)
    list(
      r = ur,
      b = input$siContUnit=="yearly",
      v = input$siInvestUnit=="year",
      d = input$siWithdrawUnit=="yearly"
    )})
  
  # Report
  summaryReport <- reactive({
    xNum <- xNum()
    xUnit <- xUnit()
    f <- xNum$f
    b <- xNum$b # add
    d <- xNum$d # minus
    r <- (xNum$r/100 + 1)^(1/xUnit$r) - 1 # Rate
    v <- ifelse(xUnit$v, 12 * xNum$v, xNum$v) # always use month
    ur <- xUnit$r
    ub <- xUnit$b
    ud <- xUnit$d
    target <- xNum$t

    # Without investment
    noInvestArr <- cal.compound(f,v,0,b,d,ur,ub,ud)
    noInvest <- noInvestArr[1,length(noInvestArr[1,])]
    noInvestStr <- num2CommasNumString(noInvest,2)

    # With investment
    fArr <- cal.compound(f,v,r,b,d,ur,ub,ud)
    # Organizing
    if (xUnit$v) {
      fArr <- fArr[2,seq(1,length(fArr[2,]),12)]
      noInvestArr <- noInvestArr[2,seq(1,length(noInvestArr[2,]),12)]
    } 
    else  {
      fArr <- fArr[2,]
      noInvestArr <- noInvestArr[2,]
    }
    finalF = fArr[length(fArr)]
    
    # How to reach target
    if (!is.na(target)) {
      # Case1: target can be reached
      # Case2: target cannot be reach: 
        # Case2.1: reduce payment (fastest option 1, may not work)
                  #optim(c(900,0),fn = opt.compound.target,method = "L-BFGS-B", lower = c(0,0), upper = c(900,Inf),t = 20000, f = f, v = v, r = r, ur = ur, ub = ub, ud = ud)
        # case2.2: increase contribution with no payment (fastest option 2, largest contribution no payment, always work the best)
                  # optim(c(900,0),fn = opt.compound.target,method = "L-BFGS-B", lower = c(0,0), upper = c(Inf,1),t = 20000, f = f, v = v, r = r, ur = ur, ub = ub, ud = ud)
        # case2.3: maintain payment but increase contribution (max pay, larger contribution, will work)
                  #optim(c(900,10),fn = opt.compound.target,method = "L-BFGS-B", lower = c(0,), upper = c(Inf,Inf),t = 20000, f = f, v = v, r = r, ur = ur, ub = ub, ud = ud)
        # case2.4: Best (my rule, resulting low payment, but slightly increase in contribution, will work)
                  #???
                  #optim(c(400,800),fn = opt.compound.target,method = "L-BFGS-B", lower = c(800,400), upper = c(976+400,800),t = 20000, f = f, v = v, r = r, ur = ur, ub = ub, ud = ud)
        
      # d.desire.close
      # b.desire.close
      # d.desire.noPay
      # b.desire.noPay
      # d.desire.fixIncome
      # b.desire.fixIncome
      
    }
    
    # Collect data
    investStr <- num2CommasNumString(finalF,2)
    gainInvFundStr <- num2CommasNumString(finalF-f,2)
    gainNoInvFundStr <- num2CommasNumString(noInvest-f,2)
    gainStr <- num2CommasNumString(finalF-noInvest,2)
    gainInvFundPercStr <- num2CommasNumString((finalF-f)/f*100,2)
    gainNoInvFundPercStr <- num2CommasNumString((noInvest-f)/f*100,2)
    gainPercStr <- num2CommasNumString(max((finalF-noInvest)/noInvest,-1)*100,2)
    # gainDiff = (Current - previous base - contribution)
    gainDiffArr <- c(0,diff(fArr)) - c(0,diff(noInvestArr))
    gainPercArr <- gainDiffArr/c(fArr[1],fArr[1:(length(fArr)-1)])
    gainPercArr[is.infinite(gainPercArr)] <- NA
    # Accumulated gain that respect to the previous invested amount (the baseline)
    gainDiffCumArr <- fArr-c(noInvestArr[1],noInvestArr[1:(length(noInvestArr)-1)]) - c(0,diff(noInvestArr))
    gainPercCumArr <- gainDiffCumArr/c(noInvestArr[1],noInvestArr[1:(length(noInvestArr)-1)])
    gainPercCumArr[is.infinite(gainPercCumArr)] <- NA
    payoutStr<- num2CommasNumString(d*v/ifelse(ud,12,1),2)
    
    # list output
    list(noinvest = noInvestStr, invest = investStr, investArr = fArr, noInvestArr = noInvestArr,
         gainNoInvFund = gainNoInvFundStr, gainNoInvFundc = gainNoInvFundPercStr,
         gainInvFund = gainInvFundStr, gainInvFundPerc = gainInvFundPercStr,
         gain = gainStr, gainPerc = gainPercStr, 
         gainDiffArr = gainDiffArr, gainPercArr = gainPercArr,
         gainDiffCumArr = gainDiffCumArr, gainPercCumArr = gainPercCumArr,
         payout = payoutStr)
  })
  # Text box
  output$reportSummary <- renderText({
    xNum <- xNum()
    summaryReport <- summaryReport()
    fArr <- summaryReport$investArr
    # Either increase contribution or reduce pay
    if (!is.na(xNum$t)) {
      if (fArr[length(fArr)] >= xNum$t) {
        targetStr <- paste0("Great! Your target can be reached!")
      }
      else {
        targetStr <- paste0("Possible solutions are: \n")  
      }
    }
    else {
      targetStr <- ""
    }
    # Increase contribution to ?$ to reach target in ? years"

    paste(
      paste0("Initial Fund: ", "$", num2CommasNumString(xNum$f)),
      paste0("Without Investment: ", "$", summaryReport$noinvest),
      paste0("With Investment: ", "$", summaryReport$invest),
      "",
      paste0("Gain/Loss without Investment (to initial fund): ", "$", summaryReport$gainNoInvFund),
      paste0("Gain/Loss ratio without Investment (to initial fund): ", summaryReport$gainNoInvFundc, "%"),
      "",
      paste0("Gain/Loss with Investment (to initial fund): ", "$", summaryReport$gainInvFund),
      paste0("Gain/Loss ratio with Investment (to initial fund): ", summaryReport$gainInvFundPerc, "%"),
      "",
      paste0("Gain/Loss (to no investment): ", "$", summaryReport$gain),
      paste0("Gain/Loss ratio (to no investment): ", summaryReport$gainPerc, "%"),
      "",
      paste0("Total pay out: ", "$", summaryReport$payout),
      "",
      paste0(targetStr,"\n"),
      paste0("*Note: Fund deposit/withdrawal takes place after the compounding rate calculation."),
      "",
      sep = "\n")
  })
  # Table 
  output$reportTable <- renderTable({
    xNum <- xNum()
    xUnit <- xUnit()
    summaryReport <- summaryReport()
    m <- data.frame(0:xNum$v, summaryReport$investArr, 
                    summaryReport$noInvestArr, 
                    summaryReport$gainDiffArr, 
                    summaryReport$gainPercArr*100,
                    summaryReport$gainDiffCumArr,
                    summaryReport$gainPercCumArr*100)
    colnames(m) <- c({if (xUnit$v) "Year" else "Month"}, 
                     "Asset ($)", 
                     "Baseline ($)", 
                     "Gain ($) (each term)", 
                     "Gain (%) (each term)",
                     "Gain ($) (to baseline)",
                     "Gain (%) (to baseline)")
    return(m)
  },rownames=FALSE, colnames=TRUE )
  
  # Plot
  # Todo: Show K if > 1000 in y axis
  output$growthPlot <- renderPlot({
    xNum <- xNum()
    xUnit <- xUnit()
    summaryReport <- summaryReport()
    barplot(summaryReport$investArr,
            xlab = paste0("Time ",{if (xUnit$v) "(Year)" else "(Month)"}),
            ylab = "Dollar ($)",
            names.arg = 0:xNum$v)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)