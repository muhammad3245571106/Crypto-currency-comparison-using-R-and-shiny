library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(skimr)
library(zoo)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(skimr)
library(lubridate)

BTC <- read.csv("Bitcoin.csv")
btc <- read.csv("BTC.csv")
ETH <- read.csv("Ethereum.csv")

Dates <- ymd(BTC$date)
typeof(Dates)
class(Dates)
BTC$date <- Dates

Dates2 <- ymd(ETH$date)
typeof(Dates2)
class(Dates2)
ETH$date <- Dates2

dates <- ymd(btc$Date)
typeof(dates)
class(dates)
btc$Date <- dates

m <- mean(btc$Volume)
sd <- sd(btc$Volume)

model <- lm(btc$Close ~ btc$Open)

btc_new <- btc[btc$Date > "2022-01-23" &    # Extract data frame subset
                 btc$Date < "2022-10-23", ]

btc_new2 <- btc[btc$Date > "2022-11-08" &    # Extract data frame subset
                  btc$Date < "2022-12-08", ]

shinyServer(function(input,output,session){
  output$mydatatable <- renderDataTable({
    BTC
  })
  output$plot1 <- renderPlotly({
    ggplot(btc, aes(x = Date, y = Close ,col ='red')) +
      geom_line() + 
      scale_x_date(date_labels = "%Y-%m")
  })
   output$plot2 <- renderPlotly({
     ggplot(btc_new, aes(x = Date, y = Close)) +            # Draw ggplot2 plot
       geom_line() + 
       scale_x_date(date_labels = "%Y-%m")
   })
   output$plot3 <- renderPlotly({
     ggplot(btc_new2, aes(x = Date, y = Close)) +            # Draw ggplot2 plot
       geom_line() + 
       scale_x_date(date_labels = "%m-%d")
   })
   
   output$plot4 <- renderPlotly({
     plot_ly(data= BTC,y = ~close, type = "box")
   })
   
   output$plot5 <- renderPlotly({
     ggplot(ETH, aes(x = date, y = close)) +            # Draw ggplot2 plot
       geom_line() + 
       scale_x_date(date_labels = "%Y-%m")
   })
   
   output$plot6 <- renderPlotly({
     plot_ly(data= ETH,y = ~close, type = "box")
   })
   
   output$plot7 <- renderPlotly({
     plot_ly(data = btc,x=~Volume,type = "histogram",color = "red")
   })
   
   
   output$plot9 <- renderPlotly({
     plot_ly(data = ETH,x=~volume,type = "histogram",color = "red")
   })
   
   
   
   
   
   
 
  #summary of bitcoin
  
  
  output$mS <- renderPrint({
    btc %>% 
      summary()
   
  })
  
  output$mDS <- renderPrint({
    ETH %>% 
     summary(ETH)
  })


  
  
  
  
  #regression model
  
  
  output$RS <- renderPrint({
   model %>% 
      summary()
  })
  
  output$PD <- renderPlotly({
    ggplot(btc, aes(x = Volume, y = pnorm(Volume,mean =m,sd = sd))) +            # Draw ggplot2 plot
      geom_point() 
  })
  

  
  output$P <- renderValueBox({
    valueBox(
      value = 39.8529 + 0.9979*16805.20 ,subtitle = "Expected Closing of BTC depending on Opening value"
    )
  })
  })