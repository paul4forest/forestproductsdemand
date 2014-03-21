# To start the server:
# runApp("code/shiny")

library(shiny)
library(ggplot2)
# Set path to root of project
if (grepl("shiny",getwd())) setwd("../..")
load("enddata/EU28 paper products base year 2010.rdata")
load("enddata/EU28 roundwood base year 2010.rdata")
load("enddata/EU28 sawnwood base year 2010.rdata")


shinyServer(function(input, output) {
    
    dataset <- reactive(function() {
        c('Paper and Paperboard', 'Sawnwood', 'Roundwood')
        if (input$products=="Paper and Paperboard")
            dtf <- paperproducts$entity
        if (input$products=="Sawnwood")
            dtf <- sawnwood$entity
        if (input$products=="Roundwood")
            dtf <- roundwood$entity
        
        # Sort countries by highest Total consumption
        highcons <- dtf[grepl("Total", dtf$Item) & dtf$Year==max(dtf$Year),]
        highcons <- highcons[order(-highcons$Consumption),]
        subset(dtf, 
               Country %in% head(highcons$Country, n=input$nbcountries))
    })
        
    output$plot <- reactivePlot(function() {
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .')
            p <- p + facet_grid(facets, scales=input$scales)
        if (input$smooth)
            p <- p + geom_smooth()
        print(p)
    }, height=700)
})