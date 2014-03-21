# Inspired by https://gist.github.com/jcheng5/3239667


library(shiny)
library(ggplot2)

load("enddata/EU28 paper products base year 2010.rdata")
dataset <- paperproducts$entity

shinyUI(pageWithSidebar(
    
    headerPanel("Forest Products Explorer"),
    sidebarPanel(
        selectInput('products', 'Products', 
                    c('Paper and Paperboard', 'Sawnwood', 'Roundwood')),
        sliderInput('nbcountries', 'Number of Countries', min=1, max=length(unique(pp$Country)),
                    value=5, step=1, round=0),
        selectInput('x', 'X', names(dataset), selected = 'Year'),
        selectInput('y', 'Y', names(dataset), selected = "Consumption"),
        selectInput('color', 'Color', c('None', names(dataset)), 
                    selecte = "Price"),
        checkboxInput('smooth', 'Smooth'),
        selectInput('facet_row', 'Facet Row', c(None='.', names(dataset)),
                    selected="Item"),
        selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)),
                    selected = "Country"),
        selectInput('scales', 'Scales', c("fixed", "free_y", "free_x", "free"))
    ),
    
    mainPanel(
        plotOutput('plot')
    )
))