library(shiny)
library(shinydashboard)
library(tidyverse)

pos <- read.csv("csv/pos.csv")
neg <- read.csv("csv/neg.csv")
bind_ <- rbind(pos,neg)
header <- dashboardHeader(title = "Body Fat Predictor")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "search", icon = icon("dashboard"),
                 selectInput("option", "Menu:",
                             c("Classic Dozen"=	"classic",
                               "Voodoo Dozen"=	"voodoo",
                               "Vegan Voodoo Dozen"=	"vegan",
                               "Holiday Dozen"=	"holiday",
                               "Peppermint Hot Cocoa Cannolo"=	"peppermint",
                               "Star of David"	="david",
                               "Bourbon Maple Pecan"=	"bourbon",
                               "Strawberry Cannolo"=	"strawberry",
                               "Caramel Cha Cha Chai"=	"caramel",
                               "Dirt"=	"dirt",
                               "Old Dirty Bastard"="bastard",
                               "Raised Glazed"	="glazed",
                               "Chocolate Ring"	="chocolate",
                               "Oh Captain, My Captain"	= "captain",
                               "Grape Ape"=	"grape",
                               "Voodoo Bubble"	="bubble",
                               "The Homer"	= "homer",
                               "Chuckles"=	"chuckles",
                               "Bacon Maple Bar"=	"bacon",
                               "Maple Bar"=	"maple Bar",
                               "Portland Cream"	="portland",
                               "Maple Cream"= "cream",
                               "Raspberry Romeo"=	"raspberry",
                               'Mango Tango'=	'mango',
                               'Voodoo Doll'= 'doll',
                               'Viscous Hibiscus'= 'hibiscus',
                               'Maple Blazer Blunt'='blunt',
                               'Guava Colada'	='colada',
                               'Plain Cake'	='plain',
                               'Chocolate Cake'	='cake',
                               'Powdered Sugar Cake'=	'powdered',
                               'Cinnamon Sugar Cake'=	'cinnamon',
                               'Peanut Cake'=	'peanut',
                               'Sprinkle Cake'=	'sprinkle',
                               'Ring of Fire'= 'fire',
                               'Marshall Mathers'=	'marshall',
                               'Double Chocolate'=	'double',
                               'Chocolate Coconut Cake'= 'coconut',
                               'Diablos Rex'=	'diablos',
                               'Butterfingering'=	'butterfingering',
                               'Buttermilk Bar'	='buttermilk',
                               'Blueberry Cake'	='blueberry',
                               'Glazed Old Fashioned'= 'fashioned',
                               'Chocolate Old Fashioned'=	'chocolate',
                               'Maple Old Fashioned'=	'maple',
                               'Apple Fritter'= 'fritter',
                               'Memphis Mafia'=	'memphis',
                               'School Daze “PB&J”'	='school')),
                 radioButtons("sort", h4("Sorted by"),
                               choices = list("useful" = 0, "date" = 1, "stars" = 2),
                               selected =0),
                 radioButtons("sortT", h4(""),
                              choices = list("decreasing" = 0, "increasing" = 1),
                              selected =1),
                 startExpanded = TRUE
                 )
        
    )
)

Body <- dashboardBody(
    fluidRow(
        column(12, align="center",
        div(style="display: inline-block;",img(src="Histogram.png", height=250, width=300)),
        div(style="display: inline-block;",img(src="BoxPlotTV.png", height=250, width=300)),
        div(style="display: inline-block;",img(src="BoxPlotStreet.png", height=250, width=300)),
        div(style="display: inline-block;",img(src="BoxPlotPrice.png", height=250, width=300)),
        div(style="display: inline-block;",img(src="BoxPlotNoise.png", height=250, width=300)),
        div(style="display: inline-block;",img(src="BoxPlotGroups.png", height=250, width=300)))),
    
    
    
        tabItem(tabName = "search",
                htmlOutput("text")
        )
    
)
ui <- dashboardPage(
    dashboardHeader(title = "STAT628M3"),
    sidebar,
    Body
)

m <- function(option, sort, t){
    output1 <- data.frame()
    for(i in 1:length(bind_[,1])){
        if(grepl(option, bind_[i,9], fixed = TRUE)){
            output1 <- rbind(output1, bind_[i,])
        }
    }
    if(length(output1)==0){
        return("No Match")
    }
    if(t == 1){
        if(sort == 1){
            output1 <- output1[order(output1$date, decreasing = T),]
        }
        else if(sort == 2){
            output1 <- output1[order(output1$stars, decreasing = T),]
        }
        else{
            output1 <- output1[order(output1$useful, decreasing = T),]
        }
    }
    else{
        if(sort == 1){
            output1 <- output1[order(output1$date, decreasing = F),]
        }
        else if(sort == 2){
            output1 <- output1[order(output1$stars, decreasing = F),]
        }
        else{
            output1 <- output1[order(output1$useful, decreasing = F),]
        }
    }
    
    
    a <- paste("stars:", output1$stars,"  useful:", output1$useful,"  date:", output1$date, sep = "")
    b <- paste("NEW COMMENT", 1:length(a), sep = "")
    return(paste("", b, a, output1$text, sep = "<br>"))
}

server <- function(input, output) {
    output$text <- renderUI({
        HTML(paste(m(input$option, input$sort, input$sortT), collapse = "<br><br><br>"))
    })
}

shinyApp(ui = ui, server = server)

