library(shiny)
library(shinyjs)
library(shinymaterial)
library(shinydashboard)
library(mongolite)
library(stringi)
library(dplyr)
library(arules)
library(arulesViz)
library(visNetwork)


# Define UI
ui <- dashboardPage(skin= 'yellow',
                    dashboardHeader( disable= T , title= "Store items Recommender"),
                    dashboardSidebar(disable = TRUE ),
                    dashboardBody(
                      shinyjs::useShinyjs(),
                      div(style= "background-image: url('stores.jpg'); background-size: cover;  z-index: 2; overflow: hidden" ,
                          div(
                            style= "background-color: rgba(0,0,0,0.55); overflow: hidden ",
                            p("Hostel Store", style= "padding: 100px 50px; color: #ffffff; font-size: 50px; font-weight: 1000")
                            #img(src= "stores.jpg", style= "width: 100%;"),
                          )),
                      navbarPage(
                        title= span( "", style="color: orange" ) ,
                        div(
                          title= span("Recommender", style= "color: orange; font-size: 17px; font-weight: 1000"),
                          fluidRow(
                            uiOutput("recommender1"),
                            uiOutput("recommender2"),
                            uiOutput("recommender3"),
                            uiOutput("recommender4"),
                            uiOutput("recommender5"),
                            uiOutput("recommender6"),
                            div(id= "recommend"),
                          ),
                          div(style= "height: 1px; background-color: black"),
                          fluidRow(
                            div(id= "itemlister", style= "display: inline-grid") 
                          ),
                          actionButton('showlist', "Show Items", style= "background-color: orange; color: white; padding: 4px; width: 100%; height: 50px; font-size: 25px" ),
                        ),
                        tabPanel(span("Market Basket Analysis", style= "color: orange; font-size: 17px; font-weight: 1000"),
                                 div(id= 'dashboard',
                                    fluidRow(
                                    column(3,
                                    wellPanel(
                                     style= "background-color: #00b9fb; color: white",
                                     h2("Support"),
                                     h4("min 1%")
                                     )),
                  
                                    column(3,
                                    wellPanel(
                                           style= "background-color: orange; color: white",
                                           h2("Confidence"),
                                           h4("min 10%")
                                    )),
                                  
                                    column(3,
                                    wellPanel(
                                           style= "background-color: teal; color: white;",
                                           h2("Lift"),
                                           h4(">= 1")
                                    )),
                                    column(3,
                                           wellPanel(
                                             style= "background-color: #9969c7; color: white;",
                                             tags$div(id= "numberOfRules")
                                           ))
                                    
                                    ),#fluidrow
                                    fluidRow(
                                      column(6,
                                             style= "color: red",
                                             wellPanel(
                                             numericInput("scatterRules", "Enter number of rules you want to plot" ,"50"),
                                             plotOutput("scatter")
                                             )),
                                      column(6,
                                            wellPanel(
                                              sliderInput(inputId = "itemCount", label = "Select number of items" , min = 1  , max = 40, value = 10),
                                              plotOutput("frequency")
                                                ))
                                    ),
                                    fluidRow(
                                      column(12,
                                             wellPanel(
                                               fluidRow(
                                              column(6, 
                                               numericInput("graphRules1", "Start from rule" ,"1"),
                                              ),
                                              column(6,
                                               numericInput("graphRules2", "To rule" ,"50"),
                                               )
                                              ),
                                                visNetworkOutput("graph", height = "700px")
                                             ))
                                    ),
                                    fluidRow(
                                      column(6,
                                             wellPanel(
                                               plotOutput("grouped")
                                             )),
                                      column(6,
                                             wellPanel(
                                               fluidRow(
                                                 column(6, 
                                                        numericInput("paracord1", "Start from rule" ,"1"),
                                                 ),
                                                 column(6,
                                                        numericInput("paracord2", "To rule" ,"5"),
                                                 )
                                               ),
                                               plotOutput("paracord")
                                             ))
                                    )
                                    
                                  )#div
                                 )
                      )#navbarPage
                    )#dashboardBody
)#dashboardPage