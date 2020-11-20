library(shiny)
library(shinydashboard)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(imager)
library(tidyverse)
library(parallel)
library(data.table)

# setwd("/Users/alucard/Desktop/R")
pubg_data <- read_csv('PUBG_ShinySampleData.csv')
map <- load.image('ERANGEL.jpg')
df_pubg <- data.frame(pubg_data$match_id,
                      pubg_data$killer_position_x,pubg_data$killer_position_y,
                      pubg_data$victim_position_x,pubg_data$victim_position_y,
                      pubg_data$time)

#########################################################Shiny STARTS FROM HERE
ui <- dashboardPage(skin = 'yellow',
                    dashboardHeader(title = 'PUBG - The Distribution of death', titleWidth = 400),
                    dashboardSidebar(width = 200,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('PUBG Heatmap', tabName = 'The PUBG Heatmap', icon = NULL)
                                                 # menuItem('CI', tabName = 'CI', icon = NULL)
                                     )
                    ),
                    dashboardBody(
                      sidebarPanel(
                        radioButtons("match", "Match:",
                                     list("Match1" = "2U4GBNA0YmkfVhVAA391nmjCo1oVv6YBCuBwVOHAEegu1uvOZFOJoeLFQFAbYn5P",
                                          "Match2" = "2U4GBNA0Ymnu2puyqf_NtQlNu-P-GhW9oVdBiQ14E36G-AiBhSA2Xn0jiFh8E__R",
                                          "Match3" = "2U4GBNA0YmnPbefQNZwA5BwIZZGr1rbZ9ME75ZSoLLy_5k7XslGLplmzebtdZiO8")),
                        
                        br(),
                        sliderInput("time", 
                                    "time of the match:", 
                                    min = 0, 
                                    max = 2400,
                                    value = 300,
                                    step=300)
                      ),
                      
                      mainPanel(
                        plotOutput("image1")
                      )
                    ))
###end of introduction

##########################################################################################Server code starts here

server <- function(input, output) {
  datasetInput <- reactive({
    df_pubg %>% filter(df_pubg$pubg_data.match_id==input$match
                      & input$time-300 <= df_pubg$pubg_data.time 
                      & df_pubg$pubg_data.time <= input$time
  )
   
#    df_pubg %>% filter(input$time-300 <= df_pubg$pubg_data.time 
#                       df_pubg$pubg_data.time <= input$time)
})
       
  output$image1 <- renderPlot({
    options(scipen=999)
    dataset<-datasetInput()
    ggplot(dataset, aes(pubg_data.killer_position_x,
                        pubg_data.killer_position_y))+
           xlim(0,800000)+ ylim(0,800000)  + 
      annotation_raster(map, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
      stat_density2d(geom = "polygon", aes(fill=..level..)) + 
      #geom_point(size=0.2)+
      scale_fill_gradient(low="transparent",high="red")+
      scale_y_reverse()+
      coord_fixed()+
      ggtitle("The Distribution of death")+
      xlab("position_x") + ylab("position_y")
  })
} 
####################################################end of introduction


shinyApp(ui, server)


