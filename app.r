library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(scales)
library(dplyr)
library(sigmoid)
library(ggthemes)

source("model.r")

ui <- fluidPage(theme = shinytheme("cosmo"),
                # in-line css styling
                tags$style(
                  "
                  body {
	                  background-image: linear-gradient(to top, #096979 0%, #616294
		                  100%);
                  }
                  .top_1 {
                    color: Black;
                    font-size: 30px;
                  }
                  .top_2{
                  color: White;
                  font-size: 30px;
                  }
                  .text_normal{
                    font-size: 20px;
                    color: Black;
                  }
                  "
                ),
                
                navbarPage(
                  theme = "darkly",
                  "Project 3",
                  tabPanel("Introduction",
                           sidebarPanel(
                             p(class = "top_1", "Company Description"),
                             p(class = "text_normal", "Third party company that specializes in making fligh plan recommendations to travelers.
                               They wish to expand into the internett provider recommendation space."),
                             p(class = "top_1", "Recommendation"),
                             p(class = "text_normal", "The provided tool allows for a visualy appealing ranking of the providers in the customers area.
                               The tool can be directly provided for customer use, or the information it provides may serve as the foundation of a recommendation.
                               The rankings are based on three factors; upload-speed, download-speed and latency.
                               It also allows for the user to input an importance weighting for each of these that will be reflected in the ranking."),
                             width = 3
                             
                           ),
                           
                           mainPanel(
                             p(class = "top_2", "Introduction Video:"),
                             tabsetPanel(type = "tab",
                                         tabPanel(HTML('<iframe width="999" height="666" src="https://www.youtube.com/embed/fJXzIU4vmuA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                             )
                           )
                  ),
                  
                  tabPanel("Recommendation Tool",
                           sidebarPanel(
                             p(class = "top_1", "Options:"),
                             
                             selectInput("state_in", "State:", c("All", "NSW", "VIC", "QLD", "NT + SA", "ACT", "WA", "TAS")),
                             
                             selectInput("tier_in", "Tier:", c("All", "12/1 Mbps", "25/5 Mbps", "25/10 Mbps", "50/20 Mbps", "75/20 Mbps", "100/20 Mbps", "100/40 Mbps", "250/25 Mbps")),
                             
                             sliderInput("up_pref_in", "Level of concern for upload speed:", value = 1, min = 0, max = 1),
                             
                             sliderInput("down_pref_in", "Level of concern for download speed:", value = 1, min = 0, max = 1),
                             
                             sliderInput("lat_pref_in", "Level of concern for latency:", value = 1, min = 0, max = 1),
                             
                             actionButton("submittion", "Submit", class = "btn btn-primary"),
                             
                             width = 3
                           ),
                          
                           mainPanel(
                             p(class = "top_2", "Ranking/Recommendation:"),
                             verbatimTextOutput("result"),
                             
                             plotOutput("comparison"),
                             width = 9
                             
                           )
                  ),
                  
                  
                  tabPanel("Evidence",
                           sidebarPanel(
                             p(class = "top_1", "Explanation/Evidence"),
                             p(class = "text_normal", "The calculations are based on soandso.
                               they are made soandso.
                               as time passes the tool becomes less valid/applicable.
                               Some cat less inputs"),
                             width = 4
                           
                           ),
                           
                           mainPanel(
                             p(class = "top_2", "Plot without using the sigmoid function. State: All, Tier: All, Level of concern: 1"),
                             plotOutput("evidence"), width = 8
                           ))
                )
)

server <- function(input, output, session) {

  UI_input <- reactive ({
   
   # Calculate recommendation
   rec_plot <- recommend(input$state_in, input$tier_in, input$up_pref_in, input$down_pref_in, input$lat_pref_in)
   print(rec_plot)

  })
  
  # Text box
  output$result <- renderPrint({
    if(input$submittion>0){
      isolate("Calculation complete, best provider has the highest score. Score of 0 is an indication that there was no available data.")
    }
    else{
      return("Ready to make recommendation")
    }
  })
  
  # Display Plot Ranking
  output$comparison <- renderPlot({
    if (input$submittion>0) { 
      isolate(UI_input())
    } 
  })
  
  # Display Plot Evidence
  output$evidence <- renderPlot({
    evidence("All", "All", 1, 1, 1)
  })
  
}

shinyApp(ui = ui, server = server)
