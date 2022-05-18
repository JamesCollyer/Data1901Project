library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(scales)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  theme = "cyborg",
                  "Project 3",
                  tabPanel("Video Introduction",
                           sidebarPanel(
                            
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tab",
                                         tabPanel(HTML('<iframe width="750" height="500" src="https://www.youtube.com/embed/fJXzIU4vmuA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                             )
                             
                           )
                  ),
                  
                  tabPanel("Recommendation Tool",
                           sidebarPanel(
                             HTML('<h4>Options'),
                             
                             selectInput("state_in", "State:", c("NSW", "VIC", "QLD", "NT + SA", "ACT", "WA", "TAS")),
                             
                             selectInput("tier_in", "Tier:", c("12/1 Mbps", "25/5 Mbps", "25/10 Mbps", "50/20 Mbps", "75/20 Mbps", "100/40 Mbps", "250/25 Mbps", "500-999/50 Mbps", "")),
                             
                             sliderInput("up_pref_in", "Level of concern for upload speed:", value = 1, min = 0, max = 1),
                             
                             sliderInput("down_pref_in", "Level of concern for download speed:", value = 1, min = 0, max = 1),
                             
                             sliderInput("lat_pref_in", "Level of concern for latency:", value = 1, min = 0, max = 1),
                             
                             actionButton("submittion", "Submit", class = "btn btn-primary")
                           ),
                          
                           mainPanel(
                             tags$label(h3("Best Provider:")),
                             verbatimTextOutput("result"),
                             tableOutput('tabledata')
                             
                           )
                  ),
                  
                  
                  tabPanel("Evidence",
                           mainPanel(
                             
                           
                           )),
                  
                  
                )
                
              
)

server <- function(input, output, session) {
  winnerrr = "Prov"
  output$result <- renderPrint({
    if(input$submittion>0){
      isolate(winnerrr)
    }
    else{
      return("Ready to make recommendation")
    }
  })
  
}

shinyApp(ui = ui, server = server)