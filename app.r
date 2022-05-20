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
                             p(class = "text_normal", "Third party company that specialises in making flight plan recommendations to travellers.
                               They wish to expand into the internet provider recommendation space."),
                             p(class = "top_1", "Recommendation"),
                             p(class = "text_normal", "The provided tool allows for a visually appealing ranking of the providers in the customers area.
                               The tool can be directly provided for customer use, or the information it provides may serve as the foundation of a recommendation.
                               The rankings are based on three factors; upload-speed, download-speed and latency.
                               It also allows for the user to input an importance scalar for each of these that will have an impact on the calculation and be reflected in the ranking."),
                             width = 3
                             
                           ),
                           
                           mainPanel(
                             p(class = "top_2", "Introduction Video:"),
                             url <- a("https://www.youtube.com/watch?v=mmLyZ0xHlOM", href="https://www.youtube.com/watch?v=mmLyZ0xHlOM"),
                             tabsetPanel(type = "tab",
                                         tabPanel(HTML('<iframe width="900" height="599" src="https://www.youtube.com/embed/mmLyZ0xHlOM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
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
                             p(class = "text_normal", "The ranking calculations are based on the various providers performance in three categories; upload-speed, download-speed and Latency.
                             To apply an equal importance/weighting to each of these categories we take the z-scores.
                             In simple terms this is a way of quantifying how much better or worse each provider performs than the mean in a standardised manner.
                             This process will leave providers that are below average with a negative score. This is displayed in the plot on the righ hand side of this page. To avoid this we plug the z-scores into the Sigmoid function.
                             This is a simple mathematical conversion that leaves all the scores in the range of 0 to 1.
                             A poor/negative z-score will evaluate to a low final score and a high/positive z-score will evaluate to final score that approaches 1."),
                             
                             p(class = "top_1", "Limitations"),
                             p(class = "text_normal", "As time passes the reliability and validity of the calculations will decrease. This is due to the calculations being based on data from December 2021.
                               Some sub categories have limited data points available and thus the ranking will be less robust and meaningful.
                               The justification for the validity of taking the z-scores is that the data is normally distributed and we know the standard deviation of each of the subcategories.
                               However, in some subcategories there are less than 30 observations and the distribution when there are minimal observations will tend to be less normal.
                               Thus the results produced in these subcategories should be interpreted with caution.
                               Note that the speed tiers above 250/25 Mpbs have been removed as there was not enough data to justify any potential findings."),
                             width = 4
                           ),
                           
                           mainPanel(
                             p(class = "top_2", "Plot without using the Sigmoid function. State: All, Tier: All, Level of concern: 1"),
                             plotOutput("evidence"), width = 8
                           )
                ),
                
                tabPanel("Acknowledgments",
                         sidebarPanel(
                           p(class = "top_1", "Acknowledgments"),
                           
                           p(class = "text_normal", "RStudio, (2021), 'shinyapps.io user guide', URL:"),
                           url <- a("https://docs.rstudio.com/shinyapps.io/", href="https://docs.rstudio.com/shinyapps.io/"),
                           
                           p(class = "text_normal", "Tutorialspoint, (2022), 'R - Functions', URL:"),
                           url <- a("https://www.tutorialspoint.com/r/r_functions.htm", href="https://www.tutorialspoint.com/r/r_functions.htm"),
                           
                           p(class = "text_normal", "GGplot, (N/A), 'Modify axis, legend, and plot labels', URL:"),
                           url <- a("https://ggplot2.tidyverse.org/reference/labs.html", href="https://ggplot2.tidyverse.org/reference/labs.html"),
                           
                           p(class = "top_1", "Data Source"),
                           
                           p(class = "text_normal", "Australian Competition and Consumer Commission, (2021). 'Measuring Broadband Australia Report 15 Dataset Release'. URL:"),
                           url <- a("https://data.gov.au/data/dataset/measuring-broadband-australia-report-15-dataset-release", href="https://data.gov.au/data/dataset/measuring-broadband-australia-report-15-dataset-release"),
                           
                           width = 5
                         )
                  
                )
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
