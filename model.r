library(dplyr)
library(ggplot2)

# Function to make a recommendation based on user input
recommend <- function(state, tier_pref, up_co, down_co, latency_co){
  
  # Importing Data
  data_raw <- read.csv("http://www.maths.usyd.edu.au/u/UG/JM/DATA1001/r/current/projects/2022data/broadband.csv")
  
  # Cleaning Data
  data = data_raw %>% filter(data_raw$rsp != "" & data_raw$rsp != "Other RSPs")

  # Consider user location and requested tier
  data = data %>% filter(data$state_or_territory == state & data$tier == tier_pref)

  # Separate the providers for calculations
  optus = filter(data, data$rsp == "Optus")
  tpg = filter(data, data$rsp == "TPG")
  dodo = filter(data, data$rsp == "Dodo & iPrimus")
  iinet = filter(data, data$rsp == "iiNet")
  exetel = filter(data, data$rsp == "Exetel")
  aussie = filter(data, data$rsp == "Aussie Broadband")
  vodafone = filter(data, data$rsp == "Vodafone")
  telstra = filter(data, data$rsp == "Telstra")
  superloop = filter(data, data$rsp == "Superloop")
    
  providers_list = list(optus, tpg, dodo, iinet, exetel, aussie, vodafone, telstra, superloop)
  Providers = c("Optus", "TPG", "Dodo & iPrimus", "iiNet", "Exetel", "Aussie Broadband", "Vodafone", "Telstra", "Superloop")
    
  # Calculate recommendation based on a combination of the rsp z score and user preference weighting
  z_up = 0
  z_down = 0
  z_lat = 0
  providers_score = c()
  current_score = 0
    
  for(i in providers_list){
    
    # If there are no data points from a provider in the given category they are excluded
    if(dim(i)[1] == 0){
      providers_score <- c(providers_score, 0)
      next
    }
    
    # We consider the Z scores for each category and multiply them by importance
    z_up = (mean(i$All.hour.trimmed.mean.upload.speed) - mean(data$All.hour.trimmed.mean.upload.speed)) / sd(data$All.hour.trimmed.mean.upload.speed)
    z_down = (mean(i$All.hour.trimmed.mean.download.speed) - mean(data$All.hour.trimmed.mean.download.speed)) / sd(data$All.hour.trimmed.mean.download.speed)
    # Note inverse score for latency as lower is better
    z_lat = -1 * ((mean(i$All.hour.trimmed.mean.latency) - mean(data$All.hour.trimmed.mean.latency)) / sd(data$All.hour.trimmed.mean.latency))
    
    current_score = (up_co * z_up) + (down_co * z_down) + (latency_co * z_lat)

    # Appending with each iteration
    providers_score <- c(providers_score, current_score)
    
  }
  
  # Converting to a data frame and printing a ranked bar graph
  df <- data.frame(Providers, providers_score)
  p <- ggplot(df, aes(x=reorder(Providers, -providers_score), y=providers_score, fill = Providers)) +
    geom_bar(stat="identity") +
    xlab("Providers") +
    ylab("Score")
  p

}





