library(dplyr)

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
    
  providers = list(optus, tpg, dodo, iinet, exetel, aussie, vodafone, telstra, superloop)
    
  # Calculate a winner based on user inputted importance ranking
  winner = 0
  best_score = -1000
  current_score = 0
  
  z_up = 0
  z_down = 0
  z_lat = 0
    
  for(i in providers){
    
    # If there are no data points from a provider in the given category they are excluded
    if(dim(i)[1] == 0){
      next
    }
    
    # We consider the Z scores for each category and multiply them by importance
    z_up = (mean(i$All.hour.trimmed.mean.upload.speed) - mean(data$All.hour.trimmed.mean.upload.speed)) / sd(data$All.hour.trimmed.mean.upload.speed)
    z_down = (mean(i$All.hour.trimmed.mean.download.speed) - mean(data$All.hour.trimmed.mean.download.speed)) / sd(data$All.hour.trimmed.mean.download.speed)
    z_lat = (mean(i$All.hour.trimmed.mean.latency) - mean(data$All.hour.trimmed.mean.latency)) / sd(data$All.hour.trimmed.mean.latency)
    
    print("up")
    print(z_up)
    print("down")
    print(z_down)
    print("lat")
    print(z_lat)
    
    current_score = (up_co * z_up) + (down_co * z_down) + (latency_co * z_lat)
      
    if(current_score >= best_score){
      best_score = current_score
      winner = i
    }
      
  }
  
  # Return the winning providers data such that information can be derived from it later
  return(winner)
  
}

# Test area

winner_data <- recommend("NSW", "250/25 Mbps", 1, 0, 0.6)

View(winner_data)





