# calculate p-values

# try something like sum(v > 2 & v < 5) after grouping by size and somehow defining the intervals

#setwd("E:/HTS USB/Honours/LOPC")
library(tidyverse)

# 
# sim_results <- read.csv("Simulation Results average abundance.csv")
# head(sim_results)
# 
# observed_results <- read.csv("Fish Gut size data.csv")
# head(observed_results)
# 
# metrics <- c("A) Abundance", "B) Biomass")
# 
# for (d in metrics){ # this loop repeats for abundance and biomass
# observed_results2 <- subset(observed_results, Metric == d)
# 
# observed_results2$UpperCI <- observed_results2$Value + 1.96 * observed_results2$SE
# observed_results2$LowerCI <- observed_results2$Value - 1.96 * observed_results2$SE
# #head(observed_results2)
# 
# 
# 
# sim_results$Within_95CI <- -9999 # placeholder value
# for (i in 1:nrow(sim_results)){
#   
#   if (sim_results$freq[i] <=  observed_results2$UpperCI[observed_results2$Size == sim_results$Size[i]] &
#       sim_results$freq[i] >=  observed_results2$LowerCI[observed_results2$Size == sim_results$Size[i]]) {
#     sim_results$Within_95CI[i] = 1
#   } else {
#     sim_results$Within_95CI[i] = 0
#    }
#  }
# 
# 
# results_summary <- sim_results %>% group_by(Size) %>% summarise(total_within_CI = sum(Within_95CI), P_value = total_within_CI/2000) # 2000 was the number of draws I made in the last step
# head(results_summary)
# results_summary
# write.csv(results_summary, paste0("Average species P_values_",d,".csv"))
# 
# }


# Now for individual species


species <- c("mado", "Yakka", "Sweep")
species_names <- c("Mado", "Yakka", "Sweep")
metric <- c("abundance", "biomass")

paste0("Simulation Results ", species[1], " ", metric[1],".csv")

for (s in 1:length(species)) {
  for (m in metric) {
    file <- paste0("Output/Simulation Results ", species[s], " ", m,".csv")
    sim_results <-
      read.csv(file) 
    
    observed_results <- read.csv("Data/Species Prey size summary.csv")
    observed_results2 <- subset(observed_results, Metric == "Abundance" & Species == species_names[s])

    
    observed_results2$UpperCI <-
      observed_results2$Mean_percent + 1.96 * observed_results2$SE
    observed_results2$LowerCI <-
      observed_results2$Mean_percent - 1.96 * observed_results2$SE
    #head(observed_results2)
    
    
    
    sim_results$Within_95CI <- -9999 # placeholder value
    for (i in 1:nrow(sim_results)) {
      if (sim_results$freq[i] <=  observed_results2$UpperCI[observed_results2$Size == sim_results$Size[i]] &
          sim_results$freq[i] >=  observed_results2$LowerCI[observed_results2$Size == sim_results$Size[i]]) {
        sim_results$Within_95CI[i] = 1
      } else {
        sim_results$Within_95CI[i] = 0
      }
    }
    
    
    results_summary <-
      sim_results %>% group_by(Size) %>% summarise(total_within_CI = sum(Within_95CI),
                                                   P_value = total_within_CI / 2000) # 2000 was the number of draws I made in the last step
    head(results_summary)
    results_summary
    write.csv(results_summary,
              paste0("Output/",species_names[s], "P_values_", m, ".csv"))
    
  }
}
