# Selectivity indices

#setwd("D:/HTS USB/Honours/LOPC")

#install.packages("selectapref")

library(selectapref)
library(plyr)
library(tidyverse)


# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Fish Gut size data.csv")

fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Value"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size", "Metric"))
head(combined_data)

combined_data <- combined_data %>% filter(Size != 255 & Size != 285) %>% drop_na()
head(combined_data)


# Selectivity index time
# Abundance First

combined_Abund <- combined_data %>% filter(Metric == "Abundance")
# combined_Abund$Abund_index <- ivlev(combined_Abund$Water_Value, combined_Abund$Guts_Value)
# plot(combined_Abund$Abund_index)
# 
# plot(combined_Abund$Water_Value ~ combined_Abund$Size)
# head(combined_Abund)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abund)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abund$Size[i]), combined_Abund$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test code (schilling et al 2019)
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 69, replace = TRUE) # 69 is average number of prey in a zooplanktivore
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
### write_csv(simulation_results, "Simulation Results average abundance.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write.csv(Expected_proportions, "Average expected proportions in diet.csv", row.names = F)

pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_abund <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_abund)
fish_guts_abund <- rename(fish_guts_abund, c("Value" = "Guts_Value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_abund)
head(expected_observed_data)


# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na() %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Average_abundance <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Average", Metric = "Abundance")
head(Cohens_D_Average_abundance)

Average_Abundance <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12))

Average_Abundance

#ggsave("2020 plots/Prey Selectivity.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

### Repeat for Biomass ####

# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Fish Gut size data.csv")

fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Value"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size", "Metric"))
head(combined_data)

combined_data <- combined_data %>% filter(Size != 255 & Size != 285) %>% drop_na()
head(combined_data)


# Selectivity index time
# Abundance First

combined_Biomass <- combined_data %>% filter(Metric == "Biomass")
combined_Biomass$Biomass_index <- ivlev(combined_Biomass$Water_Value, combined_Biomass$Guts_Value)
plot(combined_Biomass$Biomass_index)

plot(combined_Biomass$Water_Value ~ combined_Biomass$Size)
head(combined_Biomass)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Biomass)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Biomass$Size[i]), combined_Biomass$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test (Schilling et al 2019)
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 69, replace = TRUE) # 69 is average number of prey in a zooplanktivore
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results average biomass.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Average expected proportions in diet biomass.csv")

pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Biomass <- fish_guts %>% filter(Metric == "Abundance") # plot looks better and more accurate if Abundance used here
#fish_guts_Biomass$Metric <- "Biomass"

head(fish_guts_Biomass)
fish_guts_Biomass <- rename(fish_guts_Biomass, c("Value" = "Guts_Value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))


expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Biomass)
head(expected_observed_data)

Average_Biomass <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet (%)") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12))

Average_Biomass

#ggsave("2020 plots/Prey Selectivity biomass.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")


# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na() %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Average_Biomass <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Average", Metric = "Biomass")
head(Cohens_D_Average_Biomass)


### Individual species ####
# Mado first
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Mado" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Abundance")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 145, replace = TRUE) # 145 is average number of prey in a mado
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results mado abundance.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Mado expected proportions in diet abundance.csv")


pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Mado_Abund <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200))

Mado_Abund

#ggsave("2020 plots/Mado Prey Selectivity Abundance.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Mado_Abundance <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Mado", Metric = "Abundance")
head(Cohens_D_Mado_Abundance)


### Mado Biomass ####
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Mado" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Biomass")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 145, replace = TRUE) # 145 is average number of prey in a mado
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results mado biomass.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Mado expected proportions in diet biomass.csv")


pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))


expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Mado_Biomass <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200))

Mado_Biomass

#ggsave("2020 plots/Mado Prey Selectivity Biomass.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Mado_Biomass <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Mado", Metric = "Biomass")
head(Cohens_D_Mado_Biomass)



### Sweep ####
# Sweep first abundance
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Sweep" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Abundance")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 46, replace = TRUE) # 46 is average number of prey in a Sweep
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results Sweep abundance.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Sweep expected proportions in diet abundance.csv")



pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Sweep_Abund <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200))

Sweep_Abund

#ggsave("2020 plots/Sweep Prey Selectivity Abundance.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Sweep_Abundance <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Sweep", Metric = "Abundance")
head(Cohens_D_Sweep_Abundance)


### Sweep Biomass ####
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Sweep" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Biomass")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 46, replace = TRUE) # 46 is average number of prey in a Sweep
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results Sweep biomass.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Sweep expected proportions in diet biomass.csv")



pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Sweep_Biomass <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200))

Sweep_Biomass

#ggsave("2020 plots/Sweep Prey Selectivity Biomass.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")


# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Sweep_Biomass <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Sweep", Metric = "Biomass")
head(Cohens_D_Sweep_Biomass)


### Yakka ####
# Yakka first abundance
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Yakka" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Abundance")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 27, replace = TRUE) # 27 is average number of prey in a Yakka
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results Yakka abundance.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
#write_csv(Expected_proportions, "Yakka expected proportions in diet abundance.csv")


pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Yakka_Abund <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200)) + theme(legend.position = "bottom")

Yakka_Abund

#ggsave("2020 plots/Yakka Prey Selectivity Abundance.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")


# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Yakka_Abundance <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Yakka", Metric = "Abundance")
head(Cohens_D_Yakka_Abundance)



### Yakka Biomass ####
# What's in the water
available_data = read.csv("Data/Zooplankton in water available by size.csv", header = T)

abund_data <- available_data %>% filter(Metric == "Abundance (%)" | Metric == "Biomass (%)")
abund_data$Metric <- revalue(abund_data$Metric, c("Abundance (%)"="Abundance", "Biomass (%)"="Biomass"))
abund_data$Size <- as.factor(abund_data$Size)
levels(abund_data$Size)
abund_data <- rename(abund_data, c("Water_Value"="mean_Value"))
head(abund_data)

# What's in the guts

fish_guts <- read.csv("Data/Species Prey size summary.csv")
fish_guts <- fish_guts %>% filter(Species == "Yakka" & Metric == "Abundance")
abund_data <- abund_data %>% filter(Metric == "Biomass")
#fish_guts$Metric <- revalue(fish_guts$Metric, c("A) Abundance"="Abundance", "B) Biomass"="Biomass"))
fish_guts$Size <- as.factor(fish_guts$Size)
levels(fish_guts$Size)
fish_guts <- rename(fish_guts, c("Guts_Value"="Mean_percent", "SE_value"="SE"))
head(fish_guts)

# Combine
combined_data <- full_join(fish_guts, abund_data, by=c("Size"))
head(combined_data)

combined_data <- combined_data  %>% drop_na() #%>% filter(Size != 255 & Size != 285)
head(combined_data)


# Selectivity index time
# Abundance First
# 
combined_Abundance <- combined_data 
# combined_Abundance$Abundance_index <- ivlev(combined_Abundance$Water_Value, combined_Abundance$Guts_Value)
# plot(combined_Abundance$Abundance_index)
# 
# plot(combined_Abundance$Water_Value ~ combined_Abundance$Size)
# head(combined_Abundance)

# simulated zooplankton population
dummy_data <- data.frame()
for (i in 1:nrow(combined_Abundance)){
  dd_dat <- data.frame("Size" = c(rep(as.character(combined_Abundance$Size[i]), combined_Abundance$Water_Value[i]*100000)))
  dummy_data <- bind_rows(dummy_data, dd_dat)
}

hist(as.numeric(dummy_data$Size))
table(dummy_data$Size)
# looks Good, proportions match the original ones now need to simulate the feeding event

# From Sex ratio test
set.seed(1)
simulation_results <- data.frame()
for (i in 1:2000) {
  samp <- sample_n(dummy_data, 27, replace = TRUE) # 27 is average number of prey in a Yakka
  d_dat <- samp %>%
    group_by(Size) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(freq = n / sum(n)*100)
  d_dat$Simulation <- i
  simulation_results <- bind_rows(simulation_results, d_dat)
}
head(simulation_results)
#write_csv(simulation_results, "Simulation Results Yakka biomass.csv")

# Expected if eating randomly from water
Expected_proportions <- simulation_results %>% dplyr::group_by(Size) %>% 
  dplyr::summarise(Mean_Expected = mean(freq), SD_expected = sd(freq), 
                   n = n(), SE_Expected = SD_expected/sqrt(n))

Expected_proportions$Size <- as.numeric(Expected_proportions$Size)
write_csv(Expected_proportions, "Yakka expected proportions in diet biomass.csv")

pp <- ggplot(Expected_proportions, aes(x = Size, y = Mean_Expected)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_Expected-SD_expected, ymax = Mean_Expected+SD_expected)) + theme_classic() +
  ylab("Percentage in Diet")
pp

Expected_proportions$Data_Type <- "Expected"
fish_guts$Data_Type <- "Observed"
fish_guts$Size <- as.numeric(as.character(fish_guts$Size))
fish_guts_Abundance <- fish_guts %>% filter(Metric == "Abundance")

head(fish_guts_Abundance)
fish_guts_Abundance <- rename(fish_guts_Abundance, c("Value" = "Guts_Value", "SE" = "SE_value"))


head(Expected_proportions)
Expected_proportions <- rename(Expected_proportions, c("Value" ="Mean_Expected","SD" ="SD_expected", "SE"= "SE_Expected"))




expected_observed_data <- bind_rows(Expected_proportions, fish_guts_Abundance)
head(expected_observed_data)

Yakka_Biomass <- ggplot(expected_observed_data, aes(x = Size, y = Value, lty = Data_Type)) + geom_line()+
  geom_errorbar(aes(ymin=Value-1.96*SE, ymax = Value+1.96*SE)) + theme_classic() +
  ylab("Percentage in Diet") + xlab("Prey Size (ESD, µm)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  xlim(c(200,1200))

Yakka_Biomass

#ggsave("2020 plots/Yakka Prey Selectivity Biomass.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")


# Calculate Cohen's D effect size
expected_observed_data$Metric <- NULL
expected_observed_data$Species <- NULL
expected_observed_data_wide <- pivot_wider(expected_observed_data, names_from = Data_Type, values_from = c(Value, SD, n, SE))
head(expected_observed_data_wide)
expected_observed_data_wide <- expected_observed_data_wide %>% drop_na(Value_Expected) %>% rowwise %>% mutate(SD_Pooled = mean(c(SD_Expected, SD_Observed)))
head(expected_observed_data_wide)
expected_observed_data_wide$Cohens_D <- (expected_observed_data_wide$Value_Observed - expected_observed_data_wide$Value_Expected)/
  expected_observed_data_wide$SD_Pooled
head(expected_observed_data_wide)
plot(expected_observed_data_wide$Size, expected_observed_data_wide$Cohens_D, type = "l")

Cohens_D_Yakka_Biomass <- expected_observed_data_wide %>% select(Size, Cohens_D) %>% mutate(Species = "Yakka", Metric = "Biomass")
head(Cohens_D_Yakka_Biomass)

### Combine Abundance Plots ####
library(patchwork)


### Figure 3
Average_Abundance2 <- Average_Abundance + xlab("") + ylab("Mean % (±95% CI)") + scale_linetype_manual(values = c(2,1), name = "Data Type") + 
  geom_text(x = 800, y = 15, label = "3 Species Average", stat = "identity", inherit.aes = FALSE, hjust = 0)
Mado_Abund2 <- Mado_Abund + xlab("")+ ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 15, label = "Atypichthys strigatus", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")
Sweep_Abund2 <- Sweep_Abund + xlab("")+ ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 15, label = "Scorpis lineolata", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")
Yakka_Abund2 <- Yakka_Abund+ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 15, label = "Trachurus novaezelandiae", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")

Full_plot <- Average_Abundance2 + Mado_Abund2 + Sweep_Abund2 +Yakka_Abund2 +
  plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom')
Full_plot

ggsave("Output/Combined Abundance Selectivity.png", dpi = 600, width = 20, height = 21, units = "cm")
ggsave("Output/Combined Abundance Selectivity.pdf", dpi = 600, width = 20, height = 21, units = "cm")


### Figure 4
Average_Biomass2 <- Average_Biomass + xlab("") + ylab("Mean % (±95% CI)") + scale_linetype_manual(values = c(2,1), name = "Data Type") + 
  geom_text(x = 800, y = 15, label = "3 Species Average", stat = "identity", inherit.aes = FALSE, hjust = 0)
Mado_Biomass2 <- Mado_Biomass + xlab("")+ ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 12, label = "Atypichthys strigatus", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")
Sweep_Biomass2 <- Sweep_Biomass + xlab("")+ ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 15, label = "Scorpis lineolata", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")
Yakka_Biomass2 <- Yakka_Biomass+ylab("Mean % (±95% CI)")+ scale_linetype_manual(values = c(2,1), name = "Data Type")+ 
  geom_text(x = 800, y = 15, label = "Trachurus novaezelandiae", stat = "identity", inherit.aes = FALSE, hjust = 0, fontface = "italic")

Full_plot_Biomass <- Average_Biomass2 + Mado_Biomass2 + Sweep_Biomass2 +Yakka_Biomass2 +
  plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'bottom')
Full_plot_Biomass

ggsave("Output/Combined Biomass Selectivity.png", dpi = 600, width = 20, height = 21, units = "cm")
ggsave("Output/Combined Biomass Selectivity.pdf", dpi = 600, width = 20, height = 21, units = "cm")


# ### Cohen's D combined plot
# 
# Cohens_list <- list(Cohens_D_Average_abundance, Cohens_D_Average_Biomass, Cohens_D_Mado_Abundance, Cohens_D_Mado_Biomass,
#                     Cohens_D_Sweep_Abundance, Cohens_D_Sweep_Biomass, Cohens_D_Yakka_Abundance, Cohens_D_Yakka_Biomass)
# 
# Cohens_D_Full <- bind_rows(Cohens_list)
# 
# Cohens_plot <- ggplot(Cohens_D_Full, aes(x = Size, y = Cohens_D, col = Species)) + geom_line()+ geom_point()+
#   facet_wrap(~Metric, ncol = 1, scales = "free_y") + theme_classic() + xlim(c(300, 1250)) + 
#   geom_hline(yintercept = 0, col = "red")
# Cohens_plot  
  
