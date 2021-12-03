# 2020 test figures

#setwd("E:/HTS USB/Honours/LOPC")

library(tidyverse)

mydata <- read_csv("Data/Sydney Harbour Zooplankton biomass size data 2020.csv")

dat <- mydata %>% pivot_longer(cols = c(6:95),
                               names_to = "Size",
                               values_to = "Biomass")
head(dat)

dat2 <- dat %>% group_by(Site, Tide, Size) %>%
  summarise(Biomass_Mean = mean(Biomass), Biomass_SD = sd(Biomass), n = n(), SE = Biomass_SD/sqrt(n))
head(dat2)

dat2$Size <- as.numeric(dat2$Size)
dat2$Site <- as.factor(as.character(dat2$Site))

p1 <- ggplot(dat2, aes(x = Size, y = Biomass_Mean, colour = Site, lty= Tide)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Biomass_Mean-SE, ymax = Biomass_Mean+SE))
p1


dat3 <- dat %>% group_by(Site, Size) %>%
  summarise(Biomass_Mean = mean(Biomass), Biomass_SD = sd(Biomass), n = n(), SE = Biomass_SD/sqrt(n))
head(dat3)

dat3$Size <- as.numeric(dat3$Size)
dat3$Site <- as.factor(as.character(dat3$Site))

p2 <- ggplot(dat3, aes(x = Size, y = Biomass_Mean, lty = Site)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Biomass_Mean-SE, ymax = Biomass_Mean+SE)) +
  xlim(c(300,1000)) + xlab("Zooplankton Size (ESD, ?m)") + ylab("Biomass (mg m3)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10))
p2

Biomass_Size <- dat3
Biomass_Size$Metric = "Biomass (mg m3)"
Biomass_Size <- Biomass_Size %>% rename(Value = Biomass_Mean, SD = Biomass_SD)
head(Biomass_Size)

#ggsave("2020 plots/Biomass by size.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

### Now look at percentage biomass

mydata <- read_csv("Data/Sydney Harbour Zooplankton biomass size percentage data 2020.csv")

dat <- mydata %>% pivot_longer(cols = c(6:95),
                               names_to = "Size",
                               values_to = "Percent")
head(dat)

dat2 <- dat %>% group_by(Site, Tide, Size) %>%
  summarise(Biomass_Mean = mean(Percent), Biomass_SD = sd(Percent), n = n(), SE = Biomass_SD/sqrt(n))
head(dat2)

dat2$Size <- as.numeric(dat2$Size)
dat2$Site <- as.factor(as.character(dat2$Site))

p1 <- ggplot(dat2, aes(x = Size, y = Biomass_Mean, colour = Site, lty= Tide)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Biomass_Mean-SE, ymax = Biomass_Mean+SE))
p1


dat3 <- dat %>% group_by(Site, Size) %>%
  summarise(Biomass_Mean = mean(Percent), Biomass_SD = sd(Percent), n = n(), SE = Biomass_SD/sqrt(n))
head(dat3)

dat3$Size <- as.numeric(dat3$Size)
dat3$Site <- as.factor(as.character(dat3$Site))

p2 <- ggplot(dat3, aes(x = Size, y = Biomass_Mean, lty = Site)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Biomass_Mean-SE, ymax = Biomass_Mean+SE)) +
  xlim(c(300,1000)) + xlab("Zooplankton Size (?m ESD)") + ylab("Biomass (%)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10))
p2

#ggsave("2020 plots/Biomass percentage by size.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

Biomass_Percentage <- dat3
Biomass_Percentage$Metric = "Biomass (%)"
Biomass_Percentage <- Biomass_Percentage %>% rename(Value = Biomass_Mean, SD = Biomass_SD)
head(Biomass_Percentage)

### By Abundance now

mydata <- read_csv("Data/Sydney Harbour Zooplankton abundance size data 2020.csv")

dat <- mydata %>% pivot_longer(cols = c(5:94),
                               names_to = "Size",
                               values_to = "Abundance")
head(dat)

dat3 <- dat %>% group_by(Site, Size) %>%
  summarise(Abundance_Mean = mean(Abundance), Abundance_SD = sd(Abundance), n = n(), SE = Abundance_SD/sqrt(n))
head(dat3)

dat3$Size <- as.numeric(dat3$Size)
dat3$Site <- as.factor(as.character(dat3$Site))

p2 <- ggplot(dat3, aes(x = Size, y = Abundance_Mean, lty = Site)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Abundance_Mean-SE, ymax = Abundance_Mean+SE)) +
  xlim(c(300,1000)) + xlab("Zooplankton Size (ESD, ?m)") + ylab("Abundance") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10))
p2

#ggsave("2020 plots/Abundance by size.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

Abund_Size <- dat3
Abund_Size$Metric = "Abundance (# m3)"
Abund_Size <- Abund_Size %>% rename(Value = Abundance_Mean, SD = Abundance_SD)
head(Abund_Size)


### Now look at percentage biomass

mydata <- read_csv("Data/Sydney Harbour Zooplankton abundance size percentage data 2020.csv")

dat <- mydata %>% pivot_longer(cols = c(5:94),
                               names_to = "Size",
                               values_to = "Percent")
head(dat)

dat3 <- dat %>% group_by(Site, Size) %>%
  summarise(Abundance_Mean = mean(Percent), Abundance_SD = sd(Percent), n = n(), SE = Abundance_SD/sqrt(n))
head(dat3)

dat3$Size <- as.numeric(dat3$Size)
dat3$Site <- as.factor(as.character(dat3$Site))

p2 <- ggplot(dat3, aes(x = Size, y = Abundance_Mean, lty = Site)) + geom_line() +
  theme_classic() + geom_errorbar(aes(ymin=Abundance_Mean-SE, ymax = Abundance_Mean+SE)) +
  xlim(c(300,1000)) + xlab("Zooplankton Size (ESD, ?m)") + ylab("Abundance (%)") +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10))
p2

#ggsave("2020 plots/Abundance percentage by size.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")

Abund_Percent <- dat3
Abund_Percent$Metric = "Abundance (%)"
Abund_Percent <- Abund_Percent %>% rename(Value = Abundance_Mean, SD = Abundance_SD)
head(Abund_Percent)

## Make 4 panel plot ### THIS IS FIGURE S1
full_dat <- rbind(Biomass_Size, Biomass_Percentage, Abund_Size, Abund_Percent)
head(full_dat)
full_dat$Metric <- as.factor(full_dat$Metric)
full_dat$Metric <- factor(full_dat$Metric, levels = c("Abundance (# m3)", "Abundance (%)", "Biomass (mg m3)", "Biomass (%)"))
levels(full_dat$Metric)

p3 <- ggplot(full_dat, aes(x = Size, y = Value, lty = Site)) + geom_line() + 
  facet_wrap(~Metric, scales = "free_y") +
  theme_classic() + geom_errorbar(aes(ymin=Value-SE, ymax = Value+SE)) +
  xlim(c(300,1250)) + xlab("Zooplankton Size (ESD, µm)") + ylab(NULL) +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12))
p3

#ggsave("2020 plots/Zooplankton 2x2.png", dpi = 600, width = 21.8, height = 14.8, units = "cm")
#ggsave("2020 plots/Zooplankton 2x2.pdf", dpi = 600, width = 21.8, height = 14.8, units = "cm")


# Average Zooplankton ### THIS IS FIGURE 1
full_dat_avg <- full_dat %>% group_by(Size, Metric) %>% summarise(mean_Value = mean(Value), SD_value = sd(SD), n = n(), SE_value = SD_value/sqrt(n))
full_dat_avg

p4 <- ggplot(full_dat_avg, aes(x = Size, y = mean_Value)) + geom_line() + 
  facet_wrap(~Metric, scales = "free_y") +
  theme_classic() + geom_errorbar(aes(ymin=mean_Value-SE_value, ymax = mean_Value+SE_value)) +
  xlim(c(300,1250)) + xlab("Zooplankton Size (ESD, µm)") + ylab(NULL) +
  theme(axis.title = element_text(face = "bold", size = 12, colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        legend.title = element_text(face = "bold", size = 12, colour = "black"),
        legend.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12))
p4

#write.csv(full_dat_avg, "Zooplankton in water available by size.csv", row.names=F)

#ggsave("2020 plots/Zooplankton avg 2x2.pdf", dpi = 600, width = 21.8, height = 14.8, units = "cm")
