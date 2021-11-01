library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)



angry <- read_csv("angry_moods.csv")
angry$Gender <- factor(angry$Gender)
angry$Sports <- factor(angry$Sports)
#1: male 2:female
#1: athletes, 2: non-athletes
names(angry)[3:7] <- c("AO", "AI", "CO", "CI", "AE")
angry
angry$Gender<-factor(ifelse(angry$Gender==1, "male", "female"))
angry$Sports<-factor(ifelse(angry$Sports==1, "athletes", "non-athletes"))

ggplot(angry, aes(x = AO, after_stat(density))) +
    geom_histogram(bins = 20) +
    geom_density(fill = "blue", alpha = 0.2) +
    facet_wrap(~Gender + Sports, scales = "fixed")

ggplot(angry, aes(x = AI, after_stat(density))) +
    geom_histogram(bins = 20) +
    geom_density(fill = "blue", alpha = 0.2)

ggplot(angry, aes(x = CO, after_stat(density))) +
    geom_histogram(bins = 20) +
    geom_density(fill = "blue", alpha = 0.2)

ggplot(angry, aes(x = CI, after_stat(density))) +
    geom_histogram(bins = 20) +
    geom_density(fill = "blue", alpha = 0.2)

ggplot(angry, aes(x = AE, after_stat(density))) +
    geom_histogram(bins = 15) +
    geom_density(fill = "blue", alpha = 0.2) + 
    facet_wrap(~Gender + Sports, scales = "fixed")


boxplot(AE ~ Gender + Sports, data = angry)
boxplot(AE ~ Gender + Sports, data = angry)

length(which(angry$Gender == "female" & angry$Sports == "athletes"))
length(which(angry$Gender == "male" & angry$Sports == "athletes"))
length(which(angry$Gender == "female" & angry$Sports == "non-athletes"))
length(which(angry$Gender == "male" & angry$Sports == "non-athletes"))
length(which(angry$Gender == "male"))
length(which(angry$Gender == "female"))
length(which(angry$Sports == "athletes"))
length(which(angry$Sports == "non-athletes"))
pairs(angry[, 3:6])
