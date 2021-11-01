library(tidyverse)
data = read_csv("angry_moods.csv") %>% 
          mutate(Sex = ifelse(Sex==2, 0, 1)) %>% 
          mutate(Athlete = ifelse(Athlete==2, 0, 1))
#1: male 2/0:female
#1: athletes, 2/0: non-athletes
data

data$Sex <- factor(data$Sex, labels=c("female", "male"))
data$Athlete <- factor(data$Athlete, labels=c("athletes","non-athletes"))
names(data)[3:7] <- c("AO", "AI", "CO", "CI", "AE")

data$factorlab <- factor(paste(as.character(data$Sex), as.character(data$Athlete), sep="_"))
data

# data$Gender<-factor(ifelse(data$Gender==1, "male", "female"))
# data$Athlete<-factor(ifelse(data$Athlete==1, "athletes", "non-athletes"))

#boxplots
ggplot(data, aes(x = AE, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(x = AO, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(x = AI, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(x = CO, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(x = CI, fill = factorlab)) + geom_boxplot()

ggplot(data, aes(x = AE, fill = factorlab)) + geom_histogram(bin = 20)
ggplot(data, aes(x = AO, fill = factorlab)) + geom_histogram(bin = 20)
ggplot(data, aes(x = AI, fill = factorlab)) + geom_histogram(bin = 20)
ggplot(data, aes(x = CO, fill = factorlab)) + geom_histogram(bin = 20)
ggplot(data, aes(x = CI, fill = factorlab)) + geom_histogram(bin = 20)

ggplot(data, aes(x = AE, color = factorlab)) + geom_histogram(bin = 20, , alpha=0.5, position="identity")
ggplot(data, aes(x = AO, color = factorlab)) + geom_histogram(bin = 20, , alpha=0.5, position="identity")
ggplot(data, aes(x = AI, color = factorlab)) + geom_histogram(bin = 20, , alpha=0.5, position="identity")
ggplot(data, aes(x = CO, color = factorlab)) + geom_histogram(bin = 20, , alpha=0.5, position="identity")
ggplot(data, aes(x = CI, color = factorlab)) + geom_histogram(bin = 20, , alpha=0.5, position="identity")


ggplot(angry, aes(x = AO, after_stat(density))) +
    geom_histogram(bins = 20, col=factorlab) +
    geom_density(fill = "blue", alpha = 0.2)

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
