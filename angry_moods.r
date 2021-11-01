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

#mosicplot?

#boxplots
#marginal
ggplot(data, aes(y = AE, fill = Sex)) + geom_boxplot() # <-
ggplot(data, aes(y = AE, fill = Athlete)) + geom_boxplot() # <-
#joint(???)
ggplot(data, aes(y = AE, fill = factorlab)) + geom_boxplot() # <-?


ggplot(data, aes(y = AO, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(y = AI, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(y = CO, fill = factorlab)) + geom_boxplot()
ggplot(data, aes(y = CI, fill = factorlab)) + geom_boxplot()


#histograms
ggplot(data, aes(x = AE, fill = factorlab)) + geom_histogram(bins = 20) #stacked
ggplot(data, aes(x = AO, fill = factorlab)) + geom_histogram(bins = 20) #stacked
ggplot(data, aes(x = AI, fill = factorlab)) + geom_histogram(bins = 20) #stacked
ggplot(data, aes(x = CO, fill = factorlab)) + geom_histogram(bins = 20) #stacked
ggplot(data, aes(x = CI, fill = factorlab)) + geom_histogram(bins = 20) #stacked

ggplot(data, aes(x = AE, fill = factorlab)) + 
    geom_histogram(alpha=0.5, position="identity", bins = 20)
ggplot(data, aes(x = AO, fill = factorlab)) + 
    geom_histogram(alpha=0.5, position="identity", bins = 20)
ggplot(data, aes(x = AI, fill = factorlab)) + 
    geom_histogram(alpha=0.5, position="identity", bins = 20)
ggplot(data, aes(x = CO, fill = factorlab)) + 
    geom_histogram(alpha=0.5, position="identity", bins = 20)
ggplot(data, aes(x = CI, fill = factorlab)) + 
    geom_histogram(alpha=0.5, position="identity", bins = 20)

#density
ggplot(data, aes(x = AE, fill = factorlab)) + 
    geom_density(alpha=0.5, position="identity") + ylim(0, 0.05)

ggplot(data, aes(x = AO, fill = factorlab)) + 
    geom_density(alpha=0.5, position="identity") + ylim(0, 0.15)
ggplot(data, aes(x = AI, fill = factorlab)) + 
    geom_density(alpha=0.5, position="identity") + ylim(0, 0.15)
ggplot(data, aes(x = CO, fill = factorlab)) + 
    geom_density(alpha=0.5, position="identity") + ylim(0, 0.15)
ggplot(data, aes(x = CI, fill = factorlab)) + 
    geom_density(alpha=0.5, position="identity") + ylim(0, 0.15)


#scatterplot
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


# for final goal
library(GGally)
pairs(data[, 3:6]) # <- include this

data$factorlab_sim <- data$factorlab
levels(data$factorlab_sim) <- c("F-A","F-N","M-A","M-N")
ggpairs(data, aes(colour=factorlab_sim, alpha = 0.4),
    columns = c("AE", "AO", "AI", "CO", "CI"))
