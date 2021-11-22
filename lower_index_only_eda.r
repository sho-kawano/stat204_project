# ?read.csv
data <- read.csv("MH_survey_only_lower_index.csv",
    na.strings = "NA")
names(data)
names(data)[1] <- "gender"
names(data)


data$gender <- factor(data$gender)
data$age_group <- factor(data$age_group)
data$country_lockdown <- factor(data$country_lockdown)
data$marital <- factor(data$marital)
data$smoking <- factor(data$smoking)
data$fivfruitveg <- factor(data$fivfruitveg)
data$shielded <- factor(data$shielded)
data$week_soc_distancing <- factor(data$week_soc_distancing)
data$athlete <- factor(data$athlete)
