# ?read.csv
data <- read.csv("MH_survey_only_higher_index.csv",
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
data$sport_kind <- factor(data$sport_kind)
data$sport_level <- factor(data$sport_level)
data$team_inv <- factor(data$team_inv)


plot(MHC_SF_OVERALL ~ gender, data = data)
plot(MHC_SF_OVERALL ~ age_group, data = data)
plot(MHC_SF_OVERALL ~ country_lockdown, data = data)
plot(MHC_SF_OVERALL ~ marital, data = data)
plot(MHC_SF_OVERALL ~ smoking, data = data)
plot(MHC_SF_OVERALL ~ fivfruitveg, data = data)
plot(MHC_SF_OVERALL ~ hour_sleep, data = data)
plot(MHC_SF_OVERALL ~ week_soc_distancing, data = data)
plot(MHC_SF_OVERALL ~ shielded, data = data)
plot(MHC_SF_OVERALL ~ lockdown_bubble, data = data)
plot(MHC_SF_OVERALL ~ athlete, data = data)
plot(MHC_SF_OVERALL ~ jitter(AIMS_TOTAL), data = data)
abline(line(data$AIMS_TOTAL, data$MHC_SF_OVERALL))
plot(MHC_SF_OVERALL ~ sport_kind, data = data)
plot(MHC_SF_OVERALL ~ sport_level, data = data)
plot(MHC_SF_OVERALL ~ hour_weekly_playing, data = data)
plot(MHC_SF_OVERALL ~ hour_weekly_training, data = data)
plot(MHC_SF_OVERALL ~ hour_weekly_competing, data = data)
plot(MHC_SF_OVERALL ~ team_inv, data = data)
plot(MHC_SF_OVERALL ~ HADS_OVERALL, data = data)
plot(MHC_SF_OVERALL ~ RES_TOTAL, data = data)
plot(MHC_SF_OVERALL ~ LONE_TOTAL, data = data)
plot(MHC_SF_OVERALL ~ lockdown_bubble, data = data)

names(data)
data <- data[, -c(17:22)]
names(data)

model_full <- lm(MHC_SF_OVERALL ~ ., data = data)
summary(model_full)

step(model_full, direction = "backward")



model_step <- lm(MHC_SF_OVERALL ~ gender + age_group + country_lockdown + marital +
    smoking + fivfruitveg + shielded + week_soc_distancing +
    athlete + AIMS_TOTAL + HADS_OVERALL + RES_TOTAL + LONE_TOTAL, data = data)
summary(model_step)

par(mfrow = c(2, 2))
plot(model_step)




model_step2 <- lm(MHC_SF_OVERALL ~ gender + age_group + marital +
    smoking + fivfruitveg + shielded + week_soc_distancing +
    athlete + AIMS_TOTAL + HADS_OVERALL + RES_TOTAL + LONE_TOTAL, data = data)
summary(model_step2)



model_step3 <- lm(MHC_SF_OVERALL ~ gender + age_group + 
    fivfruitveg + shielded + hour_sleep +
    athlete + AIMS_TOTAL + RES_TOTAL + LONE_TOTAL + athlete, data = data) #HADS_OVERALL
summary(model_step3) #<<<<<<
par(mfrow = c(2, 2))
plot(model_step3)


par(mfrow = c(1, 1))
plot(MHC_SF_OVERALL ~ gender, data = data)
plot(MHC_SF_OVERALL ~ age_group, data = data)
plot(MHC_SF_OVERALL ~ marital, data = data)
plot(MHC_SF_OVERALL ~ smoking, data = data)
plot(MHC_SF_OVERALL ~ fivfruitveg, data = data)
plot(MHC_SF_OVERALL ~ hour_sleep, data = data)
plot(MHC_SF_OVERALL ~ week_soc_distancing, data = data)
plot(MHC_SF_OVERALL ~ shielded, data = data)
plot(MHC_SF_OVERALL ~ lockdown_bubble, data = data)
plot(MHC_SF_OVERALL ~ athlete, data = data)
plot(MHC_SF_OVERALL ~ jitter(AIMS_TOTAL), data = data)
plot(MHC_SF_OVERALL ~ HADS_OVERALL, data = data)
plot(MHC_SF_OVERALL ~ RES_TOTAL, data = data)
plot(MHC_SF_OVERALL ~ LONE_TOTAL, data = data)
plot(MHC_SF_OVERALL ~ lockdown_bubble, data = data)

names(data)
data_model_variable_only <- data[, c(1, 2, 6, 7, 8, 11, 12, 15, 16)]
names(data_model_variable_only)
pairs(data_model_variable_only)
head(data_model_variable_only)
summary(data_model_variable_only)
data_model_variable_only[,c(4, 7, 8, 9)]


cor(data_model_variable_only[, c(4, 7, 8, 9)], use="complete.obs")

# too small
model_step4 <- lm(MHC_SF_OVERALL ~ gender + age_group + 
    fivfruitveg + shielded + 
    athlete + AIMS_TOTAL + HADS_OVERALL + RES_TOTAL + LONE_TOTAL, data = data)
summary(model_step4)