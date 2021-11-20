# ?read.csv
new_athlete <- read.csv("reduced_athlete_nonathlete_MH_survey.csv",
    na.strings = "NA")
head(new_athlete)
summary(new_athlete)

new_athlete$gender <- factor(new_athlete$gender)
new_athlete$age_group <- factor(new_athlete$age_group)
new_athlete$country_during_lockdown <- factor(new_athlete$country_during_lockdown)
new_athlete$five_fruit_and_veg <- factor(new_athlete$five_fruit_and_veg)
new_athlete$shielded <- factor(new_athlete$shielded)
new_athlete$athlete <- factor(new_athlete$athlete)
new_athlete$sport_level <- factor(new_athlete$sport_level)
new_athlete$idv_team_athlete <- factor(new_athlete$idv_team_athlete)


summary(new_athlete)


plot(MHC_SF_overall ~ gender, data = new_athlete)
plot(MHC_SF_overall ~ athlete, data = new_athlete)
plot(MHC_SF_overall ~ age_group, data = new_athlete)
plot(MHC_SF_overall ~ marital, data = new_athlete)
plot(MHC_SF_overall ~ smoking, data = new_athlete)
plot(MHC_SF_overall ~ five_fruit_and_veg, data = new_athlete)
plot(MHC_SF_overall ~ hours_sleep, data = new_athlete)
plot(MHC_SF_overall ~ shielded, data = new_athlete)
plot(MHC_SF_overall ~ weeks_social_distancing, data = new_athlete)

summary(lm(MHC_SF_overall ~ athlete + gender, data = new_athlete))
summary(lm(MHC_SF_overall ~ athlete + gender + age_group + marital + smoking + five_fruit_and_veg + hours_sleep + shielded + weeks_social_distancing, data = new_athlete))

