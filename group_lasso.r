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

summary(data)


# group lasso from full model
library(gglasso)
?gglasso


model_full <- lm(MHC_SF_OVERALL ~ ., data = data)
design_matrix <- model.matrix(model_full)[,-1]
response <- model_full$model$MHC_SF_OVERALL
scaled_design_matrix <- scale(design_matrix)
scaled_response <-scale(response)

design_mat_col_group <- c(1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,7,8,9,9,9,9,9,9,9,10,11,12,13,14,15)
data.frame(colnames(design_matrix), design_mat_col_group) #for check

model_full_lasso_fit <- gglasso(
    scaled_design_matrix, scaled_response,
    lambda = 0.2,
    group = design_mat_col_group, loss = "ls")
coef(model_full_lasso_fit)
plot(model_full_lasso_fit)




# group lasso from step1 model


model_step1 <- lm(MHC_SF_OVERALL ~ gender + age_group + country_lockdown + marital +
    smoking + fivfruitveg + shielded + week_soc_distancing +
    athlete + AIMS_TOTAL + HADS_OVERALL + RES_TOTAL + LONE_TOTAL, data = data)
design_matrix_step1 <- scale(model.matrix(model_step1)[,-1])
design_response_step1 <- scale(model_step1$model$MHC_SF_OVERALL)
dim(design_matrix_step1)
length(design_response_step1)

colnames(design_matrix_step1)
design_mat_step1_col_group <- 
    c(1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,7,8,8,8,8,8,8,8,9,10,11,12,13)
data.frame(colnames(design_matrix_step1), design_mat_step1_col_group) #for check

model_step1_lasso_fit <- gglasso(
    design_matrix_step1, design_response_step1,
    lambda = 0.2,
    group = design_mat_step1_col_group, loss = "ls")
coef(model_step1_lasso_fit)
plot(model_step1_lasso_fit)
