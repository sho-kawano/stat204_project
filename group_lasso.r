# read data
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
# ?gglasso

model_full <- lm(MHC_SF_OVERALL ~ ., data = data)
design_matrix_full <- scale(model.matrix(model_full)[, -1])
response_full <- scale(model_full$model$MHC_SF_OVERALL)
colnames(design_matrix_full)

model_full_col_group <- c(1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,7,8,9,9,9,9,9,9,9,10,11,12,13,14,15)
data.frame(colnames(design_matrix_full), model_full_col_group) #for check

# fit with cv
model_full_lasso_cv <- cv.gglasso(
    design_matrix_full, response_full,
    group = model_full_col_group,
    pred.loss = "L2",
    nfolds = 10)
plot(model_full_lasso_cv)
#choose lambda = 0.03

#fit
model_full_lasso_fit <- gglasso(
    design_matrix_full, response_full,
    lambda = 0.03,
    group = model_full_col_group, loss = "ls")

names(model_full_lasso_fit)
coef(model_full_lasso_fit)
model_full_lasso_fit$npasses



# lambda vs coeff plots
try_log_lambdas <- seq(-2.4, -4, by = -0.01)
coeff_mat <- matrix(0, 38, length(try_log_lambdas))
dim(coeff_mat)
rownames(coeff_mat) <- colnames(design_matrix_full)
colnames(coeff_mat) <- try_log_lambdas
for(i in 1:length(try_log_lambdas)){
    log_lambda <- try_log_lambdas[i]
    beta_at_each_lambda <- gglasso(
        design_matrix_full, response_full,
        lambda = exp(log_lambda),
        group = model_full_col_group, loss = "ls")$beta
    coeff_mat[, i] <- beta_at_each_lambda
}

plot(try_log_lambdas, rep(0, length(try_log_lambdas)), type = "n",
    xlim = c(min(try_log_lambdas), max(try_log_lambdas)), ylim = c(-0.4, 0.2),
    xlab = "log-lambda", ylab = "coefficients")
for(i in 1:38) {
    lines(try_log_lambdas, coeff_mat[i, ], col = i+1)
    coeff_at_eval_point <- coeff_mat[i, which(try_log_lambdas == -3.5)]
    if(coeff_at_eval_point > 0.02 | coeff_at_eval_point < -0.02) {
        text(-4, coeff_mat[i, length(try_log_lambdas)],
            rownames(coeff_mat)[i], pos = 4)
        print(rownames(coeff_mat)[i])
    }
}
abline(h = 0)
abline(v = log(0.03), lty = 2); text(-3.5, -0.2, "lambda=0.03")



# # group lasso from step1 model

# model_step1 <- lm(MHC_SF_OVERALL ~ gender + age_group + country_lockdown + marital +
#     smoking + fivfruitveg + shielded + week_soc_distancing +
#     athlete + AIMS_TOTAL + RES_TOTAL + LONE_TOTAL, data = data)
# design_matrix_step1 <- scale(model.matrix(model_step1)[,-1])
# design_response_step1 <- scale(model_step1$model$MHC_SF_OVERALL)
# dim(design_matrix_step1)
# length(design_response_step1)

# colnames(design_matrix_step1)
# design_mat_step1_col_group <- 
#     c(1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,7,8,8,8,8,8,8,8,9,10,11,12)
# data.frame(colnames(design_matrix_step1), design_mat_step1_col_group) #for check

# model_step1_lasso_cv <- cv.gglasso(
#     design_matrix_step1, design_response_step1,
#     group = design_mat_step1_col_group, pred.loss = "L2",
#     nfolds = 10)
# plot(model_step1_lasso_cv)


# model_step1_lasso_fit <- gglasso(
#     design_matrix_step1, design_response_step1,
#     lambda = 0.03,
#     group = design_mat_step1_col_group, loss = "ls")
# coef(model_step1_lasso_fit)
# plot(model_step1_lasso_fit)




