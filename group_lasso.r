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
design_matrix_full <- model.matrix(model_full)[, -1]
conti_var_idx <- c(24, 33, 35, 36, 37, 38)
design_matrix_full <- scale(model.matrix(model_full)[, -1])
# for(i in conti_var_idx) {
#     design_matrix_full[, i] <- scale(design_matrix_full[, i])
# }
response_full <- scale(model_full$model$MHC_SF_OVERALL)

model_full_col_group <- c(1, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 7, 8,
    9, 9, 9, 9, 9, 9, 9, 10, 11, 12, 13, 14, 15)
data.frame(colnames(design_matrix_full), model_full_col_group) #for check

# fit with cv
model_full_lasso_cv <- cv.gglasso(
    design_matrix_full, response_full,
    group = model_full_col_group,
    pred.loss = "L2",
    nfolds = 10)

#first plot

plot(model_full_lasso_cv)
#choose lambda = 0.03
log(0.03) # -3.506558


#fit
model_full_lasso_fit <- gglasso(
    design_matrix_full, response_full,
    lambda = 0.03,
    group = model_full_col_group, loss = "ls")

names(model_full_lasso_fit)
coef(model_full_lasso_fit)
model_full_lasso_fit$npasses
lasso_coeff <- coef(model_full_lasso_fit)[coef(model_full_lasso_fit) != 0]
names(coef(model_full_lasso_fit)[coef(model_full_lasso_fit) != 0])
lasso_coeff
round(coef(model_full_lasso_fit), 5)
# (Intercept)           9.268054e-17
# gender2               5.394306e-02
# age_group2           -5.538053e-03
# age_group3           -4.049320e-03
# age_group4            1.131431e-02
# age_group5            2.122124e-03
# age_group6            1.981590e-02
# age_group7            4.816107e-03
# fivfruitveg2         -3.751264e-02
# week_soc_distancing1 -1.630502e-02
# week_soc_distancing2  1.714444e-02
# week_soc_distancing3 -4.557470e-03
# week_soc_distancing4  6.964271e-03
# week_soc_distancing5 -2.368693e-02
# week_soc_distancing6 -1.100702e-03
# week_soc_distancing7  2.481028e-02
# athlete2             -2.739221e-02
# HADS_OVERALL         -4.043449e-01
# RES_TOTAL             1.132735e-01
# LONE_TOTAL           -2.884045e-01

# lambda vs coeff plots
try_log_lambdas <- seq(-2.4, -4, by = -0.01)
coeff_mat <- matrix(0, 38, length(try_log_lambdas))
dim(coeff_mat)
rownames(coeff_mat) <- colnames(design_matrix_full)
colnames(coeff_mat) <- try_log_lambdas
for (i in seq_len(length(try_log_lambdas))) {
    log_lambda <- try_log_lambdas[i]
    beta_at_each_lambda <- gglasso(
        design_matrix_full, response_full,
        lambda = exp(log_lambda),
        group = model_full_col_group, loss = "ls")$beta
    coeff_mat[, i] <- beta_at_each_lambda
}

#second plot
plot(try_log_lambdas, rep(0, length(try_log_lambdas)), type = "n",
    xlim = c(min(try_log_lambdas), max(try_log_lambdas)), ylim = c(-0.4, 0.2),
    xlab = "log-lambda", ylab = "coefficients")
for (i in 1:38) {
    lines(try_log_lambdas, coeff_mat[i, ], col = i + 1)
    coeff_at_eval_point <- coeff_mat[i, which(try_log_lambdas == -3.5)]
    if (coeff_at_eval_point > 0.02 | coeff_at_eval_point < -0.02) {
        text(-4, coeff_mat[i, length(try_log_lambdas)],
            rownames(coeff_mat)[i], pos = 4)
        print(rownames(coeff_mat)[i])
    }
}
abline(h = 0)
abline(v = log(0.03), lty = 2); text(-3.5, -0.2, "lambda=0.03")

#===============================================================
#AFTER the LASSO MODEL, LM FIT


after_lasso_lm_fit_non_scaled <- lm(MHC_SF_OVERALL ~ gender + age_group +
    fivfruitveg + week_soc_distancing +
    athlete + HADS_OVERALL + RES_TOTAL + LONE_TOTAL, data = data)
summary(after_lasso_lm_fit_non_scaled)


after_lasso_lm_fit_non_scaled_wo_RES <- lm(MHC_SF_OVERALL ~ gender + age_group +
    fivfruitveg + 
    week_soc_distancing +
    athlete + HADS_OVERALL + LONE_TOTAL, data = data[complete.cases(data), ])
summary(after_lasso_lm_fit_non_scaled_wo_RES)
AIC(after_lasso_lm_fit_non_scaled_wo_RES)
BIC(after_lasso_lm_fit_non_scaled_wo_RES)

scaled <- scale(model.matrix(after_lasso_lm_fit_non_scaled)[, -1])
response_al <- scale(after_lasso_lm_fit_non_scaled$model$MHC_SF_OVERALL)
after_lasso_lm_fit <- lm(response_al ~ scaled)
summary(after_lasso_lm_fit)

coef(after_lasso_lm_fit)[coef(after_lasso_lm_fit) != 0]
data.frame(lm_coeff = coef(after_lasso_lm_fit),
    lasso_coeff = lasso_coeff)


par(mfrow = c(2, 2))
plot(after_lasso_lm_fit)
