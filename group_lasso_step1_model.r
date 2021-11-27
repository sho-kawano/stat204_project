
# # group lasso from step1 model

# model_step1 <- lm(MHC_SF_OVERALL ~ gender + 
#     age_group + country_lockdown + marital +
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




