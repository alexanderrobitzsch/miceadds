## File Name: mice_multilevel_imputation_draw_residuals.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:49

################################################
# draw residuals
mice_multilevel_imputation_draw_residuals <- function(predicted, sigma ){
	N0 <- length(predicted)
	imp <- predicted + stats::rnorm(N0 , mean=0 , sd = sigma)	
	return(imp)
}	
