## File Name: mice_imputation_weighted_norm_draw.R
## File Version: 0.03
	
#***** .weighted.norm.draw *******************
mice_imputation_weighted_norm_draw <- function( yobs , xobs , ry , y , x , 
	weights.obs , ridge = .00001 , ... ){
    WW <- diag( weights.obs )
    # X'*W*X
    xtx <- t(xobs) %*% WW %*% xobs
    pen <- ridge * diag(xtx)
    if (length(pen)==1){
		pen <- matrix(pen)
	}
    v <- solve(xtx + diag(pen))
    # V * ( X'*W*Y)
    coef <- t(yobs %*% WW %*% xobs %*% v)
    residuals <- yobs - xobs %*% coef
    # calculate weighted residuals
    residuals2 <- weights.obs * residuals
    sigma.star <- sqrt(sum((residuals2)^2)/ stats::rchisq(1, sum(ry) - ncol(x)))  
    beta.star <- coef + (t(chol((v + t(v))/2)) %*% stats::rnorm(ncol(x))) * sigma.star
    parm <- list(coef, beta.star, sigma.star)     
    names(parm) <- c("coef","beta", "sigma") 
    return(parm)
}
#----------------------------------------------------------------------

#*** deprecated function
.weighted.norm.draw <- mice_imputation_weighted_norm_draw
