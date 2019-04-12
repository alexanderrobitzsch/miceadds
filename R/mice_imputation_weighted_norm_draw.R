## File Name: mice_imputation_weighted_norm_draw.R
## File Version: 0.26

mice_imputation_weighted_norm_draw <- function( yobs, xobs, ry, y, x,
    weights.obs, ridge=1e-5, sample_pars=TRUE,... )
{
    n <- length(yobs)
    WW <- diag(weights.obs)
    # X'*W*X
    # xtx <- t(xobs) %*% WW %*% xobs
    weights_obs_sqrt <- sqrt(weights.obs)
    xobs1 <- weights_obs_sqrt * xobs
    xtx <- crossprod(xobs1)
    pen <- ridge * diag(xtx)
    if (length(pen)==1){
        pen <- matrix(pen)
    }
    v <- solve(xtx + diag(pen))
    # V * ( X'*W*Y)
    # coef <- t(yobs %*% WW %*% xobs %*% v)
    yobs1 <- weights_obs_sqrt * yobs
    coef <- t( yobs1 %*% xobs1 %*% v )
    residuals <- yobs - xobs %*% coef
    # calculate weighted residuals
    residuals2 <- sqrt(weights.obs) * residuals
    if (sample_pars){
        sigma.star <- sqrt(sum((residuals2)^2)/ stats::rchisq(1, sum(ry) - ncol(x)))
        beta.star <- coef + (t(chol((v + t(v))/2)) %*% stats::rnorm(ncol(x))) * sigma.star
    } else {
        beta.star <- coef
        sigma.star <- sqrt(mean((residuals2)^2))
    }

    #-- output
    parm <- list( coef=coef, beta=beta.star, sigma=sigma.star)
    return(parm)
}
#----------------------------------------------------------------------

#*** deprecated function
.weighted.norm.draw <- mice_imputation_weighted_norm_draw
