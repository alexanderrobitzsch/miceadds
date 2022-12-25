## File Name: mice_imputation_weighted_norm_draw.R
## File Version: 0.300

mice_imputation_weighted_norm_draw <- function( yobs, xobs, ry, y, x,
    weights.obs, ridge=1e-5, sample_pars=TRUE,... )
{
    requireNamespace("MASS")
    requireNamespace("Matrix")
    n <- length(yobs)
    # WW <- diag(weights.obs)
    # X'*W*X
    # xtx <- t(xobs) %*% WW %*% xobs
    weights_obs_sqrt <- sqrt(weights.obs)
    xobs1 <- weights_obs_sqrt * xobs
    xtx <- crossprod(xobs1)
    diag_xtx <- diag(xtx)
    eps1 <- 1e-10
    diag_xtx[ diag_xtx < eps1 ] <- eps1
    pen <- ridge * diag_xtx
    if (length(pen)==1){
        pen <- matrix(pen)
    }
    # v <- solve(xtx + diag(pen))
    v <- MASS::ginv(xtx + diag(pen))
    # V * ( X'*W*Y)
    # coef <- t(yobs %*% WW %*% xobs %*% v)
    yobs1 <- weights_obs_sqrt * yobs
    coef <- t( yobs1 %*% xobs1 %*% v )
    residuals <- yobs - xobs %*% coef
    # calculate weighted residuals
    residuals2 <- sqrt(weights.obs) * residuals

    if (sample_pars){
        sigma.star <- sqrt(sum((residuals2)^2)/ stats::rchisq(1, sum(ry) - ncol(x)))
        V <- (v + t(v))/2
        V <- Matrix::nearPD(x=V, base.matrix=TRUE)
        if (is.list(V)){
            V <- as.matrix(V$mat)
        }

        beta.star <- coef + (t(chol(V)) %*% stats::rnorm(ncol(x))) * sigma.star
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
