## File Name: mice_imputation_norm_draw.R
## File Version: 0.24


mice_imputation_norm_draw <- function (y, ry, x, ridge=1e-05, ...)
{
    xobs <- x[ry, ]
    yobs <- y[ry]
    xtx <- crossprod( xobs )
    pen <- rep( ridge, ncol(xtx) )
    v <- solve(xtx + diag(pen) )
    coef <- v %*%  crossprod(xobs,yobs)
    residuals <- yobs - xobs %*% coef
    df_r <- sum(ry) - ncol(x)
    df_r <- max( df_r, 2 )
    sigma.star <- sqrt(sum((residuals)^2)/ stats::rchisq(1, df_r ))
    beta.star <- coef + (t(chol((v + t(v))/2)) %*% stats::rnorm(ncol(x))) * sigma.star
    #* predicted values
    yhatobs <- x[ry, ] %*% coef
    yhatmis <- x[!ry, ] %*% beta.star
    yobs <- y[ ry ]
    #-- output
    parm <- list(coef=coef, beta=beta.star, sigma=sigma.star, yhatobs=yhatobs,
                    yhatmis=yhatmis, yobs=yobs)
    return(parm)
}

miceadds_norm_draw <- mice_imputation_norm_draw
.norm.draw2 <- mice_imputation_norm_draw
.norm.draw3 <- mice_imputation_norm_draw
