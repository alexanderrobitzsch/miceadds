## File Name: mice.impute.norm3.R
## File Version: 0.16

mice.impute.norm3 <- function (y, ry, x, ridge=10^(-5), ...)
{
    x <- cbind(1, as.matrix(x))
    wy <- ! ry
    parm <- mice_imputation_norm_draw(y=y, ry=ry, x=x, ridge=ridge,  ...)
    n <- sum(wy)
    imp <- x[wy, ] %*% parm$beta + stats::rnorm(n) * parm$sigma
    return(imp)
}
