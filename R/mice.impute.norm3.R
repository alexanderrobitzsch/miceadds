## File Name: mice.impute.norm3.R
## File Version: 0.13

mice.impute.norm3 <- function (y, ry, x, ridge=10^(-5), ...)
{
    x <- cbind(1, as.matrix(x))
    wy <- ! ry
    parm <- .norm.draw3(y, ry, x, ridge=ridge,  ...)
    n <- sum(wy)
    imp <- x[wy, ] %*% parm$beta + stats::rnorm(n) * parm$sigma
    return(imp)
}
