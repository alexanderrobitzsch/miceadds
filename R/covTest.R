## File Name: covTest.R
## File Version: 0.091


#-- function for testing the covariance
covTest <- function( x, y, conf.level=.95 )
{
    #*** exclude missings
    ind <- ( ! is.na(x) ) & ( ! is.na(y) )
    x <- x[ind]
    y <- y[ind]
    N <- length(x)
    est <- stats::cov(x=x,y=y)
    mx <- mean(x)
    my <- mean(y)
    # mu11
    mu11 <- est
    # mu22
    mu22 <- mean( (x-mx)^2*(y-my)^2 )
    # mu20, mu02
    mu20 <- stats::var(x)
    mu02 <- stats::var(y)
    # variance approximation
    se1 <- 1/N * ( mu22 - mu11^2 )
    se2 <- 1/N/(N-1)*( mu11^2 + mu20*mu02)
    se <- sqrt(se1+se2)
    # confidence interval
    quant <- stats::qnorm( 1 - (1-conf.level)/2 )
    inter <- est + quant * se * c(-1,1)
    res <- list(est=est, se=se, lower=inter[1], upper=inter[2] )
    return(res)
}
