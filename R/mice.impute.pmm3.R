## File Name: mice.impute.pmm3.R
## File Version: 4.53


mice.impute.pmm3 <- function (y, ry, x, donors=3, noise=10^5, ridge=10^(-5), ...)
{
    x <- cbind(1, as.matrix(x))
    Nmis <- sum(!ry)
    parm <- .norm.draw3(y, ry, x, ridge=ridge,  ...)
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    yobs <- y[ ry ]
    # define distance matrix
    M1 <- matrix( yhatobs[,1], nrow=sum(!ry), ncol=sum(ry), byrow=TRUE )
    M2 <- matrix( yhatmis[,1], nrow=sum(!ry), ncol=sum(ry) )
    disty <- abs( M2 - M1 )
    donor.ind <- sirt::rowKSmallest2.sirt(matr=disty, K=donors )$smallind
    N1 <- nrow(disty)
    # sampled indices
    ind.sample <- sample( 1:donors, N1, replace=TRUE )
    # select index
    res1 <- donor.ind[ cbind( 1:N1, ind.sample) ]
    # search for imputed values
    imp <- yobs[ res1 ]
    return(imp)
}

