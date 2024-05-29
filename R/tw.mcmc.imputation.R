## File Name: tw.mcmc.imputation.R
## File Version: 1.166

tw.mcmc.imputation <- function( data, iter=100, integer=FALSE )
{
    # set N and J
    N <- nrow(data)
    J <- ncol(data)
    data0 <- data
    round.near <- integer
    # Two-way imputed data (with original non-MCMC procedure)
    data.imp1 <- tw.imputation( data=data )
    # eliminate cases with exclusively missings
    ind1 <- which( rowMeans(is.na(data0))==1 )
    ind2 <- setdiff( 1L:(nrow(data)), ind1 )
    if ( length(ind1) > 0 ){
        data <- data[ -ind1, ]
        data.imp1 <- data.imp1[ -ind1,]
    }
    # set initial values
    mu <- mean( as.matrix(data.imp1 ), na.rm=TRUE )
    beta <- colMeans( data.imp1, na.rm=TRUE) - mu
    alpha <- rowMeans( data.imp1, na.rm=TRUE )
    sig2 <- sum( ( data.imp1 - outer( alpha, rep(1,J) ) -
                        outer( rep(1,N), beta ) )^2 ) / ( N - 1 ) / ( J - 1 )
    tau2 <- stats::var( alpha - mu )

    #**** MCMC algorithm
    for ( ii in 1L:iter ){
        # sample alpha
        mean.alpha <- ( mu / tau2 + rowSums( data  -
                            outer( rep(1,N), beta ), na.rm=TRUE ) / sig2 ) /
                            ( 1 / tau2 + rowSums( ! is.na(data) ) / sig2 )
        sd.alpha <-  sqrt( 1 / ( 1 / tau2 + rowSums( !is.na(data) ) / sig2 ) )
        alpha <- stats::rnorm( N, mean=mean.alpha, sd=sd.alpha )
        # sample beta
        mean.beta <- colSums( data - outer( alpha, rep( 1, J ) ), na.rm=TRUE ) /
                            colSums( ! is.na(data) )
        sd.beta <- sqrt( sig2 / colSums( ! is.na(data) ) )
        beta <- stats::rnorm( J, mean=mean.beta, sd=sd.beta )
        # sample sigma2
        nu <- sum( ! is.na(data)  )
        scale.S <- sum( ( data - outer( alpha, rep(1,J) ) -
                            outer( rep(1,N), beta ) )^2, na.rm=TRUE ) / nu
        sig2 <- nu * scale.S / stats::rchisq( 1, df=nu )
        # sample mu
        mu <- stats::rnorm( 1, mean=mean( alpha  ), sd=sqrt( tau2 / N ) )
        # sample tau
        nu <- N
        scale.S <- sum(( alpha - mu )^2 ) / N
        tau2 <- nu * scale.S / stats::rchisq( 1, df=nu )
    }
    # impute missing data
    mean.X <- outer( alpha, rep(1,J) ) + outer( rep(1,N), beta )
    sd.X <- sqrt( sig2 )
    data.imp2 <- matrix( stats::rnorm( N*J, mean=mean.X, sd=sd.X ), ncol=J)
    data.imp <- data
    ind <- is.na(data)
    data.imp[ind] <- data.imp2[ind]
    # Round to the nearest integer
    if (round.near){
        mindat <- min( data, na.rm=TRUE)
        maxdat <- max( data, na.rm=TRUE)
        data.imp <- round( data.imp )
        data.imp[ data.imp < mindat ] <- mindat
        data.imp[ data.imp > maxdat ] <- maxdat
    }
    # restructure data
    data0[ ind2, ] <- data.imp
    return(data0)
}
