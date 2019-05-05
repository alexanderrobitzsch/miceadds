## File Name: nnig_coef.R
## File Version: 0.18

nnig_coef <- function( mean=NULL, Sigma, skew, kurt )
{
    require_namespace("TAM")
    p <- length(skew)
    sigma <- sd1 <- sqrt( diag(Sigma) )
    sd1M <- TAM::tam_outer( sd1, sd1)
    Sigma0 <- Sigma / sd1M
    A <- t(chol(Sigma0, pivot=FALSE))
    colnames(A) <- paste0("X",1:p)
    rownames(A) <- paste0("Y",1:p)
    p <- ncol(Sigma0)

    if (is.null(mean)){
        mean <- rep(0,p)
    }
    x <- rep(0,p)

    #** skewness
    skew_fct <- function(x){
        fct <- rep(0,p)
        for (ii in 1:p){
            a1 <- sum( A[ii,]^3 * x )
            a2 <- sum( A[ii,]^2 )^(1.5)
            fct[ii] <- a1 / a2 - skew[ii]
        }
        val <- sum( fct^2 )
        return(val)
    }
    res <- stats::optim( par=skew, fn=skew_fct)
    ig_skew <- res$par

    #** kurtosis
    kurt_fct <- function(x){
        fct <- rep(0,p)
        for (ii in 1:p){
            a1 <- sum( A[ii,]^4 * x )
            a2 <- sum( A[ii,]^2 )^2
            fct[ii] <- a1 / a2 - kurt[ii]
        }
        val <- sum( fct^2 )
        return(val)
    }
    res <- stats::optim( par=kurt, fn=kurt_fct)
    ig_kurt <- res$par

    #** Fleishman coefficients
    fcoef <- matrix(NA, nrow=p, ncol=4)
    rownames(fcoef) <- colnames(A)
    colnames(fcoef) <- c("a","b","c","d")
    for (ii in 1:p){
        cat(paste0( "Compute Fleishman polynomial for variable ", ii, "\n") )
        fcoef[ii,] <- fleishman_coef( mean=0, sd=1, skew=ig_skew[ii], kurt=ig_kurt[ii] )
    }

    #** regression coefficients
    B <- matrix(0, p, p)
    rownames(B) <- colnames(B) <- paste0("Y",1:p)
    diag(B) <- NA
    for (ii in 2:p){
        ind <- seq(1,ii-1)
        M1 <- solve( Sigma[ ind, ind ] ) %*% Sigma[ ind, ii ]
        B[ii, ind] <- as.vector(M1[ind,1])
    }

    #** output
    res <- list( A=A, fcoef=fcoef, ig_skew=ig_skew, ig_kurt=ig_kurt, B=B,
                skew=skew, kurt=kurt, mean=mean, sigma=sigma, p=p )
    return(res)
}
