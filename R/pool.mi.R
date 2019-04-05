## File Name: pool.mi.R
## File Version: 0.35

##############################################################
# Inference for multiply imputed datasets
# The following code is copied from the mice package
# and slightly modified.
pool_mi <- function( qhat, u=NULL, se=NULL, dfcom=1E7, method="smallsample" )
{
    #****
    # qhat    ... List of parameter vectors
    # u        ... List of covariance matrices
    # se    ... List of parameter vectors of standard errors
    #****
    CALL <- match.call()

    eps <- 1E-100
    m <- length(qhat)
    k <- length(qhat[[1]] )

    if ( ! is.null(se) ){
        u <- list(1:m)
        for (ii in 1:m){
            u[[ii]] <- diag( se[[ii]]^2 )
        }
    }

    q1 <- qhat[[1]]
    names1 <- names(q1)
    qhat <- unlist(qhat)
    qhat <- matrix( qhat, nrow=m, ncol=k, byrow=TRUE )
    qbar <- colMeans(qhat)
    u0 <- u
    u <- array( 0, dim=c(m,k,k) )
    for (ii in 1:m){
        u[ii,,] <- u0[[ii]]
    }
    ubar <- apply( u, c(2, 3), mean )
    e <- qhat - matrix(qbar, nrow=m, ncol=k, byrow=TRUE)
    b <- ( t(e) %*% e )/(m - 1 + eps )
    t <- ubar + (1 + 1/m) * b
    r <- (1 + 1/m) * diag(b/ubar)
    lambda <- (1 + 1/m) * diag(b/t)
    df <- mice_df(m, lambda, dfcom, method)
    fmi <- (r + 2/(df + 3))/(r + 1)
    names(lambda) <- names1
    names(fmi) <- names1
    names(df) <- names1
    names(r) <- names1
    names(qbar) <- names1
    rownames(t) <- colnames(t) <- names1

    #----
    # include t values and standard errors
    tval <- qbar / sqrt( diag(t) )
    pval <- 2 * stats::pt( - abs(tval), df=df )
    names(tval) <- names(pval) <- names1

    #********************************
    # class mipo
    res <- list( nmis=NA, m=m, qhat=qhat, u=u, qbar=qbar,
        ubar=ubar, b=b, t=t, r=r, dfcom=dfcom, df=df,
        fmi=fmi, lambda=lambda, tval=tval, pval=pval,
        qhat_names=names1, call=CALL)
    class(res) <- "pool_mi"
    return(res)
}

###########################################################
# Calculation of degrees of freedom
mice_df <- function (m, lambda, dfcom, method)
{
    eps <- 1E-4
    lambda[lambda < eps ] <- eps
    dfold <- (m - 1 + eps )/lambda^2
    dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
    df <- dfold * dfobs/(dfold + dfobs)
    if (method !="smallsample"){
        df <- dfold
    }
    return(df)
}

###########################################################
# This function is a modification of summary.MIresult (mitools package)
summary.pool_mi <-function(object, alpha=0.05, ...)
{
    cat("Multiple imputation results:\nCall: ")
    print(object$call)
    out <- data.frame( results=object$qbar, se=sqrt(diag( object$t)) )
    crit <- stats::qt(alpha/2,object$df, lower.tail=FALSE)
    out$t <- object$tval
    out$p <- object$pval
    out$"(lower"<- out$results-crit*out$se
    out$"upper)"<- out$results+crit*out$se
    out$"missInfo" <- paste0(round(100*object$fmi,1), " %")
    print(out, ...)
}

############################################################
coef.pool_mi <- function(object, ...)
{
    return(object$qbar)
}

vcov.pool_mi <- function(object, ...)
{
    return(object$t)
}
