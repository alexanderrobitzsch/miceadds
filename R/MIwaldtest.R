## File Name: MIwaldtest.R
## File Version: 0.201


#--- MI Wald test
MIwaldtest <- function( qhat, u, Cdes=NULL, rdes=NULL, testnull=NULL)
{
    # conversion of inputs
    if ( inherits(qhat, c("array","data.frame","matrix") ) ){
        qhat <- MIwaldtest_qhat2list(qhat=qhat)
    }
    if ( inherits(u, c("array") ) ){
        u <- MIwaldtest_u2list(u=u)
    }

    if ( ! is.null(testnull) ){
        k <- length(testnull)
        pars <- names( qhat[[1]][[1]] )
        des <- create.designMatrices.waldtest( pars=pars, k=k)
        Cdes <- des$Cdes
        rdes <- des$rdes
        for (ii in 1:k){
            Cdes[ ii, testnull[ii] ] <- 1
        }
    }

    #*** compute distribution of linear form
    NB <- length( qhat )
    NV <- length( qhat[[1]] )
    NW <- 1

    # qhat and u for linear forms
    qhat0 <- qhat
    u0 <- u
    for (bb in 1:NB){
        u00 <- u0[[bb]]
        q0 <- as.vector(qhat0[[bb]])
        qhat[[bb]] <- ( Cdes %*% q0 - rdes )[,1]
        u[[bb]] <- Cdes %*% u00 %*% t(Cdes)
    }
    #**** compute F test (D1 statistic)
    u1 <- qhat1 <- as.list( 1:NB )
    for (bb in 1:NB){
        qv1 <- list(1)
        qv1[[1]] <- qhat[[bb]]
        qhat1[[bb]] <- qv1
        qv1[[1]] <- u[[bb]]
        u1[[bb]] <- qv1
    }

    #*** statistical inference
    res <- NMIwaldtest_compute_statistic( qhat=qhat1, u=u1, Cdes=Cdes, rdes=rdes,
                    NB=NB, NW=NW )
    class(res) <- "MIwaldtest"
    return(res)
}



