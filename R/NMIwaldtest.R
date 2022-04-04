## File Name: NMIwaldtest.R
## File Version: 0.292


#--- Wald test for nested multiply imputed datasets
NMIwaldtest <- function( qhat, u, Cdes=NULL, rdes=NULL, testnull=NULL )
{
    # convert qhat into a list if necessary
    if ( inherits(qhat,"array") ){
        qhat <- NMIwaldtest_qhat2list(qhat=qhat)
    }
    # convert u into a list
    if ( inherits(u,"array") ){
        u <- NMIwaldtest_u2list(u=u)
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
    #**** compute distribution of linear form
    NB <- length( qhat )
    NW <- length( qhat[[1]] )
    NV <- length( qhat[[1]][[1]] )
    # qhat and u for linear forms
    qhat0 <- qhat
    u0 <- u
    for (bb in 1:NB){
        for (ww in 1:NW){
            u00 <- u0[[bb]][[ww]]
            qhat[[bb]][[ww]] <- ( Cdes %*% qhat0[[bb]][[ww]] - rdes )[,1]
            u[[bb]][[ww]] <- Cdes %*% u00 %*% t(Cdes)
        }
    }
    #**** statistical inference
    res <- NMIwaldtest_compute_statistic( qhat=qhat, u=u, Cdes=Cdes, rdes=rdes,
                    NB=NB, NW=NW )
    class(res) <- "NMIwaldtest"
    return(res)
}
