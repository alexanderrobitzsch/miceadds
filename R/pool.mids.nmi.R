## File Name: pool.mids.nmi.R
## File Version: 0.200


#**** pooling function for nested multiple imputation objects
pool.mids.nmi <- function( object, method="largesample" )
{
    call <- match.call()
    Nimp <- object$Nimp
    NB <- Nimp["between"]
    NW <- Nimp["within"]
    anal <- object$analyses
    pool_results <- as.list(1:NB)
    for (bb in 1:NB){
        anal.bb <- anal[[bb]]
        anal2 <- list( "analyses"=anal.bb )
        class(anal2) <- "mira"
        dfcom <- 1e7
        # pool_results[[bb]] <- mice::pool( anal2, dfcom=dfcom)
        tpool <- mice::pool( anal2, dfcom=dfcom)
        pool_results[[bb]] <- tpool
    }
    pool1 <- pool_results[[1]]$pooled$b
    NV <- length(pool1)
    cn <- colnames(pool)
    cn <- paste(tpool$pooled[,"term"])
    v1 <- vcov( object$analyses[[1]][[1]] )
    if ( ! is.null( rownames(v1) ) ){
        cn <- rownames(v1)
    }

    # collect parameter estimates
    qhat <- array( NA, dim=c(NB,NW,NV) )
    dimnames(qhat)[[3]] <- cn
    dimnames(qhat)[[1]] <- paste0("Between_Imp", 1:NB )
    dimnames(qhat)[[2]] <- paste0("Within_Imp", 1:NW )
    for (bb in 1:NB){
        for (ww in 1:NW){
            c1 <- coef(anal[[bb]][[ww]])
            qhat[bb,ww,] <- c1
        }
    }

    # collect estimated variance matrices
    u <- array( NA, dim=c( NB, NW, NV, NV) )
    dimnames(u)[[4]] <- dimnames(u)[[3]] <- cn
    dimnames(u)[[1]] <- paste0("Between_Imp", 1:NB )
    dimnames(u)[[2]] <- paste0("Within_Imp", 1:NW )

    for (bb in 1:NB){
        for (ww in 1:NW){
            # u[bb,,,] <- pool_results[[bb]]$u
            u[bb,ww,,] <- vcov(anal[[bb]][[ww]])
        }
    }

    #-- summary statistics
    fit <- pool_nmi_scalar_helper( qhat=qhat, u=u, NV=NV, NB=NB, NW=NW )
    fit$Nimp <- Nimp
    class(fit) <- "mipo.nmi"
    return(fit)
}

#**** coef and vcov method
coef.mipo.nmi <- function( object, ... )
{
    return(object$qbar)
}

vcov.mipo.nmi <- function( object, ... )
{
    return(object$Tm)
}
