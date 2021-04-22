## File Name: NMIcombine.R
## File Version: 0.363


#* extension of MIcombine function to nested
#* multiply imputed datasets
NMIcombine <- function( qhat, u=NULL, se=NULL,
        NMI=TRUE, comp_cov=TRUE, is_list=TRUE, method=1 )
{

    u_NULL <- is.null(u)
    u0 <- u
    if ( ! NMI ){
            is_list <- TRUE
    }

    #--- list elements
    if ( is_list ){
        #--- no NMI
        if ( ! NMI ){
            NB <- length(qhat)
            # restructure qhat
            qhat0 <- qhat
            q1 <- as.list(1)
            qhat <- as.list(1:NB)
            for (bb in 1:NB){
                q1[[1]] <- qhat0[[bb]]
                qhat[[bb]] <- q1
            }
            # restructure u
            if ( ! is.null(u) ){
                u0 <- u
                u1 <- as.list(1)
                u <- as.list(1:NB)
                for (bb in 1:NB){
                    u1[[1]] <- u0[[bb]]
                    u[[bb]] <- u1
                }
            }
            # restructure se
            if ( ! is.null(se) ){
                se0 <- se
                u1 <- as.list(1)
                se <- as.list(1:NB)
                for (bb in 1:NB){
                    u1[[1]] <- se0[[bb]]
                    se[[bb]] <- u1
                }
            }
        }

        #--- parameter dimensions
        NB <- length( qhat )
        NW <- length( qhat[[1]] )
        NV <- length( qhat[[1]][[1]] )
        qhat0 <- qhat
        u0 <- u

        # collect parameter estimates
        qhat <- array( NA, dim=c( NB, NW, NV ) )
        v1 <- qhat0[[1]][[1]]
        names_vars <- names(v1)
        if (is.null(names_vars)){
            names_vars <- paste0("par",1:length(v1))
        }
        dimnames_qhat <- NMIcombine_include_dimnames( names_vars=names_vars, NB=NB,
                                NW=NW, dims=3)
        dimnames(qhat) <- dimnames_qhat

        for (bb in 1:NB){
            for (ww in 1:NW){
                qhat[bb,ww,] <- qhat0[[bb]][[ww]]
            }
        }

        # collect estimated variance matrices
        u <- array( 0, dim=c( NB, NW, NV, NV) )
        dimnames_u <- NMIcombine_include_dimnames( names_vars=names_vars, NB=NB,
                                NW=NW, dims=4)
        dimnames(u) <- dimnames_u
        if ( ! is.null(u0) ){
            for (bb in 1:NB){
                for (ww in 1:NW){
                    u[bb,ww,,] <- u0[[bb]][[ww]]
                }
            }
        }
    }  ## end is_list==TRUE
    #---

    if ( ! is.null(se) ){
        dim_qhat <- dim(qhat)
        NB <- dim_qhat[1]
        NW <- dim_qhat[2]
        NV <- dim_qhat[3]
        u <- array( 0, dim=c( NB, NW, NV, NV) )
        dimnames_u <- NMIcombine_include_dimnames( names_vars=names_vars, NB=NB,
                                NW=NW, dims=4)
        dimnames(u) <- dimnames_u
        for (bb in 1:NB){
           for (ww in 1:NW){
                h1 <- se[[bb]][[ww]]
                h2 <- matrix( 0, nrow=NV, ncol=NV)
                diag(h2) <- h1^2
                u[bb,ww,,] <- h2
            }
        }
        u_NULL <- FALSE
        u0 <- u
    }

    if ( ! is_list ){
        NV <- dim(qhat)[[3]]
        NB <- dim(qhat)[[1]]
        NW <- dim(qhat)[[2]]
    }

    #--- NMI inference
    res <- pool_nmi_scalar_helper( qhat=qhat, u=u, NV=NV, NB=NB, NW=NW,
                comp_cov=comp_cov, method=method)

    if ( is.null(u0) ){
        vars <- c("ubar", "Wm", "Bm", "Tm", # "df",
                    "lambda", "lambda_Between", "lambda_Within" )
        for (vv in vars){
            res[[vv]] <- NA * res[[vv]]
        }
    }

    res$Nimp <- c("Between"=NB, "Within"=NW )
    res$u_NULL <- u_NULL
    class(res) <- "mipo.nmi"
    return(res)
}

pool_nmi <- NMIcombine
