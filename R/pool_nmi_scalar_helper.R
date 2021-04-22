## File Name: pool_nmi_scalar_helper.R
## File Version: 0.341

pool_nmi_scalar_helper <- function( qhat, u, NV, NB, NW, comp_cov=TRUE,
        method=1 )
{

    # formulas follow Reiter & Raghanuthan (2007)
    #  multiple adaptations of multiple imputation
    qbar <- apply( qhat, 3, mean )
    ubar <- apply( u, c(3,4), mean )
    qbar_l <- apply( qhat, c(1,3), mean )
    # calculate variance statistics
    Wm <- matrix(NA,nrow=NV,ncol=NV)
    colnames(Wm) <- rownames(Wm) <- dimnames(qhat)[[3]]
    Bm <- Wm
    eps <- 1E-50

    # comp_cov <- TRUE
    if (NV==1){
        comp_cov <- FALSE
    }

    for (vv in 1:NV){
        qbar_l.vv <- qbar_l[,vv]
        qhat.vv <- qhat[,,vv]
        Wm[vv,vv] <-  sum( ( qhat.vv - qbar_l.vv )^2 ) / NB / (NW - 1 + eps)
        Bm[vv,vv] <- sum( (qbar_l.vv - qbar[vv])^2 ) / (NB - 1 + eps)
    }
    if ( comp_cov ){
        for (vv1 in 1:(NV-1)){
            for (vv2 in (vv1+1):(NV)){
                qbar_l.vv1 <- qbar_l[,vv1]
                qhat.vv1 <- qhat[,,vv1]
                qbar_l.vv2 <- qbar_l[,vv2]
                qhat.vv2 <- qhat[,,vv2]
                Wm[vv2,vv1] <- Wm[vv1,vv2] <-  sum( ( qhat.vv1 - qbar_l.vv1 ) *
                                ( qhat.vv2 - qbar_l.vv2 ) ) / NB / (NW - 1 + eps)
                Bm[vv2,vv1] <- Bm[vv1,vv2] <- sum( (qbar_l.vv1 - qbar[vv1]) *
                                (qbar_l.vv2 - qbar[vv2]) ) / (NB - 1 + eps)
            }  # end vv2
        }  # end vv1
    }    # end comp_cov

    Um <- ubar
    Tm <- (1+1/NB)*Bm + (1-1/NW)*Wm + Um
    df <- ( (1+1/NB)*Bm )^2 / (NB-1 + eps) / Tm^2 +
                    ( (1-1/NW)*Wm )^2 / NB / (NW-1 + eps) / Tm^2
    df <- 1 / diag( df )
    # fraction of missing information
    # in case of MI lambda is calculated as
    #        lambda <- (1 + 1/m) * diag(b/t)
    if (method==1){
        # lambda <- diag( ( Bm + (1-1/NW)*Wm ) / ( Um + Bm + (1-1/NW)*Wm ) )
        rmn <- ( ( 1 + 0 ) * diag(Bm) + ( 1 - 1 /NW ) * diag(Wm) ) / diag(Um)
    }

    #****
    # Harel & Schafer formula Sect. 3
    if ( method==2){
        rmn <- ( ( 1 + 1/NB) * diag(Bm) + ( 1 - 1 /NW ) * diag(Wm) ) / diag(Um)
    }
    lambda <- rmn / ( 1 + rmn )

    if (method==1){
         # lambda_Within <- diag( Wm / ( Um + Wm ) )
        rBA <- ( 1 + 0 ) * diag(Wm) / diag(Um)
    }
    if (method==2){
        rBA <- ( 1 + 1 / (NB*NW) ) * diag(Wm) / diag(Um)
    }
    lambda_Within <- rBA / ( 1 + rBA )
    lambda_Between <- lambda - lambda_Within
    #*** In the MI case
    # fmi <- (r + 2/(df + 3))/(r + 1)
    names1 <- colnames(Wm)
    tval <- qbar / sqrt( diag(Tm) )
    pval <- 2 * stats::pt( - abs(tval), df=df )
    names(tval) <- names(pval) <- names1

    #--- output
    fit <- list( qhat=qhat, u=u, qbar=qbar, ubar=ubar,
                Wm=Wm, Bm=Bm, Tm=Tm, df=df, lambda=lambda, lambda_Between=lambda_Between,
                lambda_Within=lambda_Within, tval=tval, pval=pval, NV=NV )
    return(fit)
}


pool.nmi.scalar.helper <- pool_nmi_scalar_helper
