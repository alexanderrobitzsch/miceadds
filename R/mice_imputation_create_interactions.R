## File Name: mice_imputation_create_interactions.R
## File Version: 1.191


#** create interactions
mice_imputation_create_interactions <- function (y_, xobs_, xall_,
    index_int_, min_int_cor_, maxcols_, imputationWeights=NULL, ry=NULL,
    use_weights=FALSE )
{
    weights_obs <- 0
    if (use_weights){
        weights_obs <- normalize_vector(imputationWeights[ry])
        y_ <- miceadds_weighted_scaling_y(y=y_, w=weights_obs)
        xobs_ <- mice_imputation_pls_scale_x( x=xobs_, imputationWeights=weights_obs,
                            use_weights=use_weights )
        weights_obs <- weights_obs / length(weights_obs)
    }
    res <- miceadds_rcpp_create_interactions( Yr=y_, Xr=xobs_, Xallr=xall_,
                index_int=index_int_, MI=min_int_cor_, maxcols=maxcols_,
                use_weights=use_weights, weights_obs=weights_obs)
    r1 <- res$allcorrs
    r1[ is.na( r1[,1] ), 1] <- 0
    #---- remove some interactions with SD=0
    g00 <- which( r1[,1] %in% c(-Inf,Inf) )
    if ( length(g00) > 0 ){
        r1[ g00, 1 ] <- 0
        r1[ g00, 2 ] <- 0
        xint <- res$xint
        xint <- xint[, - g00 ]
        res$N_interactions <- ncol(xint)
        res$xint <- xint
        res$allcorrs <- r1
    }
    return(res)
}
