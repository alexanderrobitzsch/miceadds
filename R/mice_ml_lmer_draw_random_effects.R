## File Name: mice_ml_lmer_draw_random_effects.R
## File Version: 0.178


mice_ml_lmer_draw_random_effects <- function( clus, clus_unique, y, ry, fl, fit_vc,
        re0, ngr, used_slopes, levels_id_ll, x, random.effects.shrinkage, iter_re=0)
{

    # clusters without missing values
    clus0 <- clus[!ry]

    # ind <- match( clus0, clus_unique)
    index_clus <- match( clus, clus_unique)

    # clusters with at least one observation
    clus_obs <- match( unique(fl), clus_unique)

    vu <- fit_vc[,,drop=FALSE ]     # random effects (co-)variance

    NR <- ncol(re0)
    re <- matrix(0, nrow=ngr, ncol=NR)     # re: 0 if fully unobserved
    re[clus_obs,] <- as.matrix(re0)        # re: EAP if partially observed

    pv0 <- attr(re0, "postVar")
    pv <- array(0, dim=c(NR,NR,ngr))
    pv[,,clus_obs] <- pv0                # pv: post. variance if partially observed
    pv[,,-clus_obs] <- vu                # pv: random effects cov. if fully unobserved

    #--- draw random effects
    u <- mice_multilevel_imputation_draw_random_effects( mu=re, Sigma=pv,
                ridge=random.effects.shrinkage )
    used_slopes_ll <- used_slopes[[ levels_id_ll ]]
    if (( length(used_slopes_ll)>0) & iter_re>0 ){
        stop("Cannot apply 'iter_re' in random slope models!\n")
    }
    z0 <- matrix(1, nrow=length(y), ncol=1 )
    if ( length(used_slopes_ll) > 0 ){
        z0 <- cbind( z0, x[, used_slopes_ll ] )
    }
    predicted_u <- rowSums( z0 * u[index_clus,1:NR,drop=FALSE] )
    return(predicted_u)
}
