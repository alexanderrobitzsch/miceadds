## File Name: mice_ml_lmer_collect_lme4_input.R
## File Version: 0.09

mice_ml_lmer_collect_lme4_input <- function(y, x, ry, data, levels_id, NL, fml,
        lmer_family, model, lmer_args, blme_args )
{
    y1 <- y
    y1[!ry] <- NA
    dat_lme4 <- data.frame(dv._lmer=y1, x)
    for (ll in 1:NL){
        dat_lme4[, levels_id[ll] ] <- data[, levels_id[ll] ]
    }
    lmer_args <- list( formula=fml, data=dat_lme4, na.action="na.omit"  )
    if ( model=="binary"){
        lmer_args$family <- lmer_family
    }
    # apply blme arguments if provided
    lmer_args <- mice_multilevel_imputation_blme_args( lmer_args=lmer_args,
                        blme_args=blme_args )
    #--- output
    return(lmer_args)
}
