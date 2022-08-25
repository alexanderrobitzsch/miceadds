## File Name: mice_ml_lmer_construct_lme4_formula.R
## File Version: 0.08

mice_ml_lmer_construct_lme4_formula <- function(x, intercept, levels_id, fixed_effects,
        NL, random_slopes )
{
    #** fixed effects
    rhs.f <- mice_multilevel_create_formula( variables=fixed_effects,
                    include_intercept=intercept )
    lmer_formula <- paste0("dv._lmer~", rhs.f )
    used_slopes <- as.list(1:NL)
    names(used_slopes) <- levels_id
    #** random effects
    for (ll in 1:NL){
        clus_id_ll <- levels_id[ll]
        variables <- random_slopes[[ clus_id_ll ]]
        used_slopes[[ clus_id_ll ]] <- variables
        rhs.r0 <- mice_multilevel_create_formula( variables=variables,
                        include_intercept=TRUE )
        rhs.r0 <- paste0( "+(", rhs.r0, "|", clus_id_ll, ")" )
        lmer_formula <- paste0( lmer_formula, rhs.r0 )
    }
    fml <- lmer_formula
    #--- output
    res <- list(fml=fml, fixed_effects=fixed_effects, used_slopes=used_slopes)
    return(res)
}
