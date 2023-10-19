## File Name: mice.impute.2l.pmm.R
## File Version: 0.276

mice.impute.2l.pmm <- function(y, ry, x, type, intercept=TRUE,
            groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1E-6,
            glmer.warnings=TRUE, donors=5, match_sampled_pars=TRUE,
            blme_use=FALSE, blme_args=NULL, ...)
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    #- do imputation with lmer() function from lme4 package
    imp <- mice_imputation_2l_lmer( y=y, ry=ry, x=x, type=type, intercept=intercept,
                groupcenter.slope=groupcenter.slope, draw.fixed=draw.fixed,
                random.effects.shrinkage=random.effects.shrinkage,
                glmer.warnings=glmer.warnings, model='pmm', donors=donors,
                match_sampled_pars=match_sampled_pars,
                blme_use=blme_use, blme_args=blme_args, ... )
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(imp)
}
