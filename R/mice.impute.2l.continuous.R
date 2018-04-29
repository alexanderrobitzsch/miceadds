## File Name: mice.impute.2l.continuous.R
## File Version: 0.05

mice.impute.2l.continuous <- function(y, ry, x, type, intercept=TRUE,
                                  groupcenter.slope=FALSE, draw.fixed=TRUE,
                                  random.effects.shrinkage=1E-6,
                                  glmer.warnings=TRUE, 
                                  blme_use = FALSE , blme_args = NULL ,                                    
                                  ...)
{
    imp <- mice_impute_2l_lmer(y=y, ry=ry, x=x, type=type, 
                intercept=intercept, groupcenter.slope=groupcenter.slope, 
                draw.fixed=draw.fixed,
                random.effects.shrinkage=random.effects.shrinkage,
                glmer.warnings=glmer.warnings, 
                blme_use = FALSE , blme_args = NULL , 
                model = "continuous" , ...)
    return(imp)
}
