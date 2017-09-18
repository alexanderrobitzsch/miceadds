## File Name: mice.impute.2l.continuous.R
## File Version: 0.04
## File Last Change: 2017-02-06 11:05:48

mice.impute.2l.continuous <- function(y, ry, x, type, intercept=TRUE,
                                  groupcenter.slope=FALSE, draw.fixed=TRUE,
                                  random.effects.shrinkage=1E-6,
                                  glmer.warnings=TRUE, 
								  blme_use = FALSE , blme_args = NULL , 								   
                                  ...){

	imp <- mice.impute.2l.lmer(y=y, ry=ry, x=x, type=type, 
				intercept=intercept, groupcenter.slope=groupcenter.slope, 
				draw.fixed=draw.fixed,
				random.effects.shrinkage=random.effects.shrinkage,
                glmer.warnings=glmer.warnings, 
				blme_use = FALSE , blme_args = NULL , 
				model = "continuous" , ...)
	return(imp)
}
