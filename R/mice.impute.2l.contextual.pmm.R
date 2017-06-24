mice.impute.2l.contextual.pmm <- function (y, ry, x, type , imputationWeights = NULL , 
                interactions=NULL , quadratics = NULL , ...){
	res <- mice_imputation_get_states( pos = parent.frame(n=1) )	
	vname <- res$vname
	newstate <- res$newstate
	# data preparation
	xcov <- .a2l.contextual.auxiliary( y = y  , ry=ry , x=x , type=type , ...)
	#------
	# pmm imputation at level 2
	ximp <- mice.impute.weighted.pmm( y= y , ry=ry, x = xcov , 
				imputationWeights = imputationWeights , 
				interactions= interactions , quadratics = quadratics ,   ... ) 
	return(ximp)
}
