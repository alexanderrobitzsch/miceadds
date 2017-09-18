## File Name: mice_imputation_prepare_2l_functions.R
## File Version: 0.12
## File Last Change: 2017-02-06 11:05:49

#############################################
# This preparation function is copied as 
# part from the mice:::sampler function
mice_imputation_prepare_2l_functions <- function( vname , envir , ... )
{   
	p <- get("p" , envir = envir )
	newstate <- get("newstate" , envir = envir )
	j <- newstate$co	
	r <- get("r" , envir = envir )

	#*****************************************
	#****** start copy from mice
		# for a multilevel imputation method
		predictors <- p$predictorMatrix[j, ] != 0
		# RB: formula-based specification
		if (! is.null(p$form) && nchar(p$form[j])>0) {
			myform <- paste(p$form[j], "0", sep="+")
			x <- stats::model.matrix( stats::formula(myform), p$data)
		} else {
			x <- p$data[, predictors, drop = FALSE]
			y <- p$data[, j]
			ry <- r[, j]
			type <- p$predictorMatrix[j, predictors]
			nam <- vname	
			keep <- remove.lindep_miceadds(x, y, ry, ...)
			x <- x[, keep, drop = FALSE]
			type <- type[keep]
		}
	#****** END: copy from mice
	#*****************************************	
	res <- list( y = y , x = x , ry = ry , type = type)	
	return(res)
}
