## File Name: mice.impute.bygroup.R
## File Version: 0.10

mice.impute.bygroup <- function( y , ry , x , group , 
		imputationFunction , ... ){
				
	#--- extract arguments
	pos <- parent.frame()
	vname <- mice_imputation_get_states(pos = pos)$vname	
	# imputation function
	imputationFunction_vname <- mice_imputation_extract_list_arguments( 
				micearg = imputationFunction, vname , miceargdefault = "norm" )		
	# group variable	
	group_vname <- mice_imputation_extract_list_arguments( 
				micearg = group, vname , miceargdefault = "" )	
	
	#*** full data frame with indices and all groups
	dfr_index <- data.frame( "y" = y, "ry" = ry, "group" = x[ , group_vname] )	
	groups <- unique( dfr_index$group )
	G <- length(groups)
	# remove grouping variable from set of predictors
	vars1 <- setdiff( colnames(x) , group_vname )
	x <- x[ , vars1 ]		
	for (gg in 1:G){
		# gg <- 1
		group_gg <- groups[gg]
		ind_gg <- which( dfr_index$group == group_gg )
		#-- argument list for imputations
		args <- list( y = y[ind_gg] , ry = ry[ind_gg] , x = x[ind_gg,], ... )
		Nmis <- sum( args$ry )
		imp_function <- paste0("mice.impute." , imputationFunction_vname )
		if (Nmis > 0){
			ximp <- do.call( imp_function , args )	
			ind0_gg <- which( ! dfr_index$ry )
			ind0_gg <- intersect( ind_gg , ind0_gg )
			dfr_index[ ind0_gg , "y"] <- as.vector(ximp)
		}
	}
	return( dfr_index[ ! ry , "y"] )
}

#	res <- mice_imputation_prepare_2l_functions( vname = vname , 
#					envir = parent.frame(n=1) )
