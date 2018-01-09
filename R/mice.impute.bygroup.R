## File Name: mice.impute.bygroup.R
## File Version: 0.34

mice.impute.bygroup <- function( y , ry , x , group , 
		imputationFunction, ... )
{
				
	#--- extract arguments
	pos <- parent.frame()
	res <- mice_imputation_get_states(pos = pos)
	vname <- res$vname	
	
	# imputation function
	imputationFunction_vname <- mice_imputation_extract_list_arguments( 
				micearg = imputationFunction, vname=vname , miceargdefault = "norm" )		
	# group variable	
	group_vname <- mice_imputation_extract_list_arguments( 
				micearg = group, vname=vname , miceargdefault = "" )	

	l2_imp_fct <- substring(imputationFunction_vname,1,2) == "2l"
				
	#*** full data frame with indices and all groups
	dfr_index <- data.frame( "y" = y, "ry" = ry, "group_" = x[ , group_vname] )	
	groups <- unique( dfr_index$group_ )
	G <- length(groups)
	# remove grouping variable from set of predictors
	vars1 <- setdiff( colnames(x) , group_vname )
	if (l2_imp_fct){
		res <- mice_imputation_prepare_2l_functions( vname = vname , envir = pos )		
		y <- res$y
		x <- res$x
		ry <- res$ry
		type <- res$type
		vars1 <- setdiff( colnames(x) , group_vname )
		type <- type[ vars1 ]
	}	
	x <- x[ , vars1, drop=FALSE ]		
	
	for (gg in 1:G){
		group_gg <- groups[gg]
		ind_gg <- which( dfr_index$group_ == group_gg )
		#-- argument list for imputations
		args <- list( y = y[ind_gg] , ry = ry[ind_gg] , x = x[ind_gg,,drop=FALSE], ... )		
		if (l2_imp_fct){
			args$type <- type
		}			
		res <- mice_impute_bygroup_modify_arguments(args=args, ind_gg=ind_gg, 
					imputationFunction_vname=imputationFunction_vname)	
		args <- res$args
		Nmis <- res$Nmis
		
		imp_function <- paste0("mice.impute." , imputationFunction_vname )
		if (Nmis > 0){
			ximp <- do.call( what=imp_function , args=args )	
			ind0_gg <- which( ! dfr_index$ry )
			ind0_gg <- intersect( ind_gg , ind0_gg )
			dfr_index[ ind0_gg , "y"] <- as.vector(ximp)
		}
	}
	return( dfr_index[ ! ry , "y"] )
}

#	res <- mice_imputation_prepare_2l_functions( vname = vname , 
#					envir = parent.frame(n=1) )
