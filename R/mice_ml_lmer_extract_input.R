## File Name: mice_ml_lmer_extract_input.R
## File Version: 0.09
## File Last Change: 2017-08-24 09:57:36

mice_ml_lmer_extract_input <- function(pos, levels_id, random_slopes, variables_levels,
		pls.facs, min.int.cor, min.all.cor, interactions, quadratics, model )
{
	res <- mice_imputation_get_states( pos = pos )				
	vname <- res$vname
	imp.temp <- res$newstate	
	p <- get("p" , envir = pos )
	type <- p$predictorMatrix[ vname , ]
	data <- p$data	
	#--- level identifiers
	if ( is.list( levels_id ) ){
		levels_id <- levels_id[[ vname ]]
	}
	#--- random slopes	
	levi <- intersect( levels_id , names(random_slopes) )	
	if ( ! is.null(levi) ){
		random_slopes <- random_slopes[[ vname ]]
	}		
	if ( ! is.null(random_slopes) ){
		NL <- length(random_slopes)
		nrs <- names(random_slopes)
		for (ll in 1:NL){
			level_ll <- nrs[ll]
			random_slopes[[ level_ll ]] <- setdiff( random_slopes[[ level_ll ]] , vname )
		}	
	}	
	#--- level of a variable
	vname_level <- variables_levels[ vname ]

    # extract PLS factors
    pls.facs <- mice_imputation_extract_list_arguments( micearg = pls.facs, vname = vname , 
						miceargdefault = 20 )
	# correlations interactions						
    min.int.cor <- mice_imputation_extract_list_arguments( micearg = min.int.cor , vname=vname , 
                                miceargdefault = 0 ) 
    min.all.cor <- mice_imputation_extract_list_arguments( micearg = min.all.cor , vname=vname , 
                                miceargdefault = 0 ) 
								
	#--- interactions
	interactions <- mice_imputation_extract_arguments_list(v1=interactions, vname=vname)
	
	#--- quadratics
	quadratics <- mice_imputation_extract_arguments_list(v1=quadratics, vname=vname)

	#--- model
    model <- mice_imputation_extract_list_arguments( micearg = model , vname=vname , 
                                miceargdefault = "continuous" ) 
	
	#--- output
	res <- list( vname=vname, p=p, type=type, data=data, levels_id=levels_id,
					random_slopes=random_slopes, imp.temp=imp.temp,
					vname_level=vname_level, pls.facs=pls.facs, min.int.cor=min.int.cor, 
					min.all.cor=min.all.cor, interactions=interactions, quadratics=quadratics,
					model=model )
	return(res)	
}	
	
