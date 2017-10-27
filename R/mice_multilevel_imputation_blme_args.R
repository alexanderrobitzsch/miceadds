## File Name: mice_multilevel_imputation_blme_args.R
## File Version: 0.02

mice_multilevel_imputation_blme_args <- function(lmer_args , blme_args){
	if ( ! is.null( blme_args) ){
		NL <- length(blme_args)
		for (nn in 1:NL){
			name_nn <- names(blme_args)[nn]
			lmer_args[[ name_nn ]] <- blme_args[[ nn ]]	
		}
	}
	return(lmer_args)
}
