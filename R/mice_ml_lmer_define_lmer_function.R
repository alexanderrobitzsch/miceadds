
mice_ml_lmer_define_lmer_function <- function( model, blme_use )
{	
	#*** include arguments for lmer model	
	if ( model == "binary"){	
		lmer_family <- stats::binomial(link="logit")
		if ( blme_use){
			lmer_function <- blme::bglmer						
		} else {
			lmer_function <- lme4::glmer
		}
	}
	if ( model %in% c("continuous","pmm") ){	
		lmer_family <- NULL
		if ( blme_use){
			lmer_function <- blme::blmer						
		} else {
			lmer_function <- lme4::lmer
		}		
	}	
	#--- output
	res <- list( lmer_function=lmer_function, lmer_family=lmer_family)
	return(res)	
}