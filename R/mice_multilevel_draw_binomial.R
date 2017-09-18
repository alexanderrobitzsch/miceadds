## File Name: mice_multilevel_draw_binomial.R
## File Version: 0.04
## File Last Change: 2017-02-06 11:05:49

########################################################################
# draw binomial data
mice_multilevel_draw_binomial <- function( probs ){	
	N <- length(probs)
	rn <- stats::runif(N, 0, 1)
	res <- 1*(rn < probs)
	return(res)
}
