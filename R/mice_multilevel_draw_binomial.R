
########################################################################
# draw binomial data
mice_multilevel_draw_binomial <- function( probs ){	
	N <- length(probs)
	rn <- stats::runif(N, 0, 1)
	res <- 1*(rn < probs)
	return(res)
}
