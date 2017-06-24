
##########################################
# normalize vector to sum of length x
normalize_vector <- function(x)
{
	x  <- length(x) * x / sum(x)
	return(x)
}
