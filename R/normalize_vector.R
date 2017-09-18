## File Name: normalize_vector.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:50

##########################################
# normalize vector to sum of length x
normalize_vector <- function(x)
{
	x  <- length(x) * x / sum(x)
	return(x)
}
