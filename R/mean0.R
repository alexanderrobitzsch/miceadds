## File Name: mean0.R
## File Version: 0.04
## File Last Change: 2017-02-06 11:05:48


mean0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = mean , na.rm=na.rm )
	return(sd1)
}
