## File Name: max0.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:48


max0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = max , na.rm=na.rm )
	return(sd1)
}
