## File Name: min0.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:49


min0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = min , na.rm=na.rm )
	return(sd1)
}
