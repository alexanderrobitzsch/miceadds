## File Name: max0.R
## File Version: 0.03


max0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = max , na.rm=na.rm )
	return(sd1)
}
