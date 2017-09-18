## File Name: var0.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:50

var0 <- function(x, na.rm=TRUE){
	sd1 <- stats0( x = x , FUN = stats::var , na.rm=na.rm )
	return(sd1)
}
