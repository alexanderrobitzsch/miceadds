## File Name: prop_miss.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:50


prop_miss <- function(x){
	x <- is.na(x)
	sd1 <- stats0( x = x , FUN = mean )
	return(sd1)
}
