## File Name: prop_miss.R
## File Version: 0.02


prop_miss <- function(x){
	x <- is.na(x)
	sd1 <- stats0( x = x , FUN = mean )
	return(sd1)
}
