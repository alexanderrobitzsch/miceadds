
antilogit <- function(p){
	return( 1 / ( 1 + exp(-p) ) )
}
