## File Name: antilogit.R
## File Version: 0.02

antilogit <- function(p){
	return( 1 / ( 1 + exp(-p) ) )
}
