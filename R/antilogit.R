## File Name: antilogit.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:47

antilogit <- function(p){
	return( 1 / ( 1 + exp(-p) ) )
}
