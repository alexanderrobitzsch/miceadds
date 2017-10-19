## File Name: string_extract_part.R
## File Version: 0.03
## File Last Change: 2017-09-22 15:36:22

string_extract_part <- function( vec , part=1, sep="__" )
{
	v1 <- strsplit(vec, split=sep)
	m1 <- lapply( v1 , FUN = function(ll){ ll[ part] } )
	m1 <- unlist(m1)
	return(m1)
}
