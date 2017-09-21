## File Name: string_extract_part.R
## File Version: 0.02
## File Last Change: 2017-09-20 16:07:37

string_extract_part <- function( vec , part=1, sep="__" )
{
	v1 <- strsplit(vec, split=sep)
	m1 <- lapply( v1 , FUN = function(ll){
					r0 <- ll[ part] 
				} )
	m1 <- unlist(m1)
	return(m1)
}
