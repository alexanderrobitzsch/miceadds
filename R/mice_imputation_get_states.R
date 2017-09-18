## File Name: mice_imputation_get_states.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:49

mice_imputation_get_states <- function( pos= parent.frame(n=1) ){
	if ( is.null(pos) ){
		pos <- parent.frame()
	}
    vname <- get("vname", pos = pos ) 
    newstate <- get( "newstate" , pos = pos )  
	res <- list("vname" = vname , "newstate" = newstate)
	return(res)
}
