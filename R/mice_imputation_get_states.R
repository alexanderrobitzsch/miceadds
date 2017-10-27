## File Name: mice_imputation_get_states.R
## File Version: 0.03

mice_imputation_get_states <- function( pos= parent.frame(n=1) ){
	if ( is.null(pos) ){
		pos <- parent.frame()
	}
    vname <- get("vname", pos = pos ) 
    newstate <- get( "newstate" , pos = pos )  
	res <- list("vname" = vname , "newstate" = newstate)
	return(res)
}
