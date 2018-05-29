## File Name: mice_imputation_get_states.R
## File Version: 0.23

mice_imputation_get_states <- function( pos=parent.frame(n=1), n_index=1:8 )
{
    if ( is.null(pos) ){
        pos <- parent.frame(n=1)
    }    
    #-- newstate
    newstate <- ma_exists_get(x='newstate', pos=pos, n_index=n_index )
    #-- vname
    vname <- ma_exists_get(x='yname', pos=pos, n_index=n_index )
    #*** output
    res <- list(vname=vname, newstate=newstate)
    return(res)
}

## see also
## https://github.com/stefvanbuuren/mice/blob/master/R/sampler.R
