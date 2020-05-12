## File Name: mice_imputation_get_states.R
## File Version: 0.407

mice_imputation_get_states <- function( pos=parent.frame(n=1), n_index=1:20 )
{
    if ( is.null(pos) ){
        pos <- parent.frame(n=1)
    }

    #-- newstate
    # newstate <- ma_exists_get(x='newstate', pos=pos, n_index=n_index )
    res <- ma_exists(x='newstate', pos=pos, n_index=n_index )
    newstate <- res$obj
    pos <- res$pos
    n_index <- res$n

    #-- vname
    vname <- ma_exists_get(x='yname', pos=pos, n_index=n_index )
    dep <- newstate$dep
    vname <- dep
    if (is.null(vname)){
        vname <- ma_exists_get(x='vname', pos=pos, n_index=1:20 )
        subst <- FALSE
    }

    #-- blocks
    blocks <- ma_exists_get(x='blocks', pos=pos, n_index=n_index )
    block <- blocks[[ dep ]]
    #-- data
    data <- ma_exists_get(x='data', pos=pos, n_index=n_index )

    #*** output
    res <- list(newstate=newstate, vname=vname, dep=dep, blocks=blocks, block=block,
                data=data, pos=pos, n_index=n_index)
    return(res)
}

## see also
## https://github.com/stefvanbuuren/mice/blob/master/R/sampler.R
