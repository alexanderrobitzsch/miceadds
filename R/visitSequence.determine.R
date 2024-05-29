## File Name: visitSequence.determine.R
## File Version: 0.253


visitSequence.determine <- function( impMethod, vis, data, maxit=10)
{
    ## check for character input
    is_char <- FALSE
    if ( ! is.numeric(vis) ){
        is_char <- TRUE
        vis0 <- vis
        vis <- match( colnames(data), vis )
        names(vis) <- vis0
    }
    dat <- data
    ind <- grep( '~ I', impMethod )
    I1 <- length(ind)
    # inits
    vis1 <- vis0 <- vis
    # select passive variables
    pass_vars <- names(impMethod)[ind]
    iter <- 0
    while (iter < maxit ){
        vis <- vis1
        for (var.ii in pass_vars){
            vis1 <- visitSequence_determine_handle_variable( var.ii=var.ii,
                            impMethod=impMethod, vis1=vis1, dat=dat )
        }
        visit_constant <- visitSequence_determine_equal_vecs( v1=vis, v2=vis1 )
        if ( visit_constant ){
            iter <- maxit + 1
        }
        iter <- iter + 1
    }
    if (is_char){
        vis1 <- names(vis1)
    }
    return(vis1)
}
