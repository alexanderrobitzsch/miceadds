## File Name: create_interactions.R
## File Version: 1.09


# create interactions
create_interactions <- function (y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_)
{
    res <- miceadds_rcpp_create_interactions(Yr=y_, Xr=xobs_, Xallr=xall_,
                index_int=index_int_, MI=min_int_cor_, maxcols=maxcols_,
                use_weights=FALSE, weights_obs=0)
    return(res)
}

