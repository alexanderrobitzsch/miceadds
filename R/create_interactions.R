## File Name: create_interactions.R
## File Version: 1.02

################################################
# create interactions
create_interactions <- function (y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_){ 
    create_interactions_cpp(Yr = y_, Xr = xobs_, Xallr = xall_, 
                index_int = index_int_ , MI = min_int_cor_ , maxcols=maxcols_)    
}

    # .Call("create_interactions_cpp", 
    #    y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_, 
    #    PACKAGE = "miceadds")

    # .Call("miceadds_create_interactions_cpp", 
    #    y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_, 
    #    PACKAGE = "miceadds")

# create_interactions_cpp <- function(Yr, Xr, Xallr, index_int, MI, maxcols) {
#     .Call('miceadds_create_interactions_cpp', PACKAGE = 'miceadds', 
#                Yr, Xr, Xallr, index_int, MI, maxcols)
# }                    
