## File Name: NMIcombine_include_dimnames.R
## File Version: 0.05

NMIcombine_include_dimnames <- function( names_vars, NB, NW, dims=3)
{
    dimnames_qhat <- as.list(1:dims)
    dimnames_qhat[[3]] <- names_vars
    if (dims==4){
        dimnames_qhat[[4]] <- names_vars
    }
    dimnames_qhat[[1]] <- paste0("Between_Imp", 1:NB )
    dimnames_qhat[[2]] <- paste0("Within_Imp", 1:NW )
    return(dimnames_qhat)
}
