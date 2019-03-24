## File Name: mice_imputation_extract_arguments_list.R
## File Version: 0.06

mice_imputation_extract_arguments_list <- function(v1, vname)
{
    if ( is.list(v1) ){
        v1 <- v1[[ vname ]]
    }
    v1 <- setdiff(v1, vname)
    return(v1)
}
