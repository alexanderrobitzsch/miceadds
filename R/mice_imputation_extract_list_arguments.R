## File Name: mice_imputation_extract_list_arguments.R
## File Version: 0.19


#---- extract list argument
mice_imputation_extract_list_arguments <- function( micearg, vname, miceargdefault )
{
    if( is.list(micearg) ){
        if ( ! is.null(micearg[[vname]] ) ){
            micearg <- micearg[[vname]]
        } else {
            micearg <- miceargdefault
        }
    }
    if ( is.null(micearg) ){
        micearg <- miceargdefault
    }
    return(micearg)
}

#--- deprecated function
.extract.list.arguments <- mice_imputation_extract_list_arguments

