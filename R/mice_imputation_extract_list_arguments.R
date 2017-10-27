## File Name: mice_imputation_extract_list_arguments.R
## File Version: 0.13



#*********************************************************************************
# extract list argument
mice_imputation_extract_list_arguments <- function( micearg , vname , miceargdefault ){
    # micearg   ... name of mice argument
    # vname     ... variable name
    # miceargdefault    ... default for this variable
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
    return( micearg )
}
#*****************************************************************************

#--- deprecated function
.extract.list.arguments <- mice_imputation_extract_list_arguments

