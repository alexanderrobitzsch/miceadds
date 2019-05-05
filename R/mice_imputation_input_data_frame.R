## File Name: mice_imputation_input_data_frame.R
## File Version: 0.04


mice_imputation_input_data_frame <- function(x, y, vname, trafo=NULL)
{
    res <- mice_imputation_smcfcs_clean_input(x=x, y=y, state_data=NULL,
                    sm=NULL, vname=vname, trafo=trafo)
    return(res)
}
