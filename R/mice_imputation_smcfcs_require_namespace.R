## File Name: mice_imputation_smcfcs_require_namespace.R
## File Version: 0.03

mice_imputation_smcfcs_require_namespace <- function(model_type)
{
    #-- mdmb package
    mdmb_models <- c("bct","yjt","lognorm")
    if (model_type %in% mdmb_models ){
        requireNamespace("mdmb")
    }
}
