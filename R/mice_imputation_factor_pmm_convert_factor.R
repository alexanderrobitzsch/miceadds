## File Name: mice_imputation_factor_pmm_convert_factor.R
## File Version: 0.11

mice_imputation_factor_pmm_convert_factor <- function(imp, is_factor, y_aggr)
{
    if (is_factor){
        y_factor <- y_aggr[,1]
        imp <- y_factor[ match(imp, y_aggr[,2]) ]
    }
    return(imp)
}
