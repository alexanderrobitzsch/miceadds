## File Name: mice_imputation_define_wy.R
## File Version: 0.01

mice_imputation_define_wy <- function(wy, ry)
{
    if (is.null(wy)){
        wy <- !ry
    }
    return(wy)
}
