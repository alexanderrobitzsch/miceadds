## File Name: mice_imputation_args_include_wy.R
## File Version: 0.03

mice_imputation_args_include_wy <- function(imp_function, args, wy)
{
    args_imp_function <- methods::formalArgs(def=imp_function)
    if ("wy" %in% args_imp_function){
        args$wy <- wy
    }
    return(args)
}
