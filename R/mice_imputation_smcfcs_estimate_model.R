## File Name: mice_imputation_smcfcs_estimate_model.R
## File Version: 0.244

mice_imputation_smcfcs_estimate_model <- function(data, formula,
    model_type="norm", dep=NULL, ...)
{
    mod <- NULL
    parms <- NULL
    mice_imputation_smcfcs_require_namespace(model_type=model_type)

    #-- normally distributed residuals (lm function)
    if (model_type=="norm"){
        mod <- stats::lm(formula=formula, data=data)
        parms <- mice_imputation_extract_parameters_lm(mod=mod)
    }
    #-- logistic regression
    if (model_type=="logistic"){
        mod <- stats::glm(formula=formula, data=data, family="binomial")
        parms <- list(beta=mod$coef)
    }
    #-- bc regression
    if (model_type %in% c("bc","lognorm") ){
        lambda_fixed <- NULL
        if (model_type=="lognorm"){
            lambda_fixed <- 0
        }
        mod <- mdmb::bct_regression(formula=formula, data=data,
                        lambda_fixed=lambda_fixed, ...)
        parms <- mice_imputation_extract_parameters_bc(mod=mod)
    }
    #-- yj regression
    if (model_type=="yj"){
        mod <- mdmb::yjt_regression(formula=formula, data=data, ...)
        parms <- mice_imputation_extract_parameters_yj(mod=mod)
    }

    #-- output
    res <- list(mod=mod, parms=parms, formula=formula, model_type=model_type,
                    dep=dep)
    return(res)
}
