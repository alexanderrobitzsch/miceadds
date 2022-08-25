## File Name: mice_imputation_smcfcs_evaluate_loglikelihood.R
## File Version: 0.182

mice_imputation_smcfcs_evaluate_loglikelihood <- function(model1, model2, data)
{
    if (!is.null(model1)){
        ll1 <- mice_imputation_smcfcs_evaluate_loglikelihood_model(model=model1,
                            data=data)
    } else {
        ll1 <- 0
    }
    ll2 <- mice_imputation_smcfcs_evaluate_loglikelihood_model(model=model2,
                            data=data)
    ll <- ll1 + ll2
    #-- output
    return(ll)
}
