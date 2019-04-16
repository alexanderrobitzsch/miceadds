## File Name: mice_imputation_smcfcs_evaluate_loglikelihood.R
## File Version: 0.16

mice_imputation_smcfcs_evaluate_loglikelihood <- function(model1, model2, data)
{
    ll1 <- mice_imputation_smcfcs_evaluate_loglikelihood_model(model=model1, data=data)
    ll2 <- mice_imputation_smcfcs_evaluate_loglikelihood_model(model=model2, data=data)
    ll <- ll1 + ll2
    #-- output
    return(ll)
}
