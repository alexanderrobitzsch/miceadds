## File Name: mice_imputation_smcfcs_evaluate_loglikelihood_model.R
## File Version: 0.302

mice_imputation_smcfcs_evaluate_loglikelihood_model <- function(model, data)
{
    mod <- model$mod
    parms <- model$parms
    dep <- model$dep
    model_type <- model$model_type
    y <- data[,dep]
    eps <- 1E-30
    mice_imputation_smcfcs_require_namespace(model_type=model_type)

    #- predict
    pmod <- predict(mod, data)
    pmod <- as.vector(pmod)
    #- norm
    if (model_type=="norm"){
        ll <- stats::dnorm(x=y, mean=pmod, sd=parms$sigma, log=TRUE)
    }
    #- logistic regression
    if (model_type=="logistic"){
        probs <- stats::plogis(pmod)
        ll <- log( ifelse(y==1, probs, 1-probs) + eps )
    }
    #- bc
    if (model_type %in% c("bc","lognorm") ){
        ll <- mdmb::dbct_scaled(x=y, location=pmod, shape=model$parms$sigma,
                    lambda=model$parms$lambda, df=model$parms$df,
                    log=TRUE, check_zero=TRUE)
    }
    #- yj
    if (model_type=="yj"){
        ll <- mdmb::dyjt_scaled(x=y, location=pmod, shape=model$parms$sigma,
                    lambda=model$parms$lambda, df=Inf, log=TRUE)
    }
    ll[ is.na(ll) ] <- -Inf

    #-- output
    return(ll)
}
