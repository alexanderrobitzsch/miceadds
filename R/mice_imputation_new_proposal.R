## File Name: mice_imputation_new_proposal.R
## File Version: 0.15

mice_imputation_new_proposal <- function(model, data, vname, fac_sd_proposal)
{
    sigma <- model$parms$sigma
    nmis <- nrow(data)
    x0 <- data[, vname]
    mice_imputation_smcfcs_require_namespace(model_type=model$model_type)

    #- norm
    if (model$model_type=="norm"){
        x1 <- x0 + stats::rnorm(nmis, mean=0, sd=fac_sd_proposal*sigma)
    }
    #- logistic
    if (model$model_type=="logistic"){
        pmod <- predict(model$mod, data) + rnorm(nmis, mean=0, sd=2*sqrt(3.29))
        probs <- stats::plogis(pmod)
        x1 <- 1 * ( stats::runif(nmis) < probs )
    }
    #- Box-Cox
    if (model$model_type %in% c("bc","lognorm") ){
        lambda <- model$parms$lambda
        x0 <- mdmb::bc_trafo(y=x0, lambda=lambda)
        x1 <- x0 + stats::rnorm(nmis, mean=0, sd=fac_sd_proposal*sigma)
        x1 <- mdmb::bc_antitrafo(y=x1, lambda=lambda)
    }
    #- Yeo-Johnson
    if (model$model_type=="yj"){
        lambda <- model$parms$lambda
        x0 <- mdmb::yj_trafo(y=x0, lambda=lambda)
        x1 <- x0 + stats::rnorm(nmis, mean=0, sd=fac_sd_proposal*sigma)
        x1 <- mdmb::yj_antitrafo(y=x1, lambda=lambda)
    }
    #- include proposed values
    data[,vname] <- x1
    #-- output
    return(data)
}
