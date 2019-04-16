## File Name: mice.impute.smcfcs.R
## File Version: 0.331

mice.impute.smcfcs <- function(y, ry, x, wy=NULL, sm, dep_type="norm",
    sm_type="norm", fac_sd_proposal=1, mh_iter=20, ...)
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data

    if( ! exists(x="fac_sd_proposal_all", where=pos) ){
        assign(x="fac_sd_proposal_all", value=list(), pos=pos)
    }
    fac_sd_proposal_all <- get(x="fac_sd_proposal_all", envir=pos)
    fac_sd_proposal_temp <- fac_sd_proposal_all[[vname]]
    if (is.null(fac_sd_proposal_temp)){
        fac_sd_proposal_temp <- fac_sd_proposal
    }
    #* handle substantive model
    res <- mice_imputation_smcfcs_clean_input(x=x, y=y, state_data=state_data,
                    sm=sm, vname=vname)
    sm_vname <- res$sm_vname
    dat0 <- res$dat0
    formula_imp <- res$formula_imp

    #- draw bootstrap sample
    ind_boot <- which(ry)
    ind_boot <- sort( sample(ind_boot, length(ind_boot), replace=TRUE) )

    # estimate model parameters for imputed variable
    dat1 <- dat0[ ind_boot, ]
    rownames(dat1) <- NULL

    model1 <- mice_imputation_smcfcs_estimate_model(data=dat1, formula=formula_imp,
                        model_type=dep_type, dep=vname)
    # estimate parameters of substantive model
    model2 <- mice_imputation_smcfcs_estimate_model(data=dat1, formula=sm,
                        model_type=sm_type, dep=sm_vname)
    #- samples for values
    dat2a <- dat0[ wy,, drop=FALSE ]
    nmis <- nrow(dat2a)
    accept <- rep(0, nmis)
    for (ii in 1:mh_iter){
        ll_old <- mice_imputation_smcfcs_evaluate_loglikelihood(model1=model1,
                            model2=model2, data=dat2a)
        # sample new values
        dat2b <- mice_imputation_new_proposal(model=model1, data=dat2a, vname=vname,
                        fac_sd_proposal=fac_sd_proposal_temp)
        # evaluate new likelihood
        ll_new <- mice_imputation_smcfcs_evaluate_loglikelihood(model1=model1,
                            model2=model2, data=dat2b)
        # compute MH ratio
        ll_diff <- min(10, ll_new - ll_old)
        mh_ratio <- exp(ll_diff)
        acc <- stats::runif(nmis,0,1) < mh_ratio
        dat2a[,vname] <- ifelse( acc, dat2b[,vname], dat2a[,vname] )
        accept <- accept + acc
    }
    acc <- accept / mh_iter
    ximp <- dat2a[,vname]
    #- update Metropolis Hastings factor
    fac_sd_proposal_temp <- mice_imputation_refresh_update_factor( acc=acc,
                    fac_old=fac_sd_proposal_temp)
    fac_sd_proposal_all[[vname]] <- fac_sd_proposal_temp
    assign(x="fac_sd_proposal_all", value=fac_sd_proposal_all, pos=pos)
    return(ximp)
}
