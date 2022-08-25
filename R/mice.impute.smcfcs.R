## File Name: mice.impute.smcfcs.R
## File Version: 0.365

mice.impute.smcfcs <- function(y, ry, x, wy=NULL, sm, dep_type="norm",
    sm_type="norm", fac_sd_proposal=1, mh_iter=20, ...)
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    pos <- res$pos
    state_data <- res$data

    #- collect arguments
    dep_type <- mice_imputation_extract_list_arguments( dep_type, vname, "norm")
    sm_type <- mice_imputation_extract_list_arguments( sm_type, vname, "norm")
    mh_iter <- mice_imputation_extract_list_arguments( mh_iter, vname, 20)
    sm <- mice_imputation_extract_list_arguments( sm, vname, NULL)

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

    #- draw bootstrap sample of dataset
    dat1 <- mice_imputation_smcfcs_draw_bootstrap_sample(data=dat0, ry=ry)

    #- estimate model for variable to be imputed
    model1 <- mice_imputation_smcfcs_estimate_model(data=dat1, formula=formula_imp,
                        model_type=dep_type, dep=vname)
    if (vname==sm_vname){
        model1 <- NULL
    }

    # estimate parameters of substantive model
    model2 <- mice_imputation_smcfcs_estimate_model(data=dat1, formula=sm,
                        model_type=sm_type, dep=sm_vname)

    #- samples for values
    dat2a <- dat0[ wy,, drop=FALSE ]
    nmis <- nrow(dat2a)
    accept <- rep(0, nmis)

    #- define model for proposal
    if (vname==sm_vname){
        model_proposal <- model2
    } else {
        model_proposal <- model1
    }

    for (ii in 1:mh_iter){
        ll_old <- mice_imputation_smcfcs_evaluate_loglikelihood(model1=model1,
                            model2=model2, data=dat2a)

        # sample new values
        dat2b <- mice_imputation_new_proposal(model=model_proposal, data=dat2a,
                        vname=vname, fac_sd_proposal=fac_sd_proposal_temp)

        # evaluate new likelihood
        ll_new <- mice_imputation_smcfcs_evaluate_loglikelihood(model1=model1,
                            model2=model2, data=dat2b)

        # compute MH ratio
        ll_diff <- ll_new - ll_old
        ll_diff <- ifelse(ll_diff>10, 10, ll_diff)
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
