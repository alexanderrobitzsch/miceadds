## File Name: syn_da_synthesize_lm.R
## File Version: 0.192

syn_da_synthesize_lm <- function(dat2, ind0, ind1, syn_vars, da_vars, ss,
            fix_vars, ord_vars, miss, ncomp=20, use_pls=TRUE, exact_regression=TRUE,
            formula_syn=NULL, exact_marginal=TRUE)
{
    #-- create formula
    syn_vars_ss <- syn_vars[ss]
    form <- paste0( syn_vars[ss], " ~ 1")
    if (!is.null(fix_vars)){
        v2 <- syn_da_create_formula(wv=fix_vars, ord_vars=ord_vars)
        form <- paste0( form, " + ", v2 )
    }
    if (ss>1){
        v2 <- syn_da_create_formula(wv=syn_vars[1L:(ss-1)], ord_vars=ord_vars)
        form <- paste0( form, " + ", v2 )
    }
    if (! is.null(da_vars) ){
        v2 <- syn_da_create_formula(wv=da_vars, ord_vars=ord_vars)
        form <- paste0( form, " + ", v2 )
    }
    form <- as.formula(form)
    if (!is.null(formula_syn)){
        form <- formula_syn
    }
    form <- as.formula(form)

    #-- perform PLS if requested
    if (use_pls){
        res_pls <- syn_da_compute_pls_factors(dat2=dat2, ncomp=ncomp,
                        syn_vars_ss=syn_vars_ss, form=form, ind0=ind0, ind1=ind1)
        dat20 <- res_pls$dat20
        form20 <- res_pls$form20
    } else {
        dat20 <- dat2
        form20 <- paste0(" y ~ ", paste(form)[3] )
    }
    dat20$y <- dat20[,syn_vars_ss]

    #-- synthesis using regression
    mod <- stats::lm( form20, data=dat20[ ind0, ] )
    smod <- summary(mod)
    sd_resid <- sqrt( (1-smod$adj.r.squared) * stats::var(dat2[ind0,syn_vars_ss]) )
    pmod <- predict(mod, data=dat20[ind1,])
    e <- stats::rnorm( nrow(dat2)/2, sd=sd_resid )
    y1 <- dat20[ ind0, syn_vars_ss]
    dat21 <- dat20
    dat21$y <- c(e,e)
    #- force residual being uncorrelated with predictor variables
    if (exact_regression){
        mod1 <- stats::lm( form20, data=dat21[ ind1, ] )
        e <- mod1$residuals
    }
    imp_ss <- pmod + e
    dfr1 <- data.frame( orig=dat2[ind0, syn_vars_ss], syn_sim=imp_ss )

    dfr1$index <- 1L:nrow(dfr1)
    dfr1$is_miss <- miss[, syn_vars_ss]
    dfr1 <- dfr1[ order( dfr1$syn_sim), ]
    cc <- ! dfr1$is_miss
    dfr1$syn[ cc ] <- sort(y1[cc])
    cc <- dfr1$is_miss
    if (sum(cc)>0){
        dfr1$syn[ cc ] <- sort(y1[cc])
    }
    dfr1 <- dfr1[ order(dfr1$index), ]
    #-- should marginal distribution be preserved?
    if (exact_marginal){
        syn1 <- dfr1$syn
    } else {
        syn1 <- dfr1$syn_sim
    }

    dat2[ dat2$syn==1, syn_vars_ss ] <- syn1

    #-- output
    return(dat2)
}
