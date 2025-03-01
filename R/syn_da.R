## File Name: syn_da.R
## File Version: 0.221


syn_da <- function(dat, syn_vars=NULL, fix_vars=NULL, ord_vars=NULL,
            da_noise=0.5, formula_syn=NULL, use_pls=TRUE, ncomp=20,
            exact_regression=TRUE, exact_marginal=TRUE, imp_maxit=5)
{
    dat0 <- dat
    if (! is.null(fix_vars) ){
        syn_vars <- setdiff( syn_vars, fix_vars)
    }
    NSV <- length(syn_vars)

    N0 <- nrow(dat0)
    ind0 <- 1L:N0
    ind1 <- N0 + ind0

    #- missing indicator matrix
    miss <- is.na(dat)

    #-- setting for synthetic datasets
    da_noise <- syn_da_extend_vector(x=da_noise, n=NSV, names=syn_vars)
    use_pls <- syn_da_extend_vector(x=use_pls, n=NSV, names=syn_vars)
    ncomp <- syn_da_extend_vector(x=ncomp, n=NSV, names=syn_vars)
    exact_regression <- syn_da_extend_vector(x=exact_regression, n=NSV, names=syn_vars)
    exact_marginal <- syn_da_extend_vector(x=exact_marginal, n=NSV, names=syn_vars)
    formula_syn <- syn_da_extend_list(formula=formula_syn, syn_vars=syn_vars)

    #- impute data if necessary
    sel_vars <- union(fix_vars, syn_vars)
    imp0 <- mice::mice(dat0[,sel_vars], m=1, maxit=imp_maxit, remove.constant=FALSE,
                        remove.collinear=FALSE)

    #-- include noise variables in the dataset
    dat1 <- mice::complete(imp0, action=1)
    if (sum(da_noise)>0){
        eps <- 1e-99
        var1 <- apply( dat1[,syn_vars], 2, stats::var, na.rm=TRUE)
        sd_noise <- sqrt( var1*(da_noise+eps)/(1-da_noise+eps) )
        da_vars <- NULL
        eps1 <- 1e-4
        for (ss in 1L:NSV){
            if (da_noise[ss]>eps1){
                x <- dat1[,syn_vars[ss]]
                e <- stats::rnorm(length(x), mean=0, sd=sd_noise[ss])
                dat1[, paste0( syn_vars[ss], "_DA")] <- x + e
                da_vars <- c( da_vars, paste0(syn_vars[ss], "_DA") )
            }
        }
    } else {
        da_vars <- NULL
    }
    dat1 <- data.frame(index=1L:nrow(dat1), syn=0, dat1 )

    #* define long dataset that includes original data and synthetic data
    dat2 <- dat1
    dat2$syn <- 1
    dat2[, syn_vars ] <- NA
    dat2 <- rbind(dat1, dat2)

    #--- loop over variables for variable synthesis
    for (ss in 1L:NSV){
        dat2 <- syn_da_synthesize_lm(dat2=dat2, ind0=ind0, ind1=ind1, syn_vars=syn_vars,
                    da_vars=da_vars, ss=ss, fix_vars=fix_vars, ord_vars=ord_vars,
                    ncomp=ncomp[ss], use_pls=use_pls[ss],
                    exact_regression=exact_regression[ss],
                    exact_marginal=exact_marginal[ss],
                    formula_syn=formula_syn[[ syn_vars[ss] ]], miss=miss)
        cat( paste0( "synthesize variable ", ss, " of ", NSV, "\n") )
        utils::flush.console()
    }

    #-- rearrange original dataset
    dat_syn <- dat2[ dat2$syn==1, ]
    dat_syn <- dat_syn[, ! ( colnames(dat_syn) %in% da_vars ) ]
    vars <- intersect( colnames(dat), colnames(dat_syn) )
    dat_syn <- dat_syn[, vars ]
    miss_vars <- intersect(colnames(miss), colnames(dat_syn))
    for (mm in miss_vars){
        if (sum(miss[,mm]>0)){
            dat_syn[ miss[,mm], mm ] <- NA
        }
    }

    #--- output list
    res <- list(dat_syn=dat_syn, dat2=dat2, syn_vars=syn_vars, ord_vars=ord_vars,
                    fix_vars=fix_vars, sd_noise=sd_noise, da_noise=da_noise,
                    ncomp=ncomp, use_pls=use_pls, exact_regression=exact_regression,
                    exact_marginal=exact_marginal)
    return(res)
}
