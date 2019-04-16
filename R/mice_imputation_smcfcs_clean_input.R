## File Name: mice_imputation_smcfcs_clean_input.R
## File Version: 0.04

mice_imputation_smcfcs_clean_input <- function(x, y, state_data, sm, vname)
{
    sm_lhs <- all.vars(stats::update(sm, .~0))
    if ( ! ( sm_lhs %in% colnames(x) ) ){
        x <- data.frame(new_=state_data[,sm_lhs],x)
        colnames(x)[1] <- sm_lhs
    }
    dat0 <- data.frame( vname_=y, x)
    colnames(dat0)[1] <- vname
    x <- cbind(1,x[, colnames(x) !=sm_lhs, drop=FALSE ])
    formula_imp <- as.formula(paste0(vname, " ~ ", paste0(colnames(x), collapse=" + ") ))
    #--- output
    res <- list(sm_vname=sm_lhs, dat0=dat0, formula_imp=formula_imp)
    return(res)
}
