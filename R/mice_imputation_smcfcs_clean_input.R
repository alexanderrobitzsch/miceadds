## File Name: mice_imputation_smcfcs_clean_input.R
## File Version: 0.14

mice_imputation_smcfcs_clean_input <- function(x, y, state_data, sm, vname,
    trafo=NULL)
{
    sm_lhs <- ""
    if (!is.null(sm)){
        sm_lhs <- all.vars(stats::update(sm, .~0))
        if ( ! ( sm_lhs %in% colnames(x) ) ){
            x <- data.frame(new_=state_data[,sm_lhs],x)
            colnames(x)[1] <- sm_lhs
        }
    }
    #- apply transformation
    y <- mice_imputation_transformation(x=y, trafo_fun=trafo)
    #- create data frame
    dat0 <- data.frame( vname_=y, x)
    colnames(dat0)[1] <- vname
    x <- x[, colnames(x) !=sm_lhs, drop=FALSE ]
    formula_imp <- as.formula(paste0(vname, " ~ ", paste0(colnames(x), collapse=" + ") ))
    x <- cbind(1,x)
    #--- output
    res <- list(sm_vname=sm_lhs, dat0=dat0, formula_imp=formula_imp)
    return(res)
}
