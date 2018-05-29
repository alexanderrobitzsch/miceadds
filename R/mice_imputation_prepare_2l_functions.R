## File Name: mice_imputation_prepare_2l_functions.R
## File Version: 0.24

#############################################
# This preparation function is partly copied
# from the mice:::sampler function
mice_imputation_prepare_2l_functions <- function( vname, envir, use_formula = FALSE, ... )
{
    # p <- get("p", envir=envir )
    p_data <- ma_exists_get(x='data', pos=envir)
    p_predictorMatrix <- ma_exists_get(x='predictorMatrix', pos=envir)    
    # newstate <- get("newstate", envir=envir )
    # newstate <- ma_exists_get('newstate', pos=envir)    
    j <- ma_exists_get('j', pos=envir)
    # r <- get("r", envir=envir )
    r <- ma_exists_get('r', pos=envir)
    p_form <- NULL    
    if ( use_formula ){
        p_form <- ma_exists_get('formulas', pos=envir)
    }    
    calltype <- ma_exists_get('calltype', pos=envir)
    
    #*****************************************
    #****** start copy from mice
        # for a multilevel imputation method
        predictors <- p_predictorMatrix[j, ] !=0
        # RB: formula-based specification
        if ( calltype == "formula" ) {
            myform <- paste(p_form[j], "0", sep="+")
            x <- stats::model.matrix( stats::formula(myform), p_data)
            ry <- r[, j]
            type <- 0
        }
        if (calltype == "type" ){
            x <- p_data[, predictors, drop=FALSE]
            y <- p_data[, j]
            type <- p_predictorMatrix[j, predictors]
            nam <- vname
            ry <- r[, j]
            keep <- remove.lindep_miceadds(x, y, ry, ...)            
            x <- x[, keep, drop=FALSE]
            type <- type[keep]
        }        
    #****** END: copy from mice
    #*****************************************
    res <- list( y=y, x=x, ry=ry, type=type)
    return(res)
}
