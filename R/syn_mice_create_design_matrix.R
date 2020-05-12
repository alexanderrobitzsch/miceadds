## File Name: syn_mice_create_design_matrix.R
## File Version: 0.05

syn_mice_create_design_matrix <- function(x, xp, formula=NULL)
{
    if (is.null(formula)){
        formula <- as.formula( paste0("~ ", paste0( colnames(x), collapse="+") ) )
    }
    x <- stats::model.matrix(formula, data=x)
    xp <- stats::model.matrix(formula, data=xp)
    if (colnames(x)[1]=="(Intercept)"){
        x <- x[,-1]
        xp <- xp[,-1]
    }
    colnames(xp) <- colnames(x) <- paste0("x",1:ncol(x))
    #-- output
    res <- list(x=x, xp=xp)
    return(res)
}
