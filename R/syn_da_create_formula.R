## File Name: syn_da_create_formula.R
## File Version: 0.04

syn_da_create_formula <- function(wv, ord_vars)
{
    v2 <- ifelse( wv %in% ord_vars, paste0("as.factor(",wv,")"), wv )
    v2 <- paste0( v2, collapse=" + ")
    return(v2)
}
