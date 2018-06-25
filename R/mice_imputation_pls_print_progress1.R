## File Name: mice_imputation_pls_print_progress1.R
## File Version: 0.06

mice_imputation_pls_print_progress1 <- function( pls.print.progress, vname,
    print.dims, y, ry, x, type )
{
    if( pls.print.progress  ){
        cat("\n-------------------PLS----------------------\n",vname )
        cat(" 'mice.impute.pls'")
        if (print.dims){
            cat("\n.......... Dimensions .............")
            cat("\n dim y   ", length(y) )
            cat("\n dim ry  ", length(ry), " | sum(!ry)",  sum(!ry) )
            cat("\n dim x   ", dim(x) )
            cat("\n dim type", length(type) )
            t1 <- table(type)
            cat("\n table(type)\n"  )
            print(t1)
            cat("\n\n")
        }
    }
}

