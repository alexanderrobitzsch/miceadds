## File Name: mice_imputation_pls_include_quadratics.R
## File Version: 0.11


mice_imputation_pls_include_quadratics <- function( pls.quadratics,
    pls.interactions, x0, x, pls.print.progress, xs )
{
    pls.quadratics <- union( pls.quadratics, pls.interactions )
    use.quad <- unique( intersect( colnames(x0), pls.quadratics ) )
    # exclude variables from constructing quadratic terms if they only possess 2 values
    h1 <- apply( as.matrix(x0[,use.quad]), 2, FUN=function(tt){
                        length( table(tt) ) } )
    pls.quadratics <- intersect( pls.quadratics, use.quad[ h1 > 2 ] )

    if ( length( pls.quadratics ) > 0 ){
        use.quad <- unique( intersect( colnames(x0), pls.quadratics))
        x <- cbind( x, xs[, use.quad ] * xs[, use.quad ] )
        colnames(x) <- paste0("x", 1:(ncol(x)) )
        if( pls.print.progress ){
            cat("\n", paste("Created", length(use.quad),"Quadratic Terms",
                    substring( Sys.time(),1) ), "\n")
            utils::flush.console()
            cat("Quadratic terms of ", paste(use.quad,collapse=" "), "\n", sep="")
            utils::flush.console()
        }
    }
    res <- list( x=x, use.quad=use.quad )
    return(res)
}
