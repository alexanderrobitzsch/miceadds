## File Name: mice_imputation_pls_largest_correlations.R
## File Version: 0.27

mice_imputation_pls_largest_correlations <- function( y, x, ry, type,
        use.ymat, pls.print.progress, x10, N.largest, min.all.cor,
        imputationWeights=NULL, eps=1e-20)
{

    # compute correlations
    if (min.all.cor>0){
        c1 <- mice_imputation_pls_correlation_criteria( y=y, x=x, ry=ry,
                        use.ymat=use.ymat, wt=imputationWeights)
        elim.ind <- which( abs(c1) < min.all.cor )
    }
    elim.ind <- NULL
    N11 <- ncol(x)

    Nelim <- length(elim.ind)
    if ( ( N11 - Nelim <=1 ) & (N11>2) ){
        elim.ind <- elim.ind[ -c(1:2) ]
    }
    if ( length(elim.ind) > 0){
        x <- x[, - elim.ind, drop=FALSE]
    }
    N12 <- ncol(x)
    if ( pls.print.progress){
        cat("\n", paste( N11, " Predictor Variables", sep="") )
        cat("\n", "Minimal Absolute Correlation of min.all.cor=", min.all.cor, "\n")
        cat( "  Kept", paste( N12, "Predictor Variables", names(y) ), "\n")
        utils::flush.console()
    }

    #***---***---***---***---***---***---***---***---
    if (N.largest>0){  # begin N.largest

        # look for largest correlations
        c1 <- mice_imputation_pls_correlation_criteria( y=y, x=x, ry=ry,
                    use.ymat=use.ymat, wt=imputationWeights)

        dfr1 <- data.frame( "index"=seq(1, ncol(x)), "abs.cor"=abs(as.vector(c1)) )
        dfr1 <- dfr1[ order( dfr1$abs.cor, decreasing=TRUE), ]
        x <- x[, dfr1[ 1:N.largest, "index" ] ]
        # look if some columns do have complete missing entries or SD of zero
        cmna1 <- which( colMeans( is.na(x))==1 )
        cmna2 <- which( apply( x, 2, stats::sd )==0 )
        cmna <- unique( sort( c( cmna1, cmna2 ) ) )

        if ( length(cmna) > 0 ){
            x <- x[, - cmna ]
            N.largest <- ncol(x)
        }
        if ( pls.print.progress){
            cat("\n", paste( N12, " Predictor Variables", sep="") )
            cat("\n", "Select Predictors with", N.largest, "Largest Correlations\n")
            flush.console()
        }
    } # end N.largest

    if ( is.vector(x) ){
        x <- cbind( x, x10[,1:2] )
    }
    if ( dim(x)[2]==0 ){
        x <-  x10[,1:2]
        n0 <- dim(x)[1]
        x[,1] <- x[,1] + stats::rnorm( n0, sd=eps )
        x[,2] <- x[,2] + stats::rnorm( n0, sd=eps )
    }
    #-- output
    res <- list( x=x, NX=ncol(x) )
    return(res)
}
