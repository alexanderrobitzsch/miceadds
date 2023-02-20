## File Name: mice_imputation_pls_pca_reduction.R
## File Version: 0.147


mice_imputation_pls_pca_reduction <- function(x, pcamaxcols,
    pls.print.progress, imputationWeights=NULL, use_weights=FALSE)
{
    if (! use_weights ){
        imputationWeights <- NULL
    }
    if ( ncol(x) > pcamaxcols ){
        a0 <- Sys.time()
        NX <- nrow(x)
        xdims <- min( pcamaxcols, NX-2 )
        if( pls.print.progress ){
            cat("\nDimension reduction with Principal Components Analysis\n")
            if (pcamaxcols > 1){
                cat("Dimension of X:", NX, " -> Dimension reduction to ",
                        xdims, "dimensions\n")
            }
            if (pcamaxcols < 1){
                cat("Dimension of X:", NX, " -> Dimension reduction to ",
                        100 * pcamaxcols, "% of total variance\n")
            }
        }
        x <- as.matrix(x)
        xpca <- pca.covridge(x=x, wt=imputationWeights)
        varexpl <- xpca$sdev^2
        varexpl <- cumsum( varexpl / sum( varexpl) * 100 )
        if (pcamaxcols<1){
            xdims <- which( varexpl > 100*pcamaxcols )[1]
        } else {
            xdims <- pcamaxcols
        }
        if (pls.print.progress){
            cat( " ->", xdims, "extracted dimensions\n")
            cat("Explained variance:", round( varexpl[ xdims], 2 ), " % " )
        }
        x <- xpca$scores[, 1:xdims, drop=FALSE]
        a1 <- Sys.time()
        if (pls.print.progress){
            cat("\nTime needed:", a1-a0, "\n")
        }
    }  # PCA dimension reduction
    #--- output
    return(x)
}
