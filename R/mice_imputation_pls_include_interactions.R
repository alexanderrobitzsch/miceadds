## File Name: mice_imputation_pls_include_interactions.R
## File Version: 0.227

mice_imputation_pls_include_interactions <- function(pls.interactions,
    pls.print.progress, x, y, ry, type, min.int.cor, pls.maxcols,
    use_weights=FALSE, imputationWeights=NULL)
{
    use_interactions <- ! ( is.null(pls.interactions) )
    #------------ no interactions
    if ( ! use_interactions ){
        if( pls.print.progress ){
            cat("\n", paste("Created no Interactions",
                    substring( Sys.time(),1) ), "\n")
            utils::flush.console()
        }
    }

    #------------ some interactions
    if (use_interactions){
        use.int <- intersect( colnames(x), pls.interactions )
        n <- nrow(x)
        N1 <- length(use.int)
        # center x
        xs <- miceadds_weighted_centering(x=x, w=imputationWeights)

        if (N1 > 0){
            # search for interaction variables in predictorMatrix x?
            ind.int <- sort( which(  colnames(x) %in% use.int ) )
            dfr0 <- NULL
            if( pls.print.progress ){
                cat("\nCreate Interactions")
                cat("\n", "Minimal Absolute Correlation for Interactions of ")
                cat("min.int.cor=", min.int.cor, "\n\n")
            }
            N1t <- 0
            N2t <- 0
            # which interactions should not be created?
            dont.int <- which( colnames(x) %in% (names(type)[type==6]))
            # create design matrix
            cols <- setdiff( seq( 1, ncol(x) ), dont.int )
            dfr <- cbind( rep( ind.int, each=length(cols) ),
                                rep(cols, length(ind.int) ) )
            dfr <- dfr[ dfr[, 1 ] !=dfr[,2], ]
            ind <- intersect( which( dfr[, 1] %in% ind.int),
                            which(dfr[, 2] %in% ind.int  ) )
            dfr1 <- dfr[ ind, ]
            dfr <- rbind( dfr[ setdiff( seq(1, nrow(dfr)),ind), ],
                                    dfr1[ dfr1[,1]< dfr1[,2], ])
            dfr <- dfr[ order( dfr[,1] ), ]
            # create interactions
            y_ <- y[ry]
            xobs_ <- as.matrix(x[ry,])
            xall_ <- as.matrix(x)
            res <- mice_imputation_create_interactions( y_=y_, xobs_=xobs_,
                        xall_=xall_, index_int_=as.matrix(dfr),
                        min_int_cor_=min.int.cor, maxcols_=min(nrow(dfr),pls.maxcols),
                        imputationWeights=imputationWeights, ry=ry,
                        use_weights=use_weights)
            # total number of interactions
            N1t <- nrow(res$index_int)
            # retained number of interactions
            N2t <- ncol( res$xint )

            hx <- res$xint
            index_int2 <- res$index_int
            index_int2 <- index_int2[ res$allcorrs[,2]==1,, drop=FALSE]
            if ( N2t > 0 ){
                colnames(hx) <- paste0( "X", index_int2[,1], ".", index_int2[,2] )
                x <- cbind( x, hx )
            }
            if (N2t==0 ){
                res$allcorrs <- 0 * is.na(res$allcorrs )
            }
            N1t <- rowsum( 1+0*res$allcorrs[,1], res$index_int[,1] )
            N1h <- rowsum( res$allcorrs[,2], res$index_int[,1] )
            if (pls.print.progress){
                cat(" ")
                cat(paste( seq( 1, nrow(N1t)),
                colnames(x[,ind.int]),
                            "Created", N1t[,1],
                            "Interactions | Kept", N1h[,1], "Interactions ",
                            "\n") )
            }

            #***************************
            if( pls.print.progress ){
                cat("\n")
                cat(paste("Created", sum(N1t[,1]), "Interactions in Total | ",
                            substring( Sys.time(),1) ), "\n")
                utils::flush.console()
                cat("Interactions with ", paste(use.int,collapse=" "), "\n", sep="")
                cat("Kept ", N2t, " Interactions in Total \n", sep="")
                cat("  Minimal Absolute Correlation for Interactions of min.int.cor=",
                            min.int.cor, "\n")
                utils::flush.console()
            }
        }
    }

    #--- output
    res <- list( x=x, xs=xs )
    return(res)
}
