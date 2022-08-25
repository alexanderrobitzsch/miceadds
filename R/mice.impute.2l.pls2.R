## File Name: mice.impute.2l.pls2.R
## File Version: 3.341

mice.impute.2l.pls2 <- function(y, ry, x, type, pls.facs=NULL,
            pls.impMethod="pmm", pls.print.progress=TRUE,
            imputationWeights=rep( 1,length(y) ), pcamaxcols=1E9,
            tricube.pmm.scale=NULL, min.int.cor=0, min.all.cor=0, N.largest=0,
            pls.title=NULL, print.dims=TRUE, pls.maxcols=5000,
            envir_pos=parent.frame(), ... )
{
    time1 <- Sys.time()
    n <- NULL
    imputationWeights  <- nrow(x) * imputationWeights / sum(imputationWeights)
    imp.temp <- ma_exists_get(x='newstate', pos=envir_pos)
    vname <- imp.temp$dep

    # extract PLS factors
    pls.facs <- mice_imputation_extract_list_arguments( micearg=pls.facs,
                    vname=vname, miceargdefault=20 )
    # extract PLS imputation method
    pls.impMethod <- mice_imputation_extract_list_arguments( micearg=pls.impMethod,
                    vname=vname, miceargdefault="pmm" )
    # extract scaling factor for scaling factor in tricube weighted estimation
    tricube.pmm.scale <- mice_imputation_extract_list_arguments(
                                micearg=tricube.pmm.scale,
                                vname=vname, miceargdefault=.2 )
    # define minimal correlation for interactions
    min.int.cor <- mice_imputation_extract_list_arguments( micearg=min.int.cor,
                        vname=vname, miceargdefault=0 )

    if( pls.print.progress  ){
        cat("\n-------------------PLS----------------------\n",vname )
        cat(" 'mice.impute.2l.pls2'")
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

    # include predictor variables with type !=0
    nt <- names(type)[ type !=0 ]
    nt <- intersect( nt, colnames(x) )
    x10 <- x <- x[, nt]
    use.ymat <- ( ! is.null( dim(y) ) )
    x <- as.matrix(x)

    # standardize x
    if ( stats::sd(imputationWeights) > 0){ # with weights
        iW <- outer( imputationWeights, rep(1,ncol(x) ) )
        Mx <- colSums( iW * x )  / colSums( iW )
        SDx <- sqrt( colSums( iW * x^2 ) / colSums(iW) - Mx^2  )
        x0 <- x <- ( x - outer( rep(1,n), t(Mx) ) ) / outer( rep(1,n), t(SDx) )
    } else {  # without weights
        x0 <- x <- ma.scale2( x=x )
    }

    #***** include cluster effect: group mean (eliminating the subject under study)
    if ( sum(type==-2) ){
        x1 <- cbind( y, x[, which( type==-2) ] )
        type1 <- c( 1,-2)
        ximp <- mice.impute.2l.groupmean.elim( y=y, ry=ry, x=x1, type=type1 )
        x <- as.matrix( cbind( x, ximp ) )
        colnames(x)[ ncol(x) ] <- "y_aggr"
        x10 <- x0 <- x
    }
    type <- c( type, 1 )
    N <- ncol(x)

    if( pls.print.progress  ){
        cat(" Imputation: ", imp.temp$im, ", Iteration:", imp.temp$it   )
        if ( ! is.null(pls.title)){ cat("\n ",pls.title) }
        cat( "\n\nImputation using Partial Least Squares Regression\n")
        cat( substring(Sys.time(),1),"\n" )
        utils::flush.console()
        cat( "\n", paste( ncol(x10), "Predictor Variables", names(y) ), "\n")
        cat("Used Variables ", paste(colnames(x10),collapse=" "), "\n", sep="" )
    }
    # extract interactions and quadratic terms
    pls.interactions <- names(type)[ type==4 ]
    pls.quadratics <- names(type)[ type==5 ]

    #--- create no interactions
    if ( is.null(pls.interactions) ){
        if( pls.print.progress ){
            cat("\n", paste("Created no Interactions", substring( Sys.time(),1) ), "\n")
            utils::flush.console()
        }
    } else { # create some interactions
        use.int <- intersect( colnames(x), pls.interactions  )
        N1 <- length(use.int)
        # standardize x
        cx <- colMeans( x )
        xs <- x - outer( rep(1,nrow(x)), cx )
        if (N1 > 0){
            # search for interaction variables in predictorMatrix x?
            ind.int <- sort( which(  colnames(x) %in% use.int ) )
            dfr0 <- NULL
            if( pls.print.progress ){
                cat("\nCreate Interactions")
                cat("\n", "Minimal Absolute Correlation for Interactions of min.int.cor=",
                            min.int.cor, "\n\n")
            }
            N1t <- 0 ; N2t <- 0
            # which interactions should not be created?
            dont.int <- which( colnames(x) %in% ( names(type)[ type==6 ] )  )
            # create design matrix
            cols <- setdiff( seq( 1,ncol(x) ), dont.int )
            dfr <- cbind( rep( ind.int, each=length(cols) ),
                                rep(cols, length(ind.int) ) )
            dfr <- dfr[ dfr[, 1 ] !=dfr[,2], ]
            ind <- intersect( which(dfr[, 1] %in% ind.int), which(dfr[,2] %in% ind.int ))
            dfr1 <- dfr[ ind, ]
            dfr <- rbind( dfr[ setdiff( seq(1,nrow(dfr)),ind), ],
                                dfr1[ dfr1[,1]< dfr1[,2], ])
            dfr <- dfr[ order( dfr[,1] ), ]

            # create interactions
            res <- create_interactions( y_=y[ry], xobs_=as.matrix( x[ry,] ),
                            xall_=as.matrix(x),index_int_=as.matrix(dfr),
                        min_int_cor_=min.int.cor, maxcols_=min(nrow(dfr),pls.maxcols) )
            r1 <- res$allcorrs
            r1[ is.na( r1[,1] ), 1] <- 0
            res$allcorrs <- r1

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
                cat(paste( seq( 1, nrow(N1t)), colnames(x[,ind.int]),
                            "Created", N1t[,1], "Interactions | Kept", N1h[,1],
                            "Interactions ", "\n") )
            }
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

    #---- create quadratic terms
    pls.quadratics <- union( pls.quadratics, pls.interactions )
    use.quad <- unique( intersect( colnames(x0),  pls.quadratics ) )
    # exclude variables from constructing quadratic terms if they only possess 2 values
    if ( length(use.quad)>0 ){
        h1 <- apply( as.matrix(x0[,use.quad]), 2, FUN=function(tt){ length( table(tt))})
        pls.quadratics <- intersect( pls.quadratics, use.quad[ h1 > 2 ] )
    }

    if ( length( pls.quadratics ) > 0 ){
        use.quad <- unique( intersect( colnames(x0),  pls.quadratics  )    )
        # use standardized variables for creating quadratic terms
        x <- cbind( x, xs[, use.quad ] * xs[, use.quad ] )
        colnames(x) <- paste("x", 1:(ncol(x)), sep="")
        if( pls.print.progress ){
            cat("\n", paste("Created", length(use.quad),"Quadratic Terms",
                                substring( Sys.time(),1) ), "\n") ; flush.console()
            cat("Quadratic terms of ", paste(use.quad,collapse=" "), "\n", sep="")
            utils::flush.console()
        }
    }

    #--- look for minimal absolute correlations for all variables
    if ( ! use.ymat ){
        c1 <- stats::cor( y[ry], x[ry,] )
    } else {
        # look for correlations of all the dummy variables
        c1 <- stats::cor( y[ry,], x[ry,] )
        c1 <- apply( abs(c1), 2, mean, na.rm=TRUE )
    }
    elim.ind <- which( abs(c1) < min.all.cor )
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

    # look for largest correlations
    if ( ! use.ymat ){
        c1 <- stats::cor( y[ry], x[ry,] )
    } else {
        # look for correlations of all the dummy variables
        c1 <- stats::cor( y[ry,], x[ry,] )
        c1 <- apply( abs(c1), 2, mean, na.rm=T )
    }


    if (N.largest>0){  # begin N.largest
        dfr1 <- data.frame( "index"=seq( 1, ncol(x) ), "abs.cor"=abs(as.vector(c1)) )
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
            utils::flush.console()
        }
    } # end N.largest

    if ( is.vector(x) ){
        x <- cbind( x, x10[,1:2] )
    }
    dim_x <- dim(x)
    dim_x10 <- dim(x10)
    if (is.null(dim_x10)){ dim_x10 <- c(length(y),0) }
    if ( ( dim_x[2]==0 ) & ( dim_x10[2]>=2) ){
        x <- x10[,1:2]
        eps <- 1e-20
        x[,1] <- x[,1] + stats::rnorm( dim(x)[1], sd=eps )
        x[,2] <- x[,2] + stats::rnorm( dim(x)[1], sd=eps )
    }

    if ( ncol(x) > pcamaxcols ){
        a0 <- Sys.time()
        xdims <- min( pcamaxcols, nrow(x)-2 )
        cat("\nDimension reduction with Principal Components Analysis\n")
        if (pcamaxcols > 1){
            cat("Dimension of X:", ncol(x), " -> Dimension reduction to ",
                        xdims, "dimensions\n")
        }
        if (pcamaxcols < 1){
            cat("Dimension of X:", ncol(x), " -> Dimension reduction to ",
                                    100 * pcamaxcols, "% of total variance\n")
        }

        xpca <- pca.covridge(x=as.matrix(x))
        varexpl <- xpca$sdev^2
        varexpl <- cumsum( varexpl / sum( varexpl) * 100 )
        xdims <- which( varexpl > 100*pcamaxcols )[1]
        cat( " ->", xdims, "extracted dimensions\n")
        cat("Explained variance:", round( varexpl[ xdims], 2 ), " % " )
        x <- xpca$scores[, 1:xdims, drop=FALSE]
        a1 <- Sys.time()
        cat("\nTime needed:", a1-a0, "\n")
    }

    x10 <- x    # copy data set of predictors

    #--- calculate partial least squares regression
    nfac <- min(pls.facs,ncol(x))
    yobs <- y[ ry ]
    if (use.ymat){
        yobs <- y[ry,]
    }
    # center y obs
    weight.obs <- imputationWeights[ ry ]
    weight.obs <- length(weight.obs) / sum(weight.obs) * weight.obs
    yobs <- yobs - stats::weighted.mean( x=yobs, w=weight.obs )
    xobs <- x[ ry, ]

    if( stats::sd(weight.obs) > 0){
        yobs <- weight.obs * yobs
        xobs <- outer( weight.obs, rep(1,ncol(xobs) ) ) * xobs
    }
    if( pls.print.progress ){
        cat( "\n", paste( ncol(xobs), " Dimensions", sep="")  )
        cat( "\n", paste( nfac, " PLS factors are used", sep="") )
        utils::flush.console()
        if ( pls.facs==0){
            cat( "\n", "All", ncol(x),
                        "predictors are used (no PLS dimension reduction)")
        }
        cat("\n\n" )
    }
    if (pls.facs > 0){
        if ( ! use.ymat ){
            VV <- ncol(xobs)
        } else {
            VV <- ncol(xobs)
        }
        mod <- kernelpls.fit2( X=as.matrix(xobs), Y=matrix(yobs,ncol=1),ncomp=nfac)
        if( pls.print.progress ){
            print( round( 100*mod$R2, 2 ))
        }
        dfr2 <- x
        pmod <- predict.kernelpls.fit2( mod, X=as.matrix(x) )
        x <- cbind(1, as.matrix(pmod))
        x11a <- x
        if( pls.print.progress ){
            cat( "\nPLS estimation finished ", substring(Sys.time(),1),"\n" )
            utils::flush.console()
        }
    }
    if ( pls.facs==0){
        x <- cbind( 1, x )
    }

    #--- estimate linear regression
    if (pls.impMethod !="xplsfacs" ){
        if ( stats::sd( imputationWeights) > 0  ){
            # if there exists a real sample weight vector
            x <- cbind(1, as.matrix(x))
            xobs <- x[ry,]
            yobs <- y[ry]
            weights.obs <- imputationWeights[ ry ]
            weights.obs <- length(weights.obs) * weights.obs / sum( weights.obs )
            parm <- mice_imputation_weighted_norm_draw( yobs=yobs, xobs=xobs,
                                ry=ry, y=y, x=x, weights.obs=weights.obs, ... )
        } else {
                parm <- mice_imputation_norm_draw( y=y, ry=ry, x=x )
        }
    }

    #--- do Imputation
    if( pls.print.progress  ){
        cat( "\n", paste( "Imputation Method ", pls.impMethod, sep=""), "\n" )
        if( pls.impMethod=="tricube.pmm" ){
            cat( paste( "  scaling factor", tricube.pmm.scale, "\n"))
        }
    }

    #*** imputation method 'norm' (mice package)
    if (pls.impMethod=="norm" ){
        x1 <- x[ ! ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma
    }

    #*** imputation method 'pmm' (mice package)
    if (pls.impMethod=="pmm" ){
        yhatobs <- x[ry, ] %*% parm$coef
        yhatmis <- x[!ry, ] %*% parm$beta
        x1 <- apply(as.array(yhatmis), 1, mice::.pmm.match, yhat=yhatobs, y=y[ry], ... )
    }

    #*** imputation method 'pmm5' (miceadds package)
    if (pls.impMethod=="pmm5" ){
        x1 <- mice.impute.pmm5( y=y, ry=ry, x=x,  ...)
    }

    #*** imputation method 'tricube.pmm'
    if (pls.impMethod=="tricube.pmm" ){
        x1 <- mice.impute.tricube.pmm(y=y, ry=ry, x=x,
                        tricube.pmm.scale=tricube.pmm.scale )
    }
    if (pls.impMethod=="xplsfacs" ){
        x1 <- x
    }
    time2 <- Sys.time()
    if( pls.print.progress ){
        cat( "\nMissing Data Draws finished ", substring(Sys.time(),1),"\n" )
        cat( "Time elapsed:", print(time2 - time1 ), "|", time1, "|", time2 )
        cat( "\n...................PLS....................................\n")
        utils::flush.console()
    }
    return(x1)
}


#---- auxiliary function for PLS imputation
.aux.pls.imputation <- function( newstate, vname, pls.impMethod, x, y, ry,
                imputationWeights=rep( 1, length(y)),
                interactions, quadratics, pls.facs, ... )
{
    # interactions and quadratic terms
    interactions <- .extract.list.arguments( micearg=interactions,
                        vname=vname, miceargdefault=NULL )
    interactions <- intersect( interactions, colnames(x))
    quadratics <- .extract.list.arguments( micearg=quadratics,
                        vname=vname, miceargdefault=NULL )
    quadratics <- setdiff( intersect( quadratics, colnames(x)), interactions )
    if ( is.vector(x) ){ x <- matrix( x, ncol=1 ) }
    # define variable type
    type <- rep( 1, ncol(x)  )
    names(type) <- colnames(x)
    pls.use <- FALSE
    if ( length(interactions)>0 ){
        type[ interactions ] <- 4
        pls.use <- TRUE
    }
    if ( length(quadratics)>0 ){
        type[ quadratics ] <- 5
        pls.use <- TRUE
    }
    if ( pls.use & is.null(pls.facs) ){ pls.facs <- 10000 }

    #-- PLS imputation if specified
    pls.facs <- .extract.list.arguments( micearg=pls.facs,
                        vname=vname, miceargdefault=NULL )
    if ( ! is.null(pls.facs) ){
        yimp <- mice.impute.2l.pls(y=y, ry=ry, x=x, type=type, pls.facs=pls.facs,
                        pls.impMethod=pls.impMethod,
                        imputationWeights=imputationWeights, ... )
    } else {
        yimp <- NULL
    }
    res <- list( yimp=yimp, pls.facs=pls.facs )
    return(res)
}
