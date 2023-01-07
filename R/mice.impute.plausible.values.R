## File Name: mice.impute.plausible.values.R
## File Version: 2.712

mice.impute.plausible.values <- function (y, ry, x, type, alpha=NULL,
            alpha.se=0, scale.values=NULL, sig.e.miss=1000000,
            like=NULL, theta=NULL, normal.approx=NULL,
            pviter=15, imputationWeights=rep(1, length(y)), plausible.value.print=TRUE,
            pls.facs=NULL, interactions=NULL, quadratics=NULL, extract_data=TRUE,
            control_latreg=list( progress=FALSE, ridge=1e-5 ), ...)
{
    #*******
    # old arguments which are now excluded from the function
    itemdiff <- NULL
    item.resp <- NULL
    pvirt.iter <- 30
    pvirt.burnin <- 10
    pvirt.printprogress <- FALSE

    #--- extract arguments
    # pos <- parent.frame(n=1)
    pos <- parent.frame(n=2)
    res <- mice_imputation_get_states( pos=pos )
    vname <- res$vname
    newstate <- res$newstate
    if (extract_data){
        res <- mice_imputation_prepare_2l_functions( vname=vname, envir=pos )
        y <- res$y
        x <- res$x
        ry <- res$ry
        type <- res$type
    }

    #--- define PV methods
    pvmethod <- 0
    if ( ! is.null( scale.values[[ vname ]] )){
        pvmethod <- 3
    }
    if ( ! is.null( like[[ vname ]] )){
        pvmethod <- 4
    }
    if (pvmethod==0){
        if ( ! is.null( alpha[[ vname ]] )){ pvmethod <- 1 }
        if (  is.null( alpha[[vname ]] )){ pvmethod <- 2 }
    }

    # define scale type
    scale.type <- "parallel"
    pls.facs <- mice_imputation_extract_list_arguments( micearg=pls.facs,
                    vname=vname, miceargdefault=NULL )
    interactions <- mice_imputation_extract_list_arguments( micearg=interactions,
                        vname=vname, miceargdefault=NULL )
    quadratics <- mice_imputation_extract_list_arguments( micearg=quadratics,
                        vname=vname, miceargdefault=NULL )

    #--- Plausible value imputation using tam.latreg
    #---- adapt this to include only the likelihood
    if (pvmethod==4){
        require_namespace("TAM")
        res <- include.2l.predictors_v1( y=y, x=x, ry=ry, type=type, vname=vname,
                    newstate=newstate, ... )
        X <- res$X
        #--- PLS
        if ( is.null(pls.facs) + is.null(interactions) + is.null(quadratics) < 3 ){
            plsout <- mice_imputation_pls_helper( newstate=newstate,
                        vname=vname, pls.impMethod="xplsfacs",
                        x=X[,-1], y=y, ry=rep(TRUE,length(y)),
                        imputationWeights=imputationWeights,
                        interactions=interactions, quadratics=quadratics,
                        pls.facs=pls.facs, envir_pos=pos, ... )$yimp
            # X <- plsout[,-1]
            X <- plsout
        }

        #*+*+*
        cluster <- res$cluster
        # item response data matrix
        like <- as.matrix( like[[ vname ]] )
        theta <- as.matrix( theta[[ vname ]] )
        if ( is.null( normal.approx[[ vname ]] ) ){
            normal.approx <- TRUE
        }
        X <- X[,-1,drop=FALSE]  # exclude intercept

        #-- perform latent regression
        mod0 <- TAM::tam.latreg(like=like, theta=theta, Y=X, control=control_latreg )

        #-- draw plausible values
        cat("\n")
        mod1 <- TAM::tam.pv( tamobj=mod0, normal.approx=normal.approx, nplausible=1,
                                samp.regr=TRUE )
        # extract pv imputation
        ximp <- mod1$pv[,2]
    }

    #******* Plausible value imputation with known scale scores and standard errors
    if (pvmethod==3){
        M.scale <- scale.values[[ vname ]][[ "M" ]]
        SE.scale <- scale.values[[ vname ]][[ "SE" ]]
        # compute true variance
        ind1 <- ! is.na(M.scale)
        # var.ytrue <- stats::var( M.scale[ind1], na.rm=TRUE)
        #                    - mean( (SE.scale[ ind1 ])^2, na.rm=TRUE )
        v2 <- stats::var( M.scale[ind1], na.rm=TRUE)
        var.ytrue <- v2  - stats::median( (SE.scale[ ind1 ])^2, na.rm=TRUE )
        true.var <- var.ytrue

        if (true.var < 0){
            true.var <- v2
        }
        miss <- ( is.na(M.scale) ) | ( is.na(SE.scale ) )
        M.scale[miss] <- Mscale <- mean( M.scale, na.rm=TRUE )
        SE.scale[miss] <- sig.e.miss
        # calculate initial means and variances of posterior distribution
        SE_scale_2 <- SE.scale^(-2)
        EAP <- ( SE_scale_2*M.scale + true.var^(-1)*Mscale )/(SE_scale_2 + true.var^(-1))
        Var.EAP <- 1 / ( SE_scale_2 + true.var^(-1) )
        x1 <- x
        # group mean where the actual observation is eliminated
        if ( sum( type==-2 ) > 0 ){
            x1b <- cbind( x[, type==-2 ], y )
            gm <- mice.impute.2l.groupmean.elim(y=y, ry=FALSE * ry, x=x1b, type=c(-2,1) )
            x <- x1 <- cbind( x1, gm )
            type <- c( type, 1 )
            i2 <- which( type==-2 )
            x <- x[, -i2]
            type <- type[-i2]
        }
        # group level predictors
        X <- x
        #*+*+*+*
        # PLS
        if ( is.null(pls.facs) + is.null(interactions) + is.null(quadratics) < 3 ){
            if ( is.null(interactions) ){
                interactions <- names(type)[ type==4 ]
            }
            plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname,
                    pls.impMethod="xplsfacs", x=X, y=y,
                    ry=rep(TRUE,length(y)), imputationWeights=imputationWeights,
                    interactions=interactions, quadratics=quadratics,
                    pls.facs=pls.facs,  envir_pos=pos, ... )$yimp
            X <- plsout[,-1]
        }

        #*+*+*
        ridge <- 1E-7
        xcov1 <- xcov <- X
        xcov1 <- as.matrix(xcov1)
        xcov1a <- cbind( 1, xcov1 )
        xtx <- crossprod(xcov1a)
        diag(xtx) <- diag(xtx) * (1 + ridge)
        xtx1 <- miceadds_ginv(x=xtx)

        # begin iterations for drawing plausible values
        for (iter in 1:pviter){
            # draw plausible value for individuals
            y.pv <- stats::rnorm( length(EAP), mean=EAP, sd=sqrt(Var.EAP) )
            xty <- crossprod(xcov1a, as.matrix(y.pv) )
            # calculate linear regression
            cmod <- xtx1 %*% xty
            yfitted <- xcov1a %*% cmod
            sigma2 <- mean( ( y.pv - yfitted )^2 )
            v <- sigma2 * xtx1
            diag(v) <- diag(v) + ridge
            beta.star <- as.vector(cmod) + ma_rmvnorm( n=1, mu=rep(0,nrow(v)), sigma=v)
            #-> fitted regression coefficients
            # update posterior distribution
            EAP <- ( SE_scale_2*M.scale + sigma2^(-1)*yfitted )/(SE_scale_2+sigma2^(-1))
            Var.EAP <- 1 / ( SE_scale_2 + sigma2^(-1) )
            # draw plausible value
            y.pv <- stats::rnorm( length(y),  mean=EAP, sd=sqrt(Var.EAP) )
            if ( pvirt.printprogress ){
                cat("*")
                utils::flush.console()
            }
            # add mean plausible value
            if ( sum( type==-2) ){
                x1b <- cbind( x[, type==-2 ], y.pv )
                gm <- mice.impute.2l.groupmean.elim(y=y, ry=FALSE * ry,
                                x=x1b, type=c(-2,1) )
                xcov1 <- cbind( xcov, gm )
            }
        }
        ximp <- as.vector(y.pv)
    }

    #--- PV imputation scale score according to CTT (parallel measurements)
    #---    alpha is known or unknown
    if ( pvmethod %in% c(1,2) ){
        # extract scale values
        if ( sum(type==3)==0){
            cat( "\n",paste( "Items corresponding to scale", vname,
                    "must be declared by entries of 3 in the predictor matrix"),"\n")
        }
        dat.scale <- x[, type==3, drop=FALSE ]
        x1 <- x[, type %in% c(1,2) ]
        # group level predictors
        x1 <- mice_imputation_multilevel_include_2l_predictors( y=y, x=x, ry=ry,
                            type=type, ... )
        x1 <- x1[,-1]

        #*** PLS
        if ( is.null(pls.facs) + is.null(interactions) + is.null(quadratics) < 3 ){
            ry_true <- rep(TRUE, length(y))
            plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname,
                        pls.impMethod="xplsfacs", x=x1, y=y,
                        ry=ry_true, imputationWeights=imputationWeights,
                        interactions=interactions, quadratics=quadratics,
                        pls.facs=pls.facs, envir_pos=pos,  ... )$yimp
            x1 <- plsout[,-1]
        }

        cluster <- res$cluster
        # group mean where the actual observation is eliminated
        if ( sum( type==-2 ) > 0 ){
            x1b <- cbind( x[, type==-2 ], y )
            gm <- mice.impute.2l.groupmean.elim(y=y, ry=FALSE * ry, x=x1b, type=c(-2,1))
            x1 <- cbind( x1, gm )
        }
        # compute scale score
        y1 <- rowMeans( dat.scale )

        #*** plausible value imputation if alpha is estimated or known
        if (pvmethod  %in% c(1,2) ){
            if (pvmethod==2){
                require_namespace("MBESS")
                cirel.type <- "Normal Theory"
                cir <- MBESS::ci.reliability( data=dat.scale, type=cirel.type,
                                    interval.type=TRUE )
                alpha.est <- cir$Estimated.reliability
                alpha.se <- cir$SE.reliability
            }
            if (pvmethod==1){
                alpha.known <- alpha[[vname]]
                if ( is.list(alpha.se) ){
                    alpha.se <- alpha.se[[ vname ]]
                }
                if ( is.null(alpha.se) ){
                    alpha.se <- 0
                }
                alpha.est <- alpha.known
            }
            # sampling of Cronbach's Alpha
            alpha.samp <- stats::rnorm( 1, mean=alpha.est, sd=alpha.se )
            alpha.samp <- min( .99, max( .01, alpha.samp ) )      # restriction of range
            ximp <- draw.pv.ctt( y=y1, dat.scale=dat.scale, x=x1, alpha=alpha.samp )
        }
    }

    # print progress
    if ( plausible.value.print){
        cat("\n",vname, " Plausible value imputation ")
        if (pvmethod==1){
            cat(paste("with known Cronbach's Alpha of",alpha.known,
                "and known standard error of", alpha.se, "\n")  )
        }
        if (pvmethod==2){
            cat( paste( "with estimated measurement error variance for",
                            scale.type, "items\n      "))
            cat( paste("estimated Cronbach's alpha of", round( alpha.est, 3 ) ),
                    "(SE=", round( alpha.se,3),") \n")
        }
        if (pvmethod %in% c(1,2) ){
            cat( paste("        sampled Cronbach's alpha of",
                            round( alpha.samp, 3 ) ), "\n")
        }
        if (pvmethod==3){
            cat("with known scale scores and known measurement",
                        "error standard deviations") }
        if (pvmethod==4){
            cat("using a provided likelihood")
        }
        cat("\n")
        utils::flush.console()
    }

    # return imputed values
    return(ximp)
}
