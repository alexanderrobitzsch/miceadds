## File Name: mice.impute.2l.latentgroupmean.mcmc.R
## File Version: 3.352

mice.impute.2l.latentgroupmean.mcmc <- function (y, ry, x, type,
                    pls.facs=NULL, imputationWeights=NULL,
                    interactions=NULL, quadratics=NULL,
                    mcmc.burnin=100, mcmc.adapt=100, mcmc.iter=1000,
                    draw.fixed=TRUE, EAP=FALSE, ...)
{
    require_namespace("MCMCglmm")
    # retrieve mice objects
    k <- ma_exists_get("k", pos=parent.frame())           # iteration
    i <- ma_exists_get("i", pos=parent.frame())           # imputation
    vname <- ma_exists_get("vname", pos=parent.frame())   # variable name

    # create list for storing state (first call)
    if( !exists("storeState.latentgroupmean.MCMC", where=parent.frame()) ){
        assign("storeState.latentgroupmean.MCMC", list(), pos=parent.frame())
    }

    # retrieve state for current variable/imputation
    storeState.latentgroupmean.MCMC <- get("storeState.latentgroupmean.MCMC",
                                            envir=parent.frame())

    if(k > 1){
        burnin.iter <- mcmc.adapt
        current.state <- storeState.latentgroupmean.MCMC[[vname]][[i]]
    } else {
        burnin.iter <- mcmc.burnin
        current.state <- NULL
    }

    # ***
    # run imputation
    #

    # latent group mean
    cluster <- as.numeric( x[, type==- 2] )
    covariates <- as.matrix( x[, type==1 ] )
    colnames(covariates) <- colnames(x)[ type==1 ]
    y <- x[, type==2 ]

    # distinguish cases with and without covariates
    if ( sum( type==1 ) > 0 ){

        # aggregate covariates
        cov2 <- cbind( cluster, covariates )
        covaggr <- mice.impute.2l.groupmean( y, ry, x=cov2,
                        type=c(-2, rep(1,ncol(covariates) ) ),
                        grmeanwarning=FALSE )
        colnames(covaggr) <- colnames(covariates)
        # aggregation at level 2
        covaggr.l2 <- stats::aggregate( covaggr, list( cluster ), mean )
        colnames(covaggr.l2)[-1] <- colnames(covaggr)
        y.l2 <- stats::aggregate( y, list( cluster ), mean, na.rm=T )
        h1 <- as.matrix(covaggr.l2[,-1] )
        colnames(h1) <- colnames(covaggr)

        # PLS interactions and quadratics
        newstate <- get( "newstate", pos=parent.frame() )
        vname <- get("vname", pos=parent.frame()) # get variable name
        plsout <- mice_imputation_pls_helper( newstate=newstate, vname=vname,
                        pls.impMethod="xplsfacs", x=h1, y=y.l2[,2],
                        ry=( ! is.na(y.l2[,2] )),
                        imputationWeights=rep( 1, nrow(covaggr.l2)),
                        interactions=interactions, quadratics=quadratics,
                        pls.facs=pls.facs,  ... )

        # imputation PLS
        if( ! is.null( plsout$yimp ) ){
            covaggr.l2r <- as.matrix(plsout$yimp[,-1])
            covaggr <- as.matrix( covaggr.l2r[ match( cluster, covaggr.l2[,1] ), ] )
        }

        # model input
        rownames(covaggr) <- NULL # prevent warning
        mcmcdf <- data.frame(y,cluster,covaggr)
        covnames <- paste0("x",1:ncol(covaggr))
        colnames(mcmcdf) <- c("y","cluster",covnames)
        mcmcfml <- stats::as.formula(paste0(c("y~1",covnames),collapse="+"))

    } else {

        # (empty) model input
        mcmcdf <- data.frame(y=y,cluster=cluster)
        mcmcfml <- y~1

    }

    # inverse-gamma prior IG(.001,.001)
    prior <- list( R=list(V=1, nu=.002),
                    G=list(G1=list(V=1, nu=.002)))

    mod <- MCMCglmm::MCMCglmm(mcmcfml, random=~cluster, data=mcmcdf, thin=1,
                prior=prior, burnin=burnin.iter, nitt=burnin.iter + mcmc.iter,
                start=current.state, verbose=FALSE)

    # store current state in parent frame
    new.state <- list( R=mod$VCV[mcmc.iter, 2, drop=FALSE],
                        G=mod$VCV[mcmc.iter, 1, drop=FALSE],
                        liab=NULL, QUASI=TRUE )
    # overwrite previous iteration state
    storeState.latentgroupmean.MCMC[[vname]][[i]] <- new.state
    assign("storeState.latentgroupmean.MCMC", storeState.latentgroupmean.MCMC,
                                pos=parent.frame())

    # parameter estimates (post. draw by default, EAP otherwise)
    if (draw.fixed){
        beta <- mod$Sol[mcmc.iter,]
        psi2 <- mod$VCV[mcmc.iter,1]
        sig2 <- mod$VCV[mcmc.iter,2]
    } else {
        beta <- apply(mod$Sol,2,mean)
        psi2 <- mean(mod$VCV[,1])
        sig2 <- mean(mod$VCV[,2])
    }
    modf <- stats::model.matrix(mcmcfml, data=mcmcdf) %*% beta

    # aggregate: cluster size, fixed prediction, residual from fixed
    a1 <- stats::aggregate( cbind( 1, modf, y-modf ), list( cluster), sum )
    a1[,3] <- a1[,3]/a1[,2] # average from fixed
    a1[,4] <- a1[,4]/a1[,2] # average deviation from fixed
    a1[,5] <- ( psi2 / (psi2+sig2/a1[,2]) ) * a1[,4] # EAPs of random effects
    a1[,6] <- psi2*sig2/(sig2 + psi2*a1[,2])

    # match cluster indices
    ind <- match( cluster, a1[,1] )
    ximp <- stats::rnorm( nrow(a1), mean=a1[,3]+a1[,5],
                sd=(1-EAP)*sqrt(a1[,6]) )[ ind ]
    return(ximp)
}
