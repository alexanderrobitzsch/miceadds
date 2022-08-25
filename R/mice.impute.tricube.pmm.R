## File Name: mice.impute.tricube.pmm.R
## File Version: 0.304

mice.impute.tricube.pmm <- function (y, ry, x, tricube.pmm.scale=.2,
            tricube.boot=FALSE, ...)
{
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    NM <- NULL
    x <- cbind(1, as.matrix(x))
    pos <- parent.frame()
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname

    t1 <- mice_imputation_extract_list_arguments( micearg=tricube.pmm.scale,
            vname=vname, miceargdefault=.2 )

    cat( "\n", paste( vname, "  Imputation Method tricube.pmm with scaling factor",
            t1, "\n"))
    res <- miceadds_norm_draw(y=y, ry=ry, x=x, ...)
    yhatobs <- res$yhatobs
    yhatmis <- res$yhatmis
    yobs <- res$yobs
    parm <- res

    ymean <- mean( y[ry] )
    #***
    # bootstrap
    if ( tricube.boot ){
        y1 <- y[ry]
        x1 <- x[ry,]
        B <- length(y1)
        ind <- sample( 1:B, replace=TRUE )
        parm2 <- mice_imputation_norm_draw(y=y1[ind], ry=rep(TRUE,B), x=x1[ind,], ...)
        yhatmis <- x[!ry, ] %*% parm2$beta
    }

    #*** R2 calculations
    R2.orig <- 1 - sum( ( y[ry] - yhatobs )^2 ) / sum( ( y[ry] - ymean)^2 )
    R2.samp <- 1 - sum( ( y[ry] - x[ry, ] %*% parm$beta )^2 ) / sum( ( y[ry] - ymean)^2 )
    cat( paste( "  R2 (original data): ", round(R2.orig,4), "\n"))
    cat( paste( "  R2 (sampled coefficients): ", round(R2.samp,4), "\n"))

    if ( tricube.boot ){
        R2.boot <- 1 - sum( ( x[ry, ] %*% parm2$beta - y[ry] )^2 ) /
                            sum( ( y[ry] - ymean)^2 )
        cat( paste( "  R2 (Bootstrap): ", round(R2.boot,4), "\n"))
    }
    utils::flush.console()

    #--- doing tricube pmm
    # distance function
    dg <- d0 <- d <- abs( outer( yhatmis[,1], yhatobs[,1], "-" ) )
    # look for donorset
    DM <- max(d)
    # add some small noise to create unique entries in matrix d0
    d00 <- abs(d0)
    fg1 <- min( d00[ d00 > 0 ] )
    d0 <- d0 + matrix( stats::runif( nrow(d0)*ncol(d0), 0, fg1/10000000 ),
                                ncol=ncol(d0) )
    ND <- ncol(d0)
    NY <- nrow(d0)
    # DONOR1
    rmin1 <- apply( d0, 1, min )
    d0 <- d0 + DM*( d0==matrix(rmin1, nrow=NY, ncol=ND) )
    # DONOR2
    rmin2 <- apply( d0, 1, min )
    d0 <- d0 + DM*( d0==matrix(rmin2, nrow=NY, ncol=ND) )
    # DONOR3
    rmin3 <- apply( d0, 1, min )
    d0 <- d0 + DM*( d0==matrix(rmin3, nrow=NY, ncol=ND) )
    rmd <- rowMeans(d)
    xd <- rep(1,ncol(d))
    s.tricube <- outer( t1 * rmd, xd )
    d <- d / s.tricube
    d[ d > 1 ] <- 1
    d <- ( 1 - d^3 )^3
    # redefine donorset
    eps <- 1e-6
    d <- d  + eps * ( dg==outer( rmin1, xd ) )
    d <- d  + eps * ( dg==outer( rmin2, xd ) )
    d <- d  + eps * ( dg==outer( rmin3, xd ) )
    prob.x <- d / ( rowSums(d) + eps )
    probcs <- t( sapply( seq(1,nrow(prob.x)), FUN=function(ii){
                            cumsum(prob.x[ii,]) } ) )
    probcs2 <- probcs
    # draw a random number
    RU <- stats::runif( nrow(d) )
    # write a function which calculates the index when random
    # number exceeded cumulated probability the first time
    matr <- probcs2
    xy <- outer( RU, rep( 1,ncol(matr)) )
    xy <- matr - xy
    xy[ xy < 0 ] <- 1.01
    indvec <- apply( xy, 1, which.min )
    indvec <- ifelse( indvec==0, NM, indvec )
    imp <- ( y[ry] )[ indvec ]
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)
    return(imp)
}
