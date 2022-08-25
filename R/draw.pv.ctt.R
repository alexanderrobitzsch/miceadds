## File Name: draw.pv.ctt.R
## File Version: 3.236

draw.pv.ctt <- function( y, dat.scale=NULL, x=NULL, samp.pars=TRUE, alpha=NULL,
                    sig.e=NULL, var.e=NULL, true.var=NULL )
{
    # calculate Cronbach's Alpha if alpha==NULL
    if ( ! is.null(var.e) ){
        sig.e <- sqrt( var.e )
    }
    if (  is.null(alpha) & is.null(sig.e)  ){
        alpha <- cronbach_alpha( dat.scale )
    }

    #**** calculate scale scores if not provided
    if ( ( ! is.null( dat.scale) ) & ( is.null(y) ) ){
            y <- rowMeans( dat.scale, na.rm=TRUE )
    }

    y0 <- y
    if ( ! is.null(x) ){
        x0ind <- TRUE
        x0 <- scale( x, scale=FALSE )
        mod <- stats::lm( y0 ~ as.matrix(x0) )
    } else {
        mod <- stats::lm( y0 ~ 1 )
        x0ind <- FALSE
    }
    # determine fitted y (regression model)
    yfitted <- mod$fitted
    smod <- summary(mod)
    # calculate true variance of scale
    if ( is.null(sig.e) ){
        var.ytrue <- stats::var(y,na.rm=TRUE) * alpha
    }
    if ( ( ! is.null(sig.e) ) & ( ! is.null( true.var ) ) ){
        var.ytrue <- true.var
    }
    if ( ( ! is.null(sig.e) ) & ( is.null( true.var ) ) ){
        var.ytrue <- stats::var(y, na.rm=TRUE ) - mean( sig.e^2, na.rm=TRUE)
    }
    # calculate residual variance in the regression model
    sig2.th.y1 <-  var.ytrue * ( 1 - smod$r.squared )
    vary <- stats::var(y,na.rm=TRUE)
    sig2.th.y <-  vary * ( 1 - smod$r.squared )

    # define alpha if it is not already defined
    if ( is.null(alpha) ){
        alpha <- 1 - mean( sig.e^2, na.rm=TRUE) / vary
    }
    sig2.th.y <- max( 1E-5, sig2.th.y - vary*(1-alpha) )

    # draw new residual variance
    if ( samp.pars ){
        N <- length(y)
        sig2.th.y <- N * sig2.th.y / stats::rchisq(1, N )
    }
    # calculate measurement error variance
    if (is.null(sig.e) ){
        sig2.e <- stats::var(y) * ( 1 - alpha )
    } else {
        sig2.e <- sig.e^2
        sig2.e <- ifelse( is.na(sig2.e), 1000 * mean( sig2.e, na.rm=TRUE), sig2.e )
    }
    # calculate conditional reliability
    rho.c <- sig2.th.y / ( sig2.th.y + sig2.e )
    # draw regression parameter
    if ( samp.pars ){
        v <- stats::vcov(mod)
        m <- rep(0, nrow(v) )
        rn <- miceadds_import_CDM_CDM_rmvnorm( 1, mean=m, sigma=v )
        beta.star <- stats::coef(mod) + rn
        # draw new fitted y
        if ( x0ind ){
            yfitted <- cbind(1,x0) %*% beta.star
        } else {
            yfitted <- beta.star
        }
    }

    #--------- draw plausible values
    theta.bar <- rho.c * y0 + ( 1 - rho.c ) * yfitted
    # add random noise
    sd.pv <- sqrt(  ( 1 - rho.c ) * sig2.th.y )
    y.pv <- theta.bar + stats::rnorm( length(y), mean=0, sd=sd.pv )
    y.pv <- as.vector( y.pv )

    #--- print output
    cat("\nPlausible Value Imputation\n\n")
    h1 <- round( stats::var(y0, na.rm=TRUE), 4 )
    h2 <- round( stats::var(y.pv, na.rm=TRUE), 4 )
    cat( "Observed variance:", h1, "\n")
    cat( "Sampled PV variance:", h2, "\n")
    cat( "PV Reliability:", round(h2 / h1,4  ), "\n")
    cat( "Conditional Reliability of Y given X:",
                round( mean(rho.c, na.rm=TRUE),4  ), "\n")
    return( y.pv )
}


