## File Name: micombine.cor.R
## File Version: 0.599


#-- inference for correlations | nested multiply and multiply imputed datasets
micombine.cor <- function( mi.res, variables=NULL,
        conf.level=.95, method="pearson", nested=FALSE, partial=NULL )
{
    if ( inherits(mi.res,"data.frame") ){
        mi.res <- list( mi.res )
    }
    if ( inherits(mi.res,"nested.datlist") ){
        nested <- TRUE
    }
    if (! nested ){
        mi.list <- datlist_create(mi.res)
    }
    if (nested ){
        mi.list <- nested.datlist_create(mi.res)
    }
    Nimp <- attr( mi.list, "Nimp")
    N <- attr( mi.list, "nobs_datasets")
    vars <- attr(mi.list, "variables")
    if (is.null(variables)){
        variables <- vars
    }

    VV <- length(variables)
    N_partial <- 0

    if (!is.null(partial)){
        if (! inherits(partial,"formula") ){
            partial <- as.formula( paste0(" ~ ", paste0( partial, collapse="+")))
        }
    }

    # check if variables are given in character form
    if (is.character(variables)){
        if ( ! nested ){
            variables <- which( vars %in% variables )
        }
        if ( nested ){
            variables <- which( vars %in% variables )
        }
    }
    dfr <- NULL
    for ( i in 1:(VV-1) ){
        for (j in (i+1):VV){
            if (i !=j ){
                ii <- variables[i]
                jj <- variables[j]
                if ( i !=j){
                    # calculate correlation coefficients
                    if ( ! nested ){
                        cor.ii.jj <- unlist( lapply( mi.list, FUN=function(dat){
                                dat_ii <- dat[,ii]
                                dat_jj <- dat[,jj]
                            if ( ! is.null(partial) ){
                                fm <- paste0( "dat_ii ", paste( partial, collapse=" ") )
                                mod_ii <- stats::lm( as.formula(fm), data=dat )
                                rii <- resid(mod_ii)
                                mii <- as.numeric(names( mod_ii$residuals))
                                dat_ii <- NA*dat_ii
                                dat_ii[ mii ] <- rii
                                fm <- paste0( "dat_jj ", paste( partial, collapse=" ") )
                                mod_jj <- stats::lm( as.formula(fm), data=dat )
                                rjj <- resid(mod_jj)
                                mjj <- as.numeric(names( mod_jj$residuals))
                                dat_jj <- NA*dat_jj
                                dat_jj[ mjj ] <- rjj
                                N_partial <- length( coef(mod_jj) ) - 1
                            }
                            stats::cor( dat_ii, dat_jj, method=method,
                                    use="pairwise.complete.obs" )
                        } ) )
                    }
                    if (nested){
                        cor.ii.jj <- lapply( mi.list, FUN=function(mm){
                                lapply( mm, FUN=function(dat){
                                    if ( ! is.null(partial) ){
                                        fm <- paste0( "dat_ii ", paste( partial,
                                                        collapse=" ") )
                                        mod_ii <- stats::lm( as.formula(fm), data=dat )
                                        rii <- resid(mod_ii)
                                        mii <- as.numeric(names( mod_ii$residuals))
                                        dat_ii <- NA*dat_ii
                                        dat_ii[ mii ] <- rii
                                        fm <- paste0( "dat_jj ", paste( partial,
                                                            collapse=" ") )
                                        mod_jj <- stats::lm( as.formula(fm), data=dat )
                                        rjj <- resid(mod_jj)
                                        mjj <- as.numeric(names( mod_jj$residuals))
                                        dat_jj <- NA*dat_jj
                                        dat_jj[ mjj ] <- rjj
                                        N_partial <- length( coef(mod_jj) ) - 1
                                                }
                                dat_ii <- dat[,ii]
                                dat_jj <- dat[,jj]
                                stats::cor( dat_ii, dat_jj, method=method,
                                        use="pairwise.complete.obs")
                                            } ) } )
                    }
                    res.ii.jj <- micombine_cor_compute( cor.list=cor.ii.jj, N=N,
                                        conf.level=conf.level, nested=nested,
                                        Nimp=Nimp, N_partial=N_partial)
                    dfr <- rbind( dfr, c( ii, jj, res.ii.jj ) )
                }
            }
        }
    }
    dfr1 <- dfr
    dfr <- rbind( dfr, dfr1[, c(2,1,seq(3,ncol(dfr) )) ] )
    dfr <- data.frame( "variable1"=vars[ dfr[,1] ],
                "variable2"=vars[ dfr[,2] ], dfr[, -c(1:2) ] )
    #*** define attributes
    class(dfr) <- "data.frame"
    m1 <- vector2matrix( index1=dfr$variable1, index2=dfr$variable2,
                    val=dfr$r, empty_val=1 )
    attr(dfr,"r_matrix") <- m1
    m1 <- vector2matrix( index1=dfr$variable1, index2=dfr$variable2,
                    val=dfr$rse, empty_val=NA )
    attr(dfr,"rse_matrix") <- m1
    return(dfr)
}
