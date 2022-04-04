## File Name: micombine.cov.R
## File Version: 1.201

#--- inference for correlations | nested multiply and multiply imputed datasets
micombine.cov <- function( mi.res, variables=NULL,
        conf.level=.95,  nested=FALSE )
{

    if (inherits(mi.res,"data.frame") ){
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
    N <- attr( mi.list, "nobs")
    vars <- attr(mi.list, "variables")
    if (is.null(variables)){
        variables <- vars
    }
    VV <- length(variables)
    # check if variables are given in character form
    if (is.character(variables)){
        if ( ! nested ){
            variables <- which( vars %in%  variables )
        }
        if ( nested ){
            variables <- which( vars %in%  variables )
        }
    }
    dfr <- NULL
    for ( i in 1:VV ){
        for (j in i:VV){
            ii <- variables[i]
            jj <- variables[j]
            # calculate correlation coefficients
            if ( ! nested ){
                cor.ii.jj <- lapply( mi.list, FUN=function(dat){
                        covTest(dat[,ii], dat[,jj], conf.level=conf.level)
                        } )
            }
            if ( nested){
                cor.ii.jj <- lapply( mi.list, FUN=function(mm){
                        lapply( mm, FUN=function(dat){
                        covTest(dat[,ii], dat[,jj], conf.level=conf.level)
                        } ) } )
                }
                res.ii.jj <- micombine_cov_compute( cor.list=cor.ii.jj, N=N,
                                conf.level=conf.level, nested=nested,
                                Nimp=Nimp )
                dfr <- rbind( dfr, c( ii, jj, res.ii.jj ) )
            }
        }
    dfr1 <- dfr
    dfr <- rbind( dfr, dfr1[, c(2,1,seq(3,ncol(dfr) )) ] )
    dfr <- data.frame( "variable1"=vars[ dfr[,1] ],
            "variable2"=vars[ dfr[,2] ], dfr[, -c(1:2) ] )
    #*** remove duplicated rows
    ind <- which( duplicated( paste( dfr$variable1, "_", dfr$variable2 ) ) )
    dfr <- dfr[ - ind, ]
    #*** define attributes
    class(dfr) <- "data.frame"
    m1 <- vector2matrix( index1=dfr$variable1, index2=dfr$variable2,
            val=dfr$cov, empty_val=NA )
    attr(dfr,"cov_matrix") <- m1
    m1 <- vector2matrix( index1=dfr$variable1, index2=dfr$variable2,
            val=dfr$cov_se, empty_val=NA )
    attr(dfr,"covse_matrix") <- m1
    return(dfr)
}


