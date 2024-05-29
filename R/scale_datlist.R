## File Name: scale_datlist.R
## File Version: 0.333


#- application of scale for a list of multiply imputed datasets,
#- single datasets or nested multiply imputed datasets
scale_datlist <- function( datlist, orig_var, trafo_var, weights=NULL,
                        M=0, SD=1, digits=NULL)
{
    require_namespace("TAM")
    is_dfr <- FALSE
    is_iL <- FALSE
    is_NIL <- FALSE
    is_ndl <- FALSE
    is_dl <- FALSE

    if ( inherits(datlist,"nestedImputationList") ){
        datlist <- nested.datlist_create( datlist )
        is_iL <- TRUE
    }

    if ( inherits(datlist,"nested.datlist") ){
        datlist <- nested.datlist2datlist(datlist)
        Nimp <- attr(datlist, "Nimp" )
        is_ndl <- TRUE
    }

    if ( inherits(datlist,"imputationList") ){
        datlist0 <- datlist
        datlist <- datlist_create( datlist )
        is_iL <- TRUE
    }

    if ( inherits(datlist,"datlist") ){
        datlist0 <- datlist
        datlist <- datlist_create( datlist )
        is_dl <- TRUE
    }

    #**** processing if datlist is a data frame
    if ( ! inherits(datlist,"datlist")  ){
        is_dfr <- TRUE
        datlist0 <- datlist
        datlist <- list( 1 )
        datlist[[1]] <- datlist0
        class(datlist) <- "datlist"
    }

    #*** create weights if needed
    PP <- length(datlist)
    if ( is.null(weights) ){
        N <- nrow(datlist[[1]])
        weights <- rep(1,N)
    }
    weights0 <- weights
    orig_var0 <- orig_var
    trafo_var0 <- trafo_var
    N0 <- length(orig_var0)
    for (nn in 1L:N0){
        orig_var <- orig_var0[nn]
        trafo_var <- trafo_var0[nn]
        #---- compute means and standard deviations
        res <- lapply( datlist, FUN=function(dd){
            # dd <- datlist[[1]]
            if ( is.character(weights0) ){
                weights <- dd[, weights0 ]
            }
            m1 <- stats::weighted.mean( dd[,orig_var], w=weights )
            sd1 <- TAM::weighted_sd( x=dd[,orig_var], w=weights )
            c(m1,sd1)
                } )
        #---- compute averaged mean and SD
        res <- matrix( unlist(res),ncol=2, byrow=TRUE )
        a1 <- colMeans(res)
        #---- create derived variable
        for (pp in 1L:PP){
            dd <- datlist[[pp]]
            dd[,trafo_var] <- M + SD * ( dd[,orig_var] - a1[1] ) / a1[2]
            if ( ! is.null(digits) ){
                dd[,trafo_var] <- round( dd[,trafo_var], digits )
            }
            datlist[[pp]] <- dd
        }
    }
    #---- output
    if ( is_dfr ){
        datlist <- datlist[[1]]
    }
    if ( is_iL ){
        datlist0$imputations <- datlist
        datlist <- datlist0
    }
    if ( is_dl ){
        datlist <- datlist_create(datlist)
    }
    if ( is_ndl ){
        datlist <- datlist2nested.datlist(datlist=datlist, Nimp=Nimp)
    }
    if ( is_NIL ){
        datlist <- NestedImputationList(datlist)
    }
    return(datlist)
}
