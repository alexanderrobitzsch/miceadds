## File Name: ma_wtd_stat_prepare_data.R
## File Version: 2.312


#--- auxiliary function for weighted statistics
ma_wtd_stat_prepare_data <- function(data, weights, vars=NULL )
{
    is_dfr <- TRUE        # default is class data frame
    if ( is.vector(data) ){
        data <- data.frame("Var"=data)
        is_dfr <- TRUE
    }

    #---- mids or mids.1chain
    if ( ma_inherits_or(x=data, what=c("mids","mids.1chain","mids.nmi")) ){
        data <- mids2datlist(midsobj=data)
    }

    #----- NestedImputationList
    if ( inherits(x=data, what="NestedImputationList") ){
        is_dfr <- FALSE
        data <- data$imputations
        class(data) <- "nested.datlist"
    }

    #--------------------
    # conversion in case of a nested datalist
    if ( inherits(x=data, what="nested.datlist") ){
        is_dfr <- FALSE
        data <- nesteddatlist2datlist(datlist=data)
    }

    #---- imputationList
    if ( inherits(x=data, what="imputationList") ){
        data <- data$imputations
        class(data) <- "datlist"
    }

    #---- conversion in case of a datlist
    if ( inherits(x=data, what="datlist") ){
        is_dfr <- FALSE
        if ( ! is.null(vars) ){
            M <- length(data)
            for (ii in 1:M){
                dat0 <- data[[ii]]
                data[[ii]] <- dat0[, vars, drop=FALSE ]
            }
        }
    }

    #---- conversion in case of class BIFIEdata
    if ( inherits(x=data, what="BIFIEdata") ){
        require_namespace("BIFIEsurvey")
        if ( is.null(vars) ){
            vars <- data$variables
        }
        if ( data$cdata){
            data <- BIFIEsurvey::BIFIE.BIFIEcdata2BIFIEdata(bifieobj=data,
                            varnames=vars )
        }
        data <- BIFIEsurvey::BIFIE.BIFIEdata2datalist(bifieobj=data, varnames=vars)
        data <- datlist_create(datasets=data)
        M <- length(data)
        for (ii in 1:M){
            data[[ii]][, "one"] <- NULL
            data_ii <- data[[ii]]
            data[[ii]] <- data_ii[, vars, drop=FALSE]
        }
        attr(data,"nvars") <- ncol(data[[ii]])
        is_dfr <- FALSE
    }

    #-------------------
    # conversion in case of a data frame
    if (is_dfr){
        data0 <- data
        data0 <- as.matrix(data0)
        if ( ! is.null(vars) ){
            data0 <- data0[, vars, drop=FALSE ]
        }
        data <- list(1)
        data[[1]] <- data0
    }
    #-------------------
    # creation of weights (if needed)
    if ( is.null(weights) ){
        weights <- rep(1, nrow(data[[1]]) )
    }
    res <- list( data=data, weights=weights )
    return(res)
}


ma.wtd.aux.data <- ma_wtd_stat_prepare_data
