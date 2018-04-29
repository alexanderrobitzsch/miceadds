## File Name: ma.wtd.aux.data.R
## File Version: 2.11

########################################################
# auxiliary function
ma.wtd.aux.data <- function(data , weights , vars = NULL ){
    #*****
    
    is_dfr <- TRUE        # default is class data frame
    if ( is.vector(data) ){
        data <- data.frame( "Var" = data)
        is_dfr <- TRUE
    }
            
    #---- mids or mids.1chain
    if ( class(data) %in% c("mids","mids.1chain","mids.nmi") ){
        data <- mids2datlist( data )
    }        
    
    #----- NestedImputationList
    if ( class(data) == "NestedImputationList" ){
        is_dfr <- FALSE
        data <- data$imputations
        class(data) <- "nested.datlist"
    }                        
        
    #--------------------
    # conversion in case of a nested datalist    
    if ( class(data) == "nested.datlist" ){
        is_dfr <- FALSE
        data <- nesteddatlist2datlist(data)
    }                                            
                        
    #---- imputationList
    if ( class(data) == "imputationList" ){
        data <- data$imputations
        class(data) <- "datlist"
    }
    
    #--------------------
    # conversion in case of a datlist
    if ( class(data) == "datlist" ){
        is_dfr <- FALSE
        if ( ! is.null(vars) ){
            M <- length(data)
            for (ii in 1:M){
                dat0 <- data[[ii]]
                data[[ii]] <- dat0[ , vars, drop=FALSE ]        
            }
        }
    }

    #--------------------
    # conversion in case of class BIFIEdata
    if ( class(data) == "BIFIEdata" ){
        TAM::require_namespace_msg("BIFIEsurvey")
        if ( is.null(vars) ){
            vars <- data$variables
        }
        #*** use weights
        if ( is.null(weights) ){
            weights <- data$wgt
        }                        
        if ( data$cdata){
            data <- BIFIEsurvey::BIFIE.BIFIEcdata2BIFIEdata(bifieobj=data, 
                            varnames = vars )
        }
        data <- BIFIEsurvey::BIFIE.BIFIEdata2datalist(bifieobj=data, varnames = vars)            
        data <- datlist_create( data )
        M <- length(data)
        for (ii in 1:M){
            data[[ii]][ , "one"] <- NULL
        }
        attr(data,"nvars") <- ncol(data[[ii]])
        is_dfr <- FALSE        
    }
                        
    #-------------------                    
    # conversion in case of a data frame
    if ( is_dfr ){
        data0 <- data
        data0 <- as.matrix(data0)
        if ( ! is.null(vars) ){
            data0 <- data0[ , vars , drop=FALSE ]
        }            
        data <- list(1)
        data[[1]] <- data0
    }
    #-------------------
    # creation of weights (if needed)
    if ( is.null(weights) ){
        weights <- rep(1 , nrow(data[[1]] ) )
    }
    res <- list( data = data , weights = weights )
    return(res)
}
#########################################################    



