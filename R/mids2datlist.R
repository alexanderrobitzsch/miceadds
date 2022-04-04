## File Name: mids2datlist.R
## File Version: 0.182

mids2datlist <- function( midsobj, X=NULL)
{
    datlist <- midsobj   # init
    if ( inherits(midsobj,"mids.1chain") ){
        midsobj <- midsobj$midsobj
    }

    #--- object of class mids
    if ( inherits(midsobj,"mids") ){
        m <- midsobj$m
        datlist <- as.list( 1:m )
        for (ii in 1:m){
                h1 <- mice::complete( midsobj, ii )
                if ( ! is.null(X) ){
                    h1 <- data.frame( X, h1)
                }
                datlist[[ii]] <- h1
        }
        class(datlist) <- "datlist"
        datlist <- datlist_create(datasets=datlist)
    }
    #--- object of class mids.nmi
    if ( inherits(midsobj,"mids.nmi") ){
        Nimp <- midsobj$Nimp
        datlist <- as.list(1:Nimp["between"])
        dat1 <- as.list(1:Nimp["within"])
        for (bb in 1:Nimp["between"]){
            datlist[[bb]] <- dat1
            for (ww in 1:Nimp["within"] ){
                h1 <- complete.mids.nmi( midsobj, action=c(bb,ww) )
                if ( ! is.null(X) ){
                    h1 <- data.frame( X, h1)
                }
                datlist[[bb]][[ww]] <- h1
            }
        }
        class(datlist) <- "nested.datlist"
        datlist <- nested.datlist_create(datlist)
    }
    return(datlist)
}
