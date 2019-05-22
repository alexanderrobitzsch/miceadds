## File Name: datlist2nested.datlist.R
## File Version: 0.12


#** datlist -> nested.datlist
datlist2nested.datlist <- function(datlist, Nimp)
{
    CALL <- match.call()
    if (inherits(datlist,"imputationList")){
        is_imputationList <- TRUE
        datlist <- datlist$imputations
    } else {
        is_imputationList <- FALSE
    }

    PP <- Nimp[1] * Nimp[2]
    datlist1 <- as.list( 1:Nimp[1] )
    datlist2 <- as.list( 1:Nimp[2] )
    vv <- 1
    for (bb in 1:Nimp[1] ){
        for (ww in 1:Nimp[2] ){
            datlist2[[ww]] <- datlist[[vv]]
            vv <- vv + 1
        }
        datlist1[[bb]] <- datlist2
    }
    if ( ! is_imputationList){
        datlist1 <- nested.datlist_create(datlist1)
        attr(datlist1,"call") <- CALL
    } else {
        datlist1 <- NestedImputationList(datlist1)
        datlist1$call <- CALL
    }
    return(datlist1)
}
