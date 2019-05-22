## File Name: nested.datlist2datlist.R
## File Version: 0.11


#- converts a nested list of multiply imputed datasets into a list of
#- multiply imputed datasets
nested.datlist2datlist <- function(datlist)
{
    CALL <- match.call()
    if (inherits(datlist,"NestedImputationList")){
        is_NestedImputationList <- TRUE
        datlist <- datlist$imputations
    } else {
        is_NestedImputationList <- FALSE
    }
    Nimp <- c( length(datlist), length(datlist[[1]] ) )
    names(Nimp) <- c("Between", "Within")
    PP <- Nimp[1] * Nimp[2]
    datlist0 <- as.list( 1:PP )
    vv <- 1
    for (bb in 1:Nimp[1] ){
        for (ww in 1:Nimp[2] ){
            datlist0[[vv]] <- datlist[[bb]][[ww]]
            vv <- vv + 1
        }
    }
    if (! is_NestedImputationList ){
        datlist0 <- datlist_create(datlist0)
        attr(datlist0,"call") <- CALL
    } else {
        datlist0 <- miceadds_import_mitools_imputationList(datlist0)
        datlist0$call <- CALL
    }
    return(datlist0)
}


