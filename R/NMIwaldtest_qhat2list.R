## File Name: NMIwaldtest_qhat2list.R
## File Version: 0.01


#*** convert qhat into a list
NMIwaldtest_qhat2list <- function( qhat )
{
    qhat0 <- qhat
    dq <- dim(qhat)
    NB <- dq[1]
    NW <- dq[2]
    NV <- dq[3]
    qhat <- as.list(1:NB)
    parnames <- dimnames(qhat0)[[3]]
    names(qhat) <- paste0("B_index",1:NB)
    for (bb in 1:NB){
        qhat1 <- as.list(1:NW)
        names(qhat1) <- paste0("W_index",1:NW)
        for (ww in 1:NW){
            q1 <- qhat0[ bb, ww,]
            names(q1) <- parnames
            qhat1[[ww]] <- q1
        }
        qhat[[bb]] <- qhat1
    }
    return(qhat)
}
