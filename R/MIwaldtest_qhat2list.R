## File Name: MIwaldtest_qhat2list.R
## File Version: 0.02



# convert qhat into a list
MIwaldtest_qhat2list <- function( qhat )
{
    qhat0 <- qhat
    dq <- dim(qhat)
    NB <- dq[1]
    NV <- dq[2]
    qhat <- as.list(1:NB)
    for (bb in 1:NB){
        qhat[[bb]] <- qhat0[ bb, ]
    }
    return(qhat)
}
