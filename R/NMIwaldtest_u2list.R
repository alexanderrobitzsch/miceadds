## File Name: NMIwaldtest_u2list.R
## File Version: 0.01



################################################################
# convert u into a list
NMIwaldtest_u2list <- function( u )
{
    u0 <- u
    dq <- dim(u)
    NB <- dq[1]
    NW <- dq[2]
    NV <- dq[3]
    u <- as.list(1:NB)
    for (bb in 1:NB){
        u1 <- as.list(1:NW)
        for (ww in 1:NW){
            u1[[ww]] <- u0[ bb, ww,,]
        }
        u[[bb]] <- u1
    }
    return(u)
}
################################################################
