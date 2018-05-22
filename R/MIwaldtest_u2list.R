## File Name: MIwaldtest_u2list.R
## File Version: 0.04



################################################################
# convert u into a list
MIwaldtest_u2list <- function( u )
{
    u0 <- u
    dq <- dim(u)
    NB <- dq[1]
    NV <- dq[2]
    u <- as.list(1:NB)
    for (bb in 1:NB){
        u[[bb]] <- u0[ bb,,]
    }
    return(u)
}
################################################################

