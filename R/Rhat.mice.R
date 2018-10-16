## File Name: Rhat.mice.R
## File Version: 1.11

Rhat.mice <- function( mice.object)
{
    chainMean <- mice.object$chainMean
    chainVar <- mice.object$chainVar
    dcM <- dim(chainMean)
    dfr <- data.frame(matrix( 0, nrow=dcM[1], ncol=4 ))
    for (vv in 1:dcM[1] ){
        dfr[vv,3] <- Rhat( chainMean[vv,,] )
        dfr[vv,4] <- Rhat( chainVar[vv,,] )
        dfr[vv,1] <- rownames(chainMean[,,1])[vv]
        dfr[vv,2] <- 100*mean( is.na( mice.object$data[, dfr[vv,1] ] ) )
    }
    colnames(dfr) <- c("variable", "MissProp", "Rhat.M.imp", "Rhat.Var.imp" )
    print(dfr )
    invisible(dfr)
}
