## File Name: create.designMatrices.waldtest.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:47


#################################################
# create design matrices for waldtest
create.designMatrices.waldtest <- function( pars , k ){
        NP <- length(pars)
        Cdes <- matrix( 0 , nrow=k , ncol=NP)
        colnames(Cdes) <- pars
        rdes <- rep(0,k)
        res <- list( Cdes = Cdes , rdes=rdes )
        return(res)
}
######################################################
