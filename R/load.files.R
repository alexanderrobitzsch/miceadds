## File Name: load.files.R
## File Version: 0.13


load.files <- function(files, type=NULL, path=getwd(), ...)
{
    NF <- length(files)
    dat <- load.data( filename=files[1], type=type, path=path)
    NR <- nrow(dat)
    NC <- ncol(dat)
    i1 <- nrow(dat)
    dat0 <- matrix(NA, nrow=2*NF*NR, ncol=NC)
    colnames(dat0) <- colnames(dat)
    dat0 <- as.data.frame(dat0)
    dat0[1:i1,] <- dat
    N0 <- nrow(dat0)
    if (NF>1){
        for (ff in 2:NF){
            dat <- load.data( filename=files[ff], type=type, path=path, ...)
            nd <- nrow(dat)
            if ( i1 + nd > N0 ){
                dat00 <- dat0
                dat0 <- matrix(NA, nrow=2*N0, ncol=NC)
                colnames(dat0) <- colnames(dat00)
                dat0 <- as.data.frame(dat0)
                dat0[ 1:i1, ] <- dat00[ 1:i1, ]
            }
            dat0[ i1 + seq(1,nd), ] <- dat
            i1 <- i1 + nd
            utils::flush.console()
        }
    }
    dat0 <- dat0[ 1:i1, ]
    return(dat0)
}
