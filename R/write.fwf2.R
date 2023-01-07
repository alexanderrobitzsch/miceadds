## File Name: write.fwf2.R
## File Version: 2.09


write.fwf2 <- function( dat, format.full, format.round, file )
{
    savename <- file
    if (is.null( colnames(dat) ) ){
        colnames(dat) <- paste( "V", 1:( ncol(dat) ), sep="")
    }
    matr <- matrix( " ", nrow=nrow(dat), ncol=ncol(dat) )
    ind1 <- which( format.round <=0  )
    format.full[ ind1 ] <- format.full[ind1]
    format.round[ ind1 ] <- format.round[ind1]
    I <- ncol(matr)
    for (vv in 1:I ){
        fvv <- format.round[vv]
        fff <- format.full[vv]
        v1 <- write_fwf2_format2( vec1=dat[,vv], ff=fff, fr=fvv )
        matr[,vv] <- v1
    }
    matr1 <- matr[,1]
    for (ii in 2:I){
        matr1 <- paste0( matr1, matr[,ii] )
    }
    matr <- matr1
    if ( length( grep( "\\.", savename, fixed=FALSE) ) > 0 ){
        filename <- savename
    } else {
        filename <- paste( savename, ".dat", sep="")
    }
    utils::write.table( matr, file=filename, quote=FALSE, row.names=FALSE,
                            col.names=FALSE, sep="")
    dfr <- data.frame( "variable"=colnames(dat),
                    "begin"=c( 1, cumsum( format.full )[ - ncol(dat) ] + 1 ),
                    "end"=cumsum( format.full ),  "length"=format.full )
    utils::write.table( dfr, paste( savename, "__LEGEND.txt",sep=""),
                quote=FALSE, row.names=FALSE, col.names=TRUE)
    return(dfr)
}

