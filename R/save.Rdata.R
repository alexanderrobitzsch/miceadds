## File Name: save.Rdata.R
## File Version: 1.092



### save R data objects
save.Rdata <- function( dat, name, path=NULL, part.numb=1000 )
{
    if ( is.null(path) ){ path <- getwd() }
    save( dat, file=file.path( path, paste( name, ".Rdata", sep="") ) )
    M1 <- min( part.numb,nrow(dat) )
    if (M1==part.numb){
        kuerzel <- "__PART"
    } else {
        kuerzel <- ""
    }
    utils::write.csv2( dat[ seq(1, M1 ), ],
                file=file.path( path, paste( name, kuerzel, ".csv", sep="") ),
                quote=TRUE, row.names=F, na="" )
    #*** sink output
        sink( file.path( path, paste( name, "__STR.Rout", sep="") ) )
    cat( paste( Sys.time(), "\n") )
    cat( Rsessinfo(), "\n\n")
    cat( file.path( path, paste( name, ".Rdata", sep="") ), "\n\n")
    cat( utils::str( dat, list.len=ncol(dat) + 50 ))
        sink()
    dfr <- data.frame( "column"=1:(ncol(dat)), "variable"=colnames(dat) )
    utils::write.csv2( dfr, file=file.path( path,
                            paste( name, "__VARIABLES.csv", sep="") ),
                            quote=TRUE, row.names=F, na="" )
}
