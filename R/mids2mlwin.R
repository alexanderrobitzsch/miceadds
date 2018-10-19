## File Name: mids2mlwin.R
## File Version: 0.14

mids2mlwin <- function(imp, file.prefix, path=getwd(), sep=" ", dec=".",
        silent=FALSE, X=NULL )
{
    m <- imp$m
    i <- imp$iteration
    n <- nrow(imp$data)
    varnames <- names(imp$imp)[!sapply(imp$imp, is.null)]
    impnames <- paste0(file.prefix, seq(1,m,1), ".txt")
    f1 <- paste0( file.prefix, "__impvals.txt" )
    write(x=varnames, file=f1, append=FALSE, ncolumns=length(varnames), sep=" ")
    write(x=c(n,m),  file=f1, append=TRUE, ncolumns=1)
    write(x=impnames, file=f1, append=TRUE, ncolumns=m, sep="\t")
    for (k in 1:m){
        h1 <- mice::complete(imp, action=k)
        if ( ! is.null(X) ){
            h1 <- data.frame( X, h1 )
        }
        utils::write.table(x=h1, file=impnames[k], dec=dec, sep=sep,
                col.names=FALSE, row.names=FALSE, quote=F, eol="\n", )
    }
    if (!silent){
        cat("Data Values written to", paste0(path, "/", file.prefix, 1, ".txt"),
                "through", paste0(file.prefix, m, ".txt"), "\n")
        cat("Imputation master file written to", paste0(path, "/", f1 ), "\n")
    }
}
