## File Name: write.mice.imputation.R
## File Version: 0.265

#--- write mice imputation object
write.mice.imputation <- function( mi.res, name, include.varnames=TRUE, long=TRUE,
            mids2spss=TRUE, spss.dec=",", dattype=NULL )
{
    ismids <- TRUE
    if ( inherits(mi.res,"mids.1chain" ) ) {
        mi.res0 <- mi.res
        mi.res <- mi.res$midsobj
        mi.res$chainMean <- mi.res0$chainMean
        mi.res$chainVar <- mi.res0$chainVar
        ismids <- FALSE
    }

    pf.subf <- file.path( getwd(), paste( name, sep=""))
    dir.create(pf.subf)                 # define subdirectory
    # write legend of variables
    writeLines( colnames(mi.res$data), file.path( pf.subf,
                    paste( name, "__LEGEND.txt", sep="") ))
    l1 <- paste( name, "__IMPDATA", 1:mi.res$m, ".dat", sep="")
    utils::write.table( l1, file.path( pf.subf, paste( name, "__IMP_LIST.txt", sep="") ),
            col.names=FALSE, row.names=FALSE, quote=FALSE)

    #*** save summary in subdirectory
    sink( file.path( pf.subf, paste( name, "__IMP_SUMMARY.txt", sep="") ), split=TRUE )
    cat( paste(Sys.time()), "\n\n", pf.subf, "\n\n" )
    print( summary( mi.res ) )
    cat("\n\n")

    if ( ismids ){
        if ( ( mi.res$m > 1 ) & ( dim(mi.res$chainMean)[2]  > 1 )){
            h1 <- Rhat.mice( mi.res )
            for (vv in seq(2,ncol(h1))){
                h1[,vv] <- round( h1[,vv], 3 )
            }
            print(h1)
        }
    }
    print( utils::citation()) ;
    print( utils::citation("mice"))
    print(Sys.info()) ;
    print( utils::sessionInfo())
    sink()          ## end mice summary

    for (i in 1:mi.res$m ){
        dat_i <- mice::complete( mi.res, action=i )
        utils::write.table( dat_i, file=file.path( pf.subf,
                            paste( name, "__IMPDATA", i, ".dat", sep="")),
                    quote=FALSE, row.names=FALSE, col.names=include.varnames, na="." )
        if ( ! is.null(dattype) ){
            if (dattype=="csv2" ){
                write.csv2( dat_i, file=file.path( pf.subf,
                            paste( name, "__IMPDATA", i, ".csv", sep="")),
                            quote=FALSE, row.names=FALSE, na="." )
                }
            }
        cat("\n",i)
        utils::flush.console()
    }
    cat("\n")
    # write data file in long format
    utils::write.table( mice::complete( mi.res, action="long" ),
                        file=file.path( pf.subf, paste( name, "__LONG.dat", sep="")),
                        quote=FALSE, row.names=FALSE, col.names=TRUE, na="." )
    # variable names
    writeLines( colnames( mice::complete( mi.res, action=1 ) ),
                    file.path( pf.subf, paste( name, "__VARNAMES.txt", sep="")) )
    # write SPSS file
    if (mids2spss){
        mice::mids2spss(mi.res, filedat=paste( name, "__SPSS.txt", sep=""),
                            filesps=paste( name, "__SPSS.sps", sep=""),
                            path=pf.subf, dec=spss.dec )
    }
    # Mplus-Body
    if ( ! include.varnames ){
        vars <- colnames(mi.res$data)
        vars2 <- VariableNames2String( vars, breaks=60)
        l1 <- c("TITLE: xxxx ;", "", "DATA: ", "",
                    paste( "FILE IS ", name, "__IMP_LIST.txt;", sep=""),
                    "TYPE=IMPUTATION;", "",
                    "VARIABLE:", "", "NAMES ARE",
                    vars2, ";", "", "! edit usevariables are;", "!usevar are",
                        "   ", "", "MISSING=. ;", "", "!.........................",
                    "! Mplus statements"
                )
        writeLines( l1, file.path( pf.subf,
                            paste( name, "__MPLUS-INPUT-BODY.inp", sep="") ))
    }

    # write predictorMatrix and imputationMethod
    utils::write.csv2( mi.res$method, file.path( pf.subf,
                            paste( name, "__IMPMETHOD.csv", sep="")), quote=FALSE)
    utils::write.csv2( mi.res$predictorMatrix, file.path( pf.subf,
                            paste( name, "__PREDICTORMATRIX.csv", sep="")), quote=FALSE)

    #* write mice object as a Rdata object
    save( mi.res, file=file.path( pf.subf, paste( name, ".Rdata", sep="") ) )
    #* save list of imputed datasets
    datlist <- mids2datlist( mi.res )
    save( datlist, file=file.path( pf.subf, paste( name, "__DATALIST.Rdata", sep="") ) )
}

