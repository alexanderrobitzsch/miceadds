## File Name: write_fwf2_format2.R
## File Version: 0.092


#---- utility function for formatting output in write.fwf2
write_fwf2_format2 <- function( vec1, ff, fr )
{
    do_round <- TRUE
    if ( ! is.numeric(vec1) ){
        fr <- 0
        do_round <- FALSE
    }
    if (fr==0){
        vec2 <- vec1
        if (do_round){
            vec2 <- round( vec2, fr )
        }
        blank.vv <- paste( rep( " ", ff ), collapse="" )
        vec2 <- paste( substring( blank.vv, 1, ff - nchar(paste(vec2)) ), vec2, sep="")
    } else {
        d.vv <- round( vec1, fr ) + 10^(-(fr+1))
        blank.vv <- paste( rep( " ", ff+1 ), collapse="" )
        d.vv <- paste( substring( blank.vv, 1, ff+1 - nchar(d.vv) ), d.vv, sep="")
        g.vv <- grep("NA",d.vv)
        d.vv[ g.vv  ] <- ifelse( ff > 1,  gsub( "NA", " .", d.vv[g.vv] ),
                                gsub( "NA", ".", d.vv[g.vv] ) )
        vec2 <- substring( d.vv, 1, ff )
    }
    return(vec2)
}
