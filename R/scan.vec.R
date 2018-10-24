## File Name: scan.vec.R
## File Version: 0.22

scan.vector <- function( vec )
{
    strings <- c( " ", "\n", "\t")
    for (ss in strings){
        vec <- unlist( strsplit( vec, split=ss, fixed=TRUE) )
    }
    vec <- vec[ vec !="" ]
    return(vec)
}

scan.vec <- scan.vector
