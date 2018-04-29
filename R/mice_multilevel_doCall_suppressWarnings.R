## File Name: mice_multilevel_doCall_suppressWarnings.R
## File Version: 0.02

#######################################################
# calls a function and suppresses warnings if requested

mice_multilevel_doCall_suppressWarnings <- function( what , args , warnings = TRUE){
    if (warnings){
        res <- do.call( what = what , args = args)
    } else {
        suppressWarnings(
            res <- do.call( what = what , args = args)
            )
    }
    return(res)
}    
