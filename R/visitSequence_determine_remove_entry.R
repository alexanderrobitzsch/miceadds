## File Name: visitSequence_determine_remove_entry.R
## File Version: 0.04


#***************************************************
# remove entry
visitSequence_determine_remove_entry <- function( vis1, ness.ii, var.ii )
{
    index.ness.ii <- unlist( sapply( ness.ii, FUN=function(nn){
                match( nn, names(vis1) ) } ) )
    index.var.ii <- which( names(vis1) %in% var.ii )
    IN <- length( index.var.ii)
    l1 <- NULL
    for (vv in seq_len(IN) ){
        if ( ! ( ( index.var.ii[vv] - 1 ) %in% index.ness.ii  ) ){
            l1 <- c( l1, index.var.ii[vv] )
        }
    }
    if ( length(l1) > 0 ){
        vis1 <- vis1[ - l1 ]
    }
    return(vis1)
}


remove.entry <- visitSequence_determine_remove_entry
