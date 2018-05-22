## File Name: visitSequence_determine_add_entry.R
## File Version: 0.04



#*************************************************
# add missing entry in visit sequence
visitSequence_determine_add_entry <- function( ness.ii.nn, var.ii, vis1, occ=1 )
{
    index.ness.ii <- which( names(vis1)==ness.ii.nn )[occ]
    index.var.ii <- which( names(vis1) %in% var.ii )
    if ( ! ( (index.ness.ii+1) %in% index.var.ii ) ){
        vis1 <- append( vis1, vis1[ var.ii ][1], index.ness.ii )
    }
    return(vis1)
}

add.entry <- visitSequence_determine_add_entry
