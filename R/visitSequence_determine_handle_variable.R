## File Name: visitSequence_determine_handle_variable.R
## File Version: 0.082


# handle one passive variable
visitSequence_determine_handle_variable <- function( var.ii, impMethod, vis1, dat )
{
    formula_ii <- stats::as.formula( impMethod[ var.ii ] )
    vars_ii <- stats::get_all_vars( formula=formula_ii, data=dat[1,] )
    ness.ii <- colnames(vars_ii)
    IN <- length(ness.ii)
    for (nn in seq_len(IN) ){
        i2 <- match( ness.ii[nn], names(vis1) )
        for (oo in seq_len( length(i2) ) ){
            vis1 <- visitSequence_determine_add_entry( ness.ii.nn=ness.ii[nn],
                            var.ii=var.ii, vis1=vis1, occ=oo)
        }
    }
    vis1 <- visitSequence_determine_remove_entry( vis1=vis1, ness.ii=ness.ii,
                        var.ii=var.ii )
    return(vis1)
}

handle.variable <- visitSequence_determine_handle_variable
