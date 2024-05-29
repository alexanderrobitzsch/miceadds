## File Name: ANSI_create_table.R
## File Version: 0.580


#*** create table with results
ANSI_create_table <- function (dat, criterion,
        horiz_vars, horiz_vals=NULL, vert_vars, vert_vals=NULL,
        subset, digits=NULL, dec=".",
        horiz_empty=NULL, vert_empty=NULL )
{
    x <- dat
    # subset of a dataset
    r <- if (missing(subset)){
        rep_len(TRUE, nrow(x))
    } else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r))
            stop("'subset' must be logical")
        r & !is.na(r)
    }
    x <- x[r,]

    #** check for missing variables
    l1 <- setdiff(c(horiz_vars,vert_vars), colnames(dat))
    if (length(l1)>0){
        l2 <- paste0( l1, collapse=" ")
        # stop(paste0( "The following variables were not found: ", l2, "\n"))
        warning(paste0( "The following variables were not found: ", l2, "\n"))
    }

    #--- horizontal variables
    NH <- length(horiz_vars)
    if ( is.null(horiz_vals) ){
        horiz_vals <- as.list(1L:NH)
        names(horiz_vals) <- horiz_vars
        for (nn in 1L:NH){
            horiz_vals[[nn]] <- sort( unique( x[, horiz_vars[nn]] ) )
        }
    }
    if ( ! is.null(horiz_vals) ){
        horiz_vals1 <- as.list(1L:NH)
        names(horiz_vals1) <- horiz_vars
        if ( ! is.null( names(horiz_vals) ) ){
            for (nn in horiz_vars)
                horiz_vals1[[nn]] <- horiz_vals[[nn]]
                res <- ANSI_create_table_check_variable_values(variable=nn,
                            values=horiz_vals[[nn]], dat=dat)
            }
            horiz_vals <- horiz_vals1
    }
    h2 <- as.list( 1L:NH)
    names(h2) <- horiz_vars[ seq(NH,1,-1) ]
    for (nn in 1L:NH){
        h2[[NH-nn+1]] <- horiz_vals[[nn]]
    }
    horiz_table <- expand.grid( h2 )[, seq(NH,1,-1),drop=FALSE]
    horiz_NR <- nrow(horiz_table)
    horiz_NC <- ncol(horiz_table)

    #--- vertical variables
    NH <- length(vert_vars)
    if ( is.null(vert_vals) ){
        vert_vals <- as.list( 1L:NH)
        names(vert_vals) <- vert_vars
        for (nn in 1L:NH){
            vert_vals[[nn]] <- sort( unique( x[, vert_vars[nn]] ) )
        }
    }
    if ( ! is.null(vert_vals) ){
        vert_vals1 <- as.list(1L:NH)
        names(vert_vals1) <- vert_vars
        if ( ! is.null( names(vert_vals) ) ){
            for (nn in vert_vars)
                vert_vals1[[ nn ]] <- vert_vals[[nn]]
                res <- ANSI_create_table_check_variable_values(variable=nn,
                            values=vert_vals[[nn]], dat=dat)
        }
        vert_vals <- vert_vals1
    }
    h2 <- as.list( 1L:NH)
    names(h2) <- vert_vars[ seq(NH,1,-1) ]
    for (nn in 1L:NH){
        h2[[NH-nn+1]] <- vert_vals[[nn]]
    }
    vert_table <- expand.grid(h2)[, seq(NH,1,-1),drop=FALSE]
    vert_NR <- nrow(vert_table)
    vert_NC <- ncol(vert_table)

    #--- create complete table
    dfr <- matrix( NA, nrow=horiz_NR, ncol=vert_NR)
    NN <- nrow(x)
    for (hr in 1L:horiz_NR){
        for (vr in 1L:vert_NR){
            ind <- 1L:NN
            for (nn in 1L:horiz_NC){
                ind0 <- which(  paste(x[, horiz_vars[nn] ])==paste(horiz_table[hr,nn]) )
                ind <- intersect( ind, ind0 )
            }
            for (nn in 1L:vert_NC){
                ind0 <- which( paste(x[, vert_vars[nn] ])==paste(vert_table[vr,nn]) )
                ind <- intersect( ind, ind0 )
            }
            if ( length(ind) > 1){
                cat("Selected more than one line!\n")
                print(x[ind,])
                stop()
            }
            if ( length(ind)==1 ){
                x1 <- x[ind,criterion]
                dfr[hr,vr] <- x1
            }
        }
    }

    #*** labels horizontal variables
    nn <- 1
    cn <- paste0( horiz_vars[nn], "=", horiz_table[,nn] )
    if (horiz_NC>1){
        for (nn in 2L:horiz_NC){
            cn <- paste0( cn, " # ", horiz_vars[nn], "=", horiz_table[,nn] )
        }
    }
    rownames(dfr) <- cn

    #*** labels vertical variables
    nn <- 1
    cn <- paste0( vert_vars[nn], "=", vert_table[,nn] )
    if (vert_NC>1){
        for (nn in 2L:vert_NC){
            cn <- paste0( cn, " # ", vert_vars[nn], "=", vert_table[,nn] )
        }
    }
    colnames(dfr) <- cn
    if ( ! is.null(digits) ){
        V <- ncol(dfr)
        if ( length(digits)!=V){
            digits <- rep(digits[1],V)
        }
        for (vv in 1L:V){
            # num1 <- round( as.numeric( paste(dfr[,vv])), digits )
            # g1 <- sprintf( paste0("%.",digits[vv], "f"), num1 )
            num1 <- as.numeric(paste(dfr[,vv]))
            num1 <- round( num1, digits[vv] )
            num1 <- as.numeric(paste(num1))
            g1 <- sprintf( paste0("%.",digits[vv], "f"), num1 )
            if ( dec==","){
                g1 <- gsub( ".", ",", g1, fixed=TRUE)
            }
            dfr[,vv] <- g1
        }  # end vv
    }
    #**** include empty rows if needed
    if ( ! is.null( horiz_empty) ){
        dfr <- ANSI_matrix_include_rows(mat=dfr, empty=horiz_empty, fill="")
    }

    if ( ! is.null( vert_empty) ){
        dfr <- ANSI_matrix_include_cols(mat=dfr, empty=vert_empty, fill="")
    }
    attr(dfr, "horiz_table") <- horiz_table
    attr(dfr, "vert_table") <- vert_table
    return(dfr)
}
