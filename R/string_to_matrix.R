## File Name: string_to_matrix.R
## File Version: 0.190

string_to_matrix <- function(x, rownames=NULL, col_elim=NULL, as_numeric=FALSE,
        diag_val=NULL, extend=FALSE, col1_numeric=FALSE, split=" ")
{
    x <- unlist(strsplit(x, split="\n", fixed=TRUE ))
    x <- gsub("\t", " ", x, fixed=TRUE )
    x <- x[ x!=""]

    #-- split vectors
    v1 <- sapply( x, FUN=function(vv){
                g1 <- strsplit( vv, split=split )[[1]]
                g1 <- g1[ g1 !="" ]
                return(g1)
                }, simplify=FALSE)
    NV <- length(v1)

    #-- dimensions
    sizes <- rep(NA, NV)
    sizes1 <- col1 <- sizes
    for (vv in 1:NV){
        sizes1[vv] <- sizes[vv] <- length(v1[[vv]])
        if (col1_numeric){
            col1[vv] <- min( which(! is.na( as.numeric(paste0(v1[[vv]])))))
            sizes1[vv] <- sizes1[vv] - col1[vv] + 1
        }
    }

    NR <- length(x)
    NC <- max(sizes)
    if (col1_numeric){
        NC <- max(sizes1) + 1
    }
    mat <- matrix("", nrow=NR, ncol=NC)
    for (vv in 1:NV){
        v1_vv <- v1[[vv]]
        if (col1_numeric){
            mat[vv,1] <- paste0( v1_vv[ seq(1, col1[vv]-1) ], collapse=split)
            mat[vv,1+seq(1,sizes1[vv])] <- v1_vv[ seq(col1[vv], sizes[vv]) ]
        } else {
            mat[ vv, seq(1,sizes[vv]) ] <- v1_vv
        }
    }
    if ( ! is.null(rownames) ){
        rownames(mat) <- mat[,rownames]
    }
    if ( ! is.null(col_elim) ){
        mat <- mat[, - col_elim ]
    }
    if ( as_numeric){
        NC <- ncol(mat)
        mat0 <- mat
        mat <- matrix(NA, nrow=nrow(mat0), ncol=ncol(mat0) )
        for (cc in 1:NC){
            mat_cc <- as.numeric(mat0[,cc])
            if (mean(is.na(mat_cc)) < 1){
                mat[,cc] <- mat_cc
            } else {
                mat[,cc] <- mat0[,cc]
            }
        }
        rownames(mat) <- rownames(mat0)
    }
    if ( ! is.null(diag_val) ){
        diag(mat) <- diag_val
    }
    if ( extend ){
        mat0 <- mat
        mat0[ is.na(mat0) ] <- 0
        mat <- ( mat0 + t(mat0) )
        diag(mat) <- diag(mat0)
        colnames(mat) <- rownames(mat)
    }
    #--- output
    return(mat)
}
