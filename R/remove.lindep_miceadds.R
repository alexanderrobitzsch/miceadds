## File Name: remove.lindep_miceadds.R
## File Version: 0.06
## File Last Change: 2017-02-06 11:05:50

################################################
# copied remove.lindep function from mice package

remove.lindep_miceadds <- function(x, y, ry, eps = 1e-04, maxcor = 0.99, 
		allow.na = FALSE, ...) {
    if (ncol(x) == 0)
        return(NULL)
    if (eps <= 0)
        stop("\n Argument 'eps' must be positive.")
    xobs <- x[ry, , drop = FALSE]
	
	updateLog_frame <- 2
	
    if (allow.na) {
        if (sum(ry) == 0) {
            # escape for columns with only missing data  SvB 10/3/2011
#            updateLog(out = "No observed cases, predictor removal skipped", 
#						frame = updateLog_frame )
            return(rep(TRUE, ncol(x)))
        }
    }
    yobs <- as.numeric(y[ry])
    keep <- unlist(apply(xobs, 2, var) > eps)
    keep[is.na(keep)] <- FALSE
    highcor <- suppressWarnings((unlist(apply(xobs, 2, cor, yobs)) < maxcor))
    keep <- keep & highcor
    if (all(!keep))
#        updateLog(out = "All predictors are constant or have too high correlation.", 
#					frame = updateLog_frame )
    if (length(keep) == 1) keep[1] <- TRUE  # SvB 19/1/14
    k <- sum(keep)
	xobs2 <- xobs[, keep, drop = FALSE]
	if ( nrow(xobs2) > 0 ){
		cx <- cor( xobs2 , use = "all.obs")
		eig <- eigen(cx, symmetric = TRUE)
		ncx <- cx
		while (eig$values[k]/eig$values[1] < eps) {
			j <- (1:k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
			keep[keep][j] <- FALSE
			ncx <- cx[keep[keep], keep[keep], drop = FALSE]
			k <- k - 1
			eig <- eigen(ncx)
		}
		if (!all(keep)) {
			out <- paste(dimnames(x)[[2]][!keep], collapse = ", ")
	#        updateLog(out = out, frame = updateLog_frame )
		}
	} else {
		keep <- rep( TRUE , ncol(xobs) )
	}
    return(keep)
}


# updateLog <- function(out = NULL, meth = NULL, frame = 2) {
#    s <- get("state", parent.frame(frame))
#    r <- get("loggedEvents", parent.frame(frame))
#
#    rec <- data.frame(it = s$it, im = s$im, co = s$co, dep = s$dep, 
#			meth = ifelse(is.null(meth), s$meth, meth), out = ifelse(is.null(out),
#					"", out))
#
#    if (s$log)
#        rec <- rbind(r, rec)
#    s$log <- TRUE
#    assign("state", s, pos = parent.frame(frame), inherits = TRUE)
#    assign("loggedEvents", rec, pos = parent.frame(frame), inherits = TRUE)
#    return()
# }
