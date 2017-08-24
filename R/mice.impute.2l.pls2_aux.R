
################################################
# create interactions
create_interactions <- function (y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_){ 
	# .Call("create_interactions_cpp", ... )
	.Call("miceadds_create_interactions_cpp", 
		y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_, 
		PACKAGE = "miceadds")
}

# create_interactions_cpp <- function(Yr, Xr, Xallr, index_int, MI, maxcols) {
#     .Call('miceadds_create_interactions_cpp', PACKAGE = 'miceadds', 
#				Yr, Xr, Xallr, index_int, MI, maxcols)
# }					