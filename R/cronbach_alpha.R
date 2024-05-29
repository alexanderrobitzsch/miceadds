## File Name: cronbach_alpha.R
## File Version: 0.02


# unstandardized estimate of Cronbach's alpha
cronbach_alpha <- function( dat.scale )
{
    I <- ncol( dat.scale )
    var.scale <- stats::var( dat.scale, use='pairwise.complete.obs' )
    v.bar <- mean( diag( var.scale )  )
    c.bar <- mean( var.scale[ upper.tri( var.scale ) ] )
    alpha <- ( I * c.bar ) / ( v.bar + (I-1) * c.bar )
    return(alpha)
}
