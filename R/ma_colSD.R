## File Name: ma_colSD.R
## File Version: 0.01

ma_colSD <- function(x)
{
    apply( x, 2, stats::sd )
}
