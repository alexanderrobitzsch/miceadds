## File Name: crlrem.R
## File Version: 1.071

# remove line endings
crlrem <- function( filename1, filename2 )
{
    filename <- filename1
    con <- file(filename, 'rb')
    bin <- readBin(con, raw(), 100000)
    bin <- bin[ which(bin !='0d') ]
    close(con)
    Sys.sleep(1)
    con <- file(filename2, 'wb')
    writeBin(bin, con)
    close(con)
}
