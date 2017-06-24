\name{mids2mlwin}
\alias{mids2mlwin}
\title{Export \code{mids} object to MLwiN}
\usage{
mids2mlwin(imp, file.prefix, path = getwd(), sep = " ", dec = ".", silent = FALSE,
   X = NULL)
}
\arguments{
  \item{imp}{The \code{imp} argument is an object of class
  \code{mids}, typically produced by the \code{mice()}
  function.}

  \item{file.prefix}{A character string describing the
  prefix of the output data files.}

  \item{path}{A character string containing the path of the
  output file.  By default, files are written to the
  current \code{R} working directory.}

   \item{sep}{The separator between the data fields.}

  \item{dec}{The decimal separator for numerical data.}
  
  \item{silent}{A logical flag stating whether the names of
  the files should be printed.}

  \item{X}{Optional data frame of variables to be included in
imputed datasets.}  
}
\value{
  The return value is \code{NULL}.
}
\description{
  Converts a \code{mids} object into a format recognized by the multilevel
  software MLwiN.
}
%\details{
%xxx
%}
\author{
Thorsten Henke <mail@thorstenluka.de>
}


\examples{
\dontrun{
# imputation nhanes data
data(nhanes)
imp <- mice::mice(nhanes)
# write files to MLwiN
mids2mlwin(imp, file.prefix="nhanes" )
}	
}

%\seealso{
%  \code{\link[=mids-class]{mids}}, \code{\link{mids2spss}}
%}
%\keyword{manip}

