%% File Name: data.allison.Rd
%% File Version: 0.19

\name{data.allison}
\alias{data.allison}
\alias{data.allison.gssexp}
\alias{data.allison.hip}
\alias{data.allison.usnews}

\docType{data}

\title{
Datasets from Allison's \emph{Missing Data} Book
}

\description{
Datasets from Allison's missing data book (Allison 2002).
}


\usage{
data(data.allison.gssexp)
data(data.allison.hip)
data(data.allison.usnews)
}


\format{

\itemize{
\item Data \code{data.allison.gssexp}:

\code{'data.frame':   2991 obs. of  14 variables:} \cr
\code{ $ AGE     : num  33 59 NA 59 21 22 40 25 41 45 ...} \cr
\code{ $ EDUC    : num  12 12 12 8 13 15 9 12 12 12 ...} \cr
\code{ $ FEMALE  : num  1 0 1 0 1 1 1 0 1 1 ...} \cr
\code{ $ SPANKING: num  1 1 2 2 NA 1 3 1 1 NA ...} \cr
\code{ $ INCOM   : num  11.2 NA 16.2 18.8 13.8 ...} \cr
\code{ $ NOCHILD : num  0 0 0 0 1 1 0 0 0 0 ...} \cr
\code{ $ NODOUBT : num  NA NA NA 1 NA NA 1 NA NA 1 ...} \cr
\code{ $ NEVMAR  : num  0 0 0 0 1 1 0 1 0 0 ...} \cr
\code{ $ DIVSEP  : num  1 0 0 0 0 0 0 0 0 1 ...} \cr
\code{ $ WIDOW   : num  0 0 0 0 0 0 1 0 1 0 ...} \cr
\code{ $ BLACK   : num  1 1 1 0 1 1 0 1 1 1 ...} \cr
\code{ $ EAST    : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ MIDWEST : num  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ SOUTH   : num  0 0 0 0 0 0 0 0 0 0 ...} \cr



\item Data \code{data.allison.hip}:

\code{'data.frame':   880 obs. of  7 variables:} \cr
\code{ $ SID : num  1 1 1 1 2 2 2 2 9 9 ...} \cr
\code{ $ WAVE: num  1 2 3 4 1 2 3 4 1 2 ...} \cr
\code{ $ ADL : num  3 2 3 3 3 1 2 1 3 3 ...} \cr
\code{ $ PAIN: num  0 5 0 0 0 1 5 NA 0 NA ...} \cr
\code{ $ SRH : num  2 4 2 2 4 1 1 2 2 3 ...} \cr
\code{ $ WALK: num  1 0 0 0 0 0 0 0 1 NA ...} \cr
\code{ $ CESD: num  9 28 31 11.6 NA ...} \cr


\item Data \code{data.allison.usnews}:

\code{'data.frame':   1302 obs. of  7 variables:} \cr
\code{ $ CSAT   : num  972 961 NA 881 NA ...} \cr
\code{ $ ACT    : num  20 22 NA 20 17 20 21 NA 24 26 ...} \cr
\code{ $ STUFAC : num  11.9 10 9.5 13.7 14.3 32.8 18.9 18.7 16.7 14 ...} \cr
\code{ $ GRADRAT: num  15 NA 39 NA 40 55 51 15 69 72 ...} \cr
\code{ $ RMBRD  : num  4.12 3.59 4.76 5.12 2.55 ...} \cr
\code{ $ PRIVATE: num  1 0 0 0 0 1 0 0 0 1 ...} \cr
\code{ $ LENROLL: num  4.01 6.83 4.49 7.06 6.89 ...} \cr


}

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
The datasets were downloaded from \emph{http://www.ats.ucla.edu/stat/examples/md/}.
}


\references{
Allison, P. D. (2002). \emph{Missing data}. Newbury Park, CA: Sage.
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Hip dataset | Imputation using a wide format
#############################################################################

# at first, the hip dataset is 'melted' for imputation

data(data.allison.hip)
  ##   head(data.allison.hip)
  ##     SID WAVE ADL PAIN SRH WALK   CESD
  ##   1   1    1   3    0   2    1  9.000
  ##   2   1    2   2    5   4    0 28.000
  ##   3   1    3   3    0   2    0 31.000
  ##   4   1    4   3    0   2    0 11.579
  ##   5   2    1   3    0   4    0     NA
  ##   6   2    2   1    1   1    0  2.222

library(reshape)
hip.wide <- reshape::reshape(data.allison.hip, idvar="SID", timevar="WAVE",
                direction="wide")
  ##   > head(hip.wide, 2)
  ##     SID ADL.1 PAIN.1 SRH.1 WALK.1 CESD.1 ADL.2 PAIN.2 SRH.2 WALK.2 CESD.2 ADL.3
  ##   1   1     3      0     2      1      9     2      5     4      0 28.000     3
  ##   5   2     3      0     4      0     NA     1      1     1      0  2.222     2
  ##     PAIN.3 SRH.3 WALK.3 CESD.3 ADL.4 PAIN.4 SRH.4 WALK.4 CESD.4
  ##   1      0     2      0     31     3      0     2      0 11.579
  ##   5      5     1      0     12     1     NA     2      0     NA

# imputation of the hip wide dataset
imp <- mice::mice( as.matrix( hip.wide[,-1] ), m=5, maxit=3 )
summary(imp)
}
}



%% \keyword{datasets}
