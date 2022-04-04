%% File Name: data.enders.Rd
%% File Version: 0.151

\name{data.enders}
\alias{data.enders}

\alias{data.enders.depression}
\alias{data.enders.eatingattitudes}
\alias{data.enders.employee}

\docType{data}

\title{
Datasets from Enders' \emph{Missing Data} Book
}


\description{
Datasets from Enders' missing data book (2010).
}


\usage{
data(data.enders.depression)
data(data.enders.eatingattitudes)
data(data.enders.employee)
}

\format{


\itemize{
\item Dataset \code{data.enders.depression}:

\code{'data.frame':   280 obs. of  8 variables:} \cr
\code{ $ txgroup: int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ dep1   : int  46 49 40 47 33 44 45 53 40 55 ...} \cr
\code{ $ dep2   : int  44 42 28 47 33 41 43 35 43 45 ...} \cr
\code{ $ dep3   : int  26 29 31 NA 34 34 34 35 35 36 ...} \cr
\code{ $ r2     : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ r3     : int  0 0 0 1 0 0 0 0 0 0 ...} \cr
\code{ $ pattern: int  3 3 3 2 3 3 3 3 3 3 ...} \cr
\code{ $ dropout: int  0 0 0 1 0 0 0 0 0 0 ...} \cr

\item Dataset \code{data.enders.eatingattitudes}:

\code{'data.frame':   400 obs. of  14 variables:} \cr
\code{ $ id   : num  1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{ $ eat1 : num  4 6 3 3 3 4 5 4 4 6 ...} \cr
\code{ $ eat2 : num  4 5 3 3 2 5 4 3 7 5 ...} \cr
\code{ $ eat10: num  4 6 2 4 3 4 4 4 6 5 ...} \cr
\code{ $ eat11: num  4 6 2 3 3 5 4 4 5 5 ...} \cr
\code{ $ eat12: num  4 6 3 4 3 4 4 4 4 6 ...} \cr
\code{ $ eat14: num  4 7 2 4 3 4 4 4 6 6 ...} \cr
\code{ $ eat24: num  3 6 3 3 3 4 4 4 4 5 ...} \cr
\code{ $ eat3 : num  4 5 3 3 4 4 3 6 4 5 ...} \cr
\code{ $ eat18: num  5 6 3 5 4 5 3 6 4 6 ...} \cr
\code{ $ eat21: num  4 5 2 4 4 4 3 5 4 5 ...} \cr
\code{ $ bmi  : num  18.9 26 18.3 18.2 24.4 ...} \cr
\code{ $ wsb  : num  9 13 6 5 10 7 11 8 10 12 ...} \cr
\code{ $ anx  : num  11 19 8 14 7 11 12 12 14 12 ..} \cr

\item Dataset \code{data.enders.employee}:

\code{'data.frame':   480 obs. of  9 variables:} \cr
\code{ $ id      : num  1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{ $ age     : num  40 53 46 37 44 39 33 43 35 37 ...} \cr
\code{ $ tenure  : num  10 14 10 8 9 10 7 9 9 10 ...} \cr
\code{ $ female  : num  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ wbeing  : num  8 6 NA 7 NA 7 NA 7 7 5 ...} \cr
\code{ $ jobsat  : num  8 5 7 NA 5 NA 5 NA 7 6 ...} \cr
\code{ $ jobperf : num  6 5 7 5 5 7 7 7 7 6 ...} \cr
\code{ $ turnover: num  0 0 0 0 0 0 0 0 1 0 ...} \cr
\code{ $ iq      : num  106 93 107 94 107 118 103 106 108 97 ...} \cr


}

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


\source{
The datasets were downloaded from
\url{https://www.appliedmissingdata.com/}.
}


\references{
Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
}


%\code{\link{data.allison}} (Example 1)

%\examples{
%data(data.enders.depression)
%## maybe str(data.enders.depression) ; plot(data.enders.depression) ...
%}
%% \keyword{datasets}
