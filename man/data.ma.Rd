%% File Name: data.ma.Rd
%% File Version: 0.40

\name{data.ma}
\alias{data.ma}

\alias{data.ma01}
\alias{data.ma02}
\alias{data.ma03}
\alias{data.ma04}
\alias{data.ma05}
\alias{data.ma06}
\alias{data.ma07}
\alias{data.ma08}
\alias{data.ma09}

\docType{data}

\title{
Example Datasets for \pkg{miceadds} Package
}

\description{
Example datasets for \pkg{miceadds} package.
}


\usage{
data(data.ma01)
data(data.ma02)
data(data.ma03)
data(data.ma04)
data(data.ma05)
data(data.ma06)
data(data.ma07)
data(data.ma08)
}


\format{
\itemize{
\item Dataset \code{data.ma01}:

Dataset with students nested within school and
student weights (\code{studwgt}). The format is

\code{'data.frame':   4073 obs. of  11 variables:} \cr
\code{ $ idstud  : num  1e+07 1e+07 1e+07 1e+07 1e+07 ...} \cr
\code{ $ idschool: num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ studwgt : num  6.05 6.05 5.27 5.27 6.05 ...} \cr
\code{ $ math    : int  594 605 616 524 685 387 536 594 387 562 ...} \cr
\code{ $ read    : int  647 651 539 551 689 502 503 597 580 576 ...} \cr
\code{ $ migrant : int  0 0 0 1 0 0 1 0 0 0 ...} \cr
\code{ $ books   : int  6 6 5 2 6 3 4 6 6 5 ...} \cr
\code{ $ hisei   : int  NA 77 69 45 66 53 43 NA 64 50 ...} \cr
\code{ $ paredu  : int  3 7 7 2 7 3 4 NA 7 3 ...} \cr
\code{ $ female  : int  1 1 0 0 1 1 0 0 1 1 ...} \cr
\code{ $ urban   : num  1 1 1 1 1 1 1 1 1 1 ...} \cr

\item Dataset \code{data.ma02}:

10 multiply imputed datasets of incomplete data \code{data.ma01}.
The format is

\code{List of 10} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr
\code{ $ :'data.frame':       4073 obs. of  11 variables:} \cr

\item Dataset \code{data.ma03}:

This dataset contains one variable
\code{math_EAP} for which a conditional posterior distribution with EAP
and its associated standard deviation is available.

\code{'data.frame':   120 obs. of  8 variables:} \cr
\code{ $ idstud    : int  1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 ...} \cr
\code{ $ female    : int  0 1 1 1 1 0 1 1 1 1 ...} \cr
\code{ $ migrant   : int  1 1 0 1 1 0 0 0 1 0 ...} \cr
\code{ $ hisei     : int  44 NA 26 NA 32 60 31 NA 34 26 ...} \cr
\code{ $ educ      : int  NA 2 NA 1 4 NA 2 NA 2 NA ...} \cr
\code{ $ read_wle  : num  74.8 78.1 103.2 81.2 119.2 ...} \cr
\code{ $ math_EAP  : num  337 342 264 285 420 ...} \cr
\code{ $ math_SEEAP: num  28 29.5 28.6 28.5 27.5 ...} \cr


\item Dataset \code{data.ma04}:

This dataset contains two hypothetical
scales \code{A} and \code{B} and single variables \code{V5}, \code{V6} and
\code{V7}.

\code{'data.frame':   281 obs. of  13 variables:} \cr
\code{ $ group: int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ A1   : int  2 2 2 1 1 3 3 NA 2 1 ...} \cr
\code{ $ A2   : int  2 2 2 3 1 2 4 4 4 4 ...} \cr
\code{ $ A3   : int  2 3 3 4 1 3 2 2 2 4 ...} \cr
\code{ $ A4   : int  3 4 6 4 7 5 3 5 5 1 ...} \cr
\code{ $ V5   : int  2 2 5 5 4 3 4 1 3 4 ...} \cr
\code{ $ V6   : int  2 5 5 1 1 3 2 2 2 4 ...} \cr
\code{ $ V7   : int  6 NA 4 5 6 2 5 5 6 7 ...} \cr
\code{ $ B1   : int  7 NA 6 4 5 2 5 7 3 7 ...} \cr
\code{ $ B2   : int  6 NA NA 6 3 3 4 6 6 7 ...} \cr
\code{ $ B3   : int  7 NA 7 4 3 4 3 7 5 NA ...} \cr
\code{ $ B4   : int  4 5 6 5 4 3 4 5 2 1 ...} \cr
\code{ $ B5   : int  7 NA 7 4 4 3 5 7 5 4 ...} \cr


\item Dataset \code{data.ma05}:

This is a two-level dataset with students nested within classes. Variables
at the student level are \code{Dscore}, \code{Mscore}, \code{denote},
\code{manote}, \code{misei} and \code{migrant}. Variables at the class
level are \code{sprengel} and \code{groesse}.

\code{'data.frame':   1673 obs. of  10 variables:} \cr
\code{ $ idstud  : int  100110001 100110002 100110003 100110004 100110005  ...} \cr
\code{ $ idclass : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...} \cr
\code{ $ Dscore  : int  NA 558 643 611 518 552 NA 534 409 543 ...} \cr
\code{ $ Mscore  : int  404 563 569 621 653 651 510 NA 517 566 ...} \cr
\code{ $ denote  : int  NA 1 1 1 3 2 3 2 3 2 ...} \cr
\code{ $ manote  : int  NA 1 1 1 1 1 2 2 2 1 ...} \cr
\code{ $ misei   : int  NA 51 NA 38 NA 50 53 53 38 NA ...} \cr
\code{ $ migrant : int  NA 0 0 NA 0 0 0 0 0 NA ...} \cr
\code{ $ sprengel: int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ groesse : int  25 25 25 25 25 25 25 25 25 25 ...} \cr

\item Dataset \code{data.ma06}:

This is a dataset in which the variable \code{FC} is only available
with grouped values (coarse data or interval data).

\code{'data.frame':   198 obs. of  7 variables:} \cr
\code{ $ id    : num  1001 1002 1003 1004 1005 ...} \cr
\code{ $ A1    : int  14 7 10 15 0 5 9 6 8 0 ...} \cr
\code{ $ A2    : int  5 6 4 8 2 5 4 0 7 0 ...} \cr
\code{ $ Edu   : int  4 3 1 5 5 1 NA 1 5 3 ...} \cr
\code{ $ FC    : int  3 2 2 2 2 NA NA 2 2 NA ...} \cr
\code{ $ FC_low: num  10 5 5 5 5 0 0 5 5 0 ...} \cr
\code{ $ FC_upp: num  15 10 10 10 10 100 100 10 10 100 ...} \cr

\item Dataset \code{data.ma07}:

This is a three-level dataset in which the variable \code{FC} is only available
with grouped values (coarse data or interval data).


\code{'data.frame':   1600 obs. of  9 variables:} \cr
\code{ $ id3: num  1001 1001 1001 1001 1001 ...} \cr
\code{ $ id2: num  101 101 101 101 101 101 101 101 101 101 ...} \cr
\code{ $ id1: int  1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{ $ x1 : num  0.91 1.88 NA 1.52 0.93 0.51 2.11 0.99 2.42 NA ...} \cr
\code{ $ x2 : num  -0.58 1.12 0.87 -0.01 -0.14 0.48 1.85 -0.9 0.93 0.63 ...} \cr
\code{ $ y1 : num  1.66 1.66 1.66 1.66 1.66 1.66 1.66 1.66 1.66 1.66 ...} \cr
\code{ $ y2 : num  0.96 0.96 0.96 0.96 0.96 0.96 0.96 0.96 0.96 0.96 ...} \cr
\code{ $ z1 : num  -0.53 -0.53 -0.53 -0.53 -0.53 -0.53 -0.53 -0.53 -0.53 -0.53 ...} \cr
\code{ $ z2 : num  0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 ...} \cr

\item Dataset \code{data.ma08}:

List with several vector of strings containing descriptive data from
published articles. See \code{\link{string_to_matrix}} for converting
these strings into matrices.

\code{List of 4} \cr
\code{ $ mat1: chr [1:6] "1. T1_mental_health" ...} \cr
\code{ $ mat2: chr [1:16] "1. Exp voc-T1 -" ...} \cr
\code{ $ mat3: chr [1:12] "1. TOWRE age 7\t-\t\t\t\t\t\t" ...} \cr
\code{ $ mat4: chr [1:18] "1. Vocab. age 7\t-\t\t\t\t\t" ...} \cr

\item Dataset \code{data.ma09}:

This is a subset of a PISA dataset that is used for generating synthetic data.

\code{'data.frame':   342 obs. of  41 variables:} \cr
\code{ $ SEX    : int  1 2 1 2 1 2 2 2 2 1 ...} \cr
\code{ $ AGE    : num  16 15.9 16.3 15.5 15.9 ...} \cr
\code{ $ HISEI  : int  37 46 66 51 25 NA 54 52 51 69 ...} \cr
\code{ $ FISCED : int  3 3 6 3 3 NA 3 3 2 2 ...} \cr
\code{ $ MISCED : int  3 4 4 4 3 NA 4 3 4 4 ...} \cr
\code{ $ PV1MATH: num  643 556 510 604 462 ...} \cr
\code{ $ M474Q01: int  1 1 1 1 0 1 1 1 1 0 ...} \cr
\code{ $ M155Q02: int  2 2 2 2 2 0 0 2 2 2 ...} \cr
\code{ $ M155Q01: int  1 1 0 1 1 1 1 1 1 1 ...} \cr
\code{[...]} \cr
}
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
%\examples{
%data(data.ma01)
%## maybe str(data.ma01) ; plot(data.ma01) ...
%}
%% \keyword{datasets}
