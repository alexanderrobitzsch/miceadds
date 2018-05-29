%% File Name: miceadds-utilities.Rd
%% File Version: 0.05

\name{miceadds-utilities}
\alias{miceadds-utilities}
\alias{ma_exists_get}
\alias{ma_exists}


\title{Utility Functions in \pkg{miceadds}}

\description{
Utility functions in \pkg{miceadds}.
}

\usage{
## searches for objects in parent environments
ma_exists_get( x, pos, n_index=1:8)
ma_exists( x, pos, n_index=1:8)
}


\arguments{
\item{x}{Object name (character)}
\item{pos}{Environment}
\item{n_index}{Levels in \code{parent.frame} in which object is searched}
}

\details{
The function \code{ma_exists_get} is used in \code{miceadds:::mice_imputation_get_states}.
}


%\examples{
%
%}
