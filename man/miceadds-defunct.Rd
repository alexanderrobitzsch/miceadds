%% File Name: miceadds-defunct.Rd
%% File Version: 0.153

\name{miceadds-defunct}
\alias{miceadds-defunct}
\alias{mice.impute.2l.pls}
\alias{fast.groupmean}
\alias{fast.groupsum}
\alias{mice.impute.2l.plausible.values}
\alias{mice.impute.2lonly.pmm2}
\alias{mice.impute.2lonly.norm2}
\alias{mice.impute.tricube.pmm2}



\title{Defunct \pkg{miceadds} Functions}

\description{
These functions have been removed or replaced in the \pkg{miceadds}
package.
}

\usage{
fast.groupmean(...)
fast.groupsum(...)
mice.impute.2l.plausible.values(...)
mice.impute.2l.pls(...)
mice.impute.2lonly.norm2(...)
mice.impute.2lonly.pmm2(...)
mice.impute.tricube.pmm2(...)
}

\arguments{
    \item{\dots}{Arguments to be passed.}
}

\details{
The \code{fast.groupmean} function has been replaced by
\code{\link{GroupMean}}.

The \code{fast.groupsum} function has been replaced by
\code{\link{GroupSum}}.

The \code{mice.impute.2l.plausible.values} function has been replaced by
\code{\link{mice.impute.plausible.values}}.

The \code{mice.impute.2l.pls2} function has been replaced by
\code{\link{mice.impute.pls}}.

The \code{mice.impute.2lonly.norm2} and \code{mice.impute.2lonly.pmm2}
functions can be safely replaced by the
\code{\link[mice:mice.impute.2lonly.norm]{mice::mice.impute.2lonly.norm}} and
\code{\link[mice:mice.impute.2lonly.pmm]{mice::mice.impute.2lonly.pmm}}
functions in the \pkg{mice} package.

The \code{mice.impute.tricube.pmm2} function has been replaced by
\code{\link{mice.impute.tricube.pmm}}.
}

% \examples{
% # test
% }
