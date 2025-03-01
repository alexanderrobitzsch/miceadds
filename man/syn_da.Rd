%% File Name: syn_da.Rd
%% File Version: 0.061

\name{syn_da}
\alias{syn_da}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generation of Synthetic Data Utilizing Data Augmentation
}
\description{
This function generates synthetic data utilizing data augmentation
(Jiang et al., 2022; Grund et al., 2022). Continuous
and ordinal variables can be handled. The order of the synthesized variables
can be defined using the argument \code{syn_vars}.
}
\usage{
syn_da(dat, syn_vars=NULL, fix_vars=NULL, ord_vars=NULL, da_noise=0.5,
   formula_syn=NULL, use_pls=TRUE, ncomp=20, exact_regression=TRUE,
   exact_marginal=TRUE, imp_maxit=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Original dataset
}
  \item{syn_vars}{
Vector with variable names that should be synthesized
}
  \item{fix_vars}{
Vector with variable names that are held fixed in the synthesis
}
  \item{ord_vars}{
Vector with ordinal variables that are treated as factors when modeled
as predictors in the regression model
}
  \item{da_noise}{
Proportion of variance (i.e., unreliability) that is added as noise in data
augmentation. The argument can be numeric or a vector, depending on whether it is made
variable-specific. Can also be a vector of the same dimension as \code{syn_vars}
if different unreliabilities should be used. Variables that should not receive
a noise variable should be specified with an \code{1} entry (see Example 2).
If \code{da_noise=1}, no noisy versions of the original variables are specified.
}
\item{formula_syn}{Optional list of regression formulas for conditional models.
Formulas can be a specified for a subset of synthesized variables.
Non-specified formulas are automatically specified by linear models.}
  \item{use_pls}{
Logical indicating whether partial least squares (PLS) should be used for dimension
reduction
}
  \item{ncomp}{
Number of PLS factors
}
  \item{exact_regression}{
Logical indicating whether residuals are forced to be uncorrelated with
predictors in the synthesis model
}
  \item{exact_marginal}{
Logical indicating whether marginal distributions of the variables should be
preserved
}
  \item{imp_maxit}{
Number of iterations in the imputation if the original dataset contains
missing values
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with entries

\item{dat_syn}{generated synthetic data}
\item{dat2}{Data frame containing original and synthetic data}
\item{\ldots}{more entries}
}

\references{
Grund, S., Luedtke, O., & Robitzsch, A. (2022). Using synthetic data to improve
the reproducibility of statistical results in psychological research.
\emph{Psychological Methods}. Epub ahead of print. \doi{10.1037/met0000526}

Jiang, B., Raftery, A. E., Steele, R. J., & Wang, N. (2022). Balancing inferential
integrity and disclosure risk via model targeted masking and multiple imputation.
\emph{Journal of the American Statistical Association, 117}(537), 52-66.
\doi{10.1080/01621459.2021.1909597}
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Generate synthetic data with item responses and covariates
#############################################################################

data(data.ma09, package="miceadds")
dat <- data.ma09

# fixed variables in synthesis
fix_vars <- c("PV1MATH", "SEX","AGE")
# ordinal variables in synthesis
ord_vars <- c("FISCED", "MISCED", items)
# variables that should be synthesized
syn_vars <- c("HISEI", "FISCED", "MISCED", items)

#-- synthesize data
mod <- miceadds::syn_da( dat=dat, syn_vars=syn_vars, fix_vars=fix_vars,
            ord_vars=ord_vars, da_noise=0.5, imp_maxit=2, use_pls=TRUE, ncomp=20,
            exact_regression=TRUE, exact_marginal=TRUE)
#- extract synthetic dataset
mod$dat_syn

#############################################################################
# EXAMPLE 2: Not all variables are augmented, formula specifications
#############################################################################

data(data.ma09, package="miceadds")
dat <- data.ma09

# fixed variables in synthesis
fix_vars <- c("PV1MATH", "SEX")
# ordinal variables in synthesis
ord_vars <- c("FISCED", "MISCED")
# variables that should be synthesized
syn_vars <- c("AGE","HISEI", "FISCED", "MISCED")
# no noise variable for FISCED and MISCED should be specified
da_noise <- c(AGE=0.1, HISEI=0.1, FISCED=0, MISCED=0)
# define conditional models for some variables
formula_syn <- list(
        AGE=AGE ~ 1 + PV1MATH + SEX + I(PV1MATH^2) + AGE_DA + HISEI_DA,
        HISEI=HISEI ~ 1 + PV1MATH + SEX + AGE + I(PV1MATH^2) + I(AGE^2) +
                   I(AGE*PV1MATH) + AGE_DA + HISEI_DA
                     )


#-- synthesize data
mod <- miceadds::syn_da( dat=dat, syn_vars=syn_vars, fix_vars=fix_vars,
            ord_vars=ord_vars, da_noise=da_noise,
            formula_syn=formula_syn, imp_maxit=2, use_pls=TRUE, ncomp=20,
            exact_regression=TRUE, exact_marginal=TRUE)
str(mod)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
