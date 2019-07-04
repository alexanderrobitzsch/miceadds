%% File Name: data.internet.Rd
%% File Version: 0.19

\name{data.internet}
\alias{data.internet}
\docType{data}
\title{
Dataset Internet
}
\description{
Dataset with items corresponding to internet attitudes.
}
\usage{
data(data.internet)
}

\format{
  A data frame with 281 observations on the following 22 variables.

  The format of the dataset is

\code{'data.frame':   281 obs. of  22 variables:} \cr
\code{ $ IN1 : num  1 5 2 3 1 3 2 3 2 1 ...} \cr
\code{ $ IN2 : num  4 3 2 7 7 4 4 7 4 3 ...} \cr
\code{ $ IN3 : num  4 5 4 2 1 2 5 2 2 4 ...} \cr
\code{ [...]} \cr
\code{ $ IN20: num  3 2 2 3 3 4 2 7 2 2 ...} \cr
\code{ $ IN21: num  3 3 6 5 4 4 5 5 6 5 ...} \cr
\code{ $ IN22: num  3 4 2 5 3 5 3 7 3 5 ...} \cr

}

\details{
The following text is copied from
\emph{http://people.few.eur.nl/groenen/Data/index.htm}

The data set is based on a questionnaire on attitudes towards the Internet. It
consists of evaluations of 22 statements about the Internet by 281 students at
Erasmus University Rotterdam. These data were gathered around 2002
before the wide availability of broadband Internet access in the Netherlands.
The statements were evaluated using a seven-point Likert scale, ranging
from 1 (completely disagree) to 7 (completely agree).

We would like to thank Peter Verhoef for making these data available.

Each variable (statement) is coded as follows:

1. Completely disagree \cr
2. Disagree \cr
3. Slightly disagree \cr
4. Neutral \cr
5. Slightly agree \cr
6. Agree \cr
7. Completely agree \cr

Internet items:

 1. Paying using Internet is safe\cr
 2. Surfing the Internet is easy\cr
 3. Internet is unreliable\cr
 4. Internet is slow\cr
 5. Internet is user-friendly\cr
 6. Internet is the future's means of communication\cr
 7. Internet is addictive\cr
 8. Internet is fast\cr
 9. Sending personal data using the Internet is unsafe\cr
10. The prices of Internet subscriptions are high\cr
11. Internet offers many possibilities for abuse\cr
12. The costs of surfing are high\cr
13. Internet offers unbounded opportunities\cr
14. Internet phone costs are high\cr
15. The content of web sites should be regulated\cr
16. Internet is easy to use\cr
17. I like surfing\cr
18. I often speak with friends about the Internet\cr
19. I like to be informed of important new things\cr
20. I always attempt new things on the Internet first\cr
21. I regularly visit websites recommended by others\cr
22. I know much about the Internet\cr
}


\source{
Peter Verhoef

\emph{http://people.few.eur.nl/groenen/Data/index.htm}
}


%\references{
%%  ~~ possibly secondary sources and usages ~~
%}


\examples{
data(data.internet)
# missing proportions
colMeans( is.na(data.internet) )
}


%% \keyword{datasets}
