micombine.chisquare <- function( dk , df , display = TRUE ,
			version = 1){
        # INPUT:
        # dk    ... vector of chi square values resulting from multiply imputed data
        # df    ... degrees of freedom of chi square statistics
        M <- length(dk)
		
		if (version == 0){
			mean.dk <-  mean( dk )
			sdk.square <- stats::var( sqrt( dk ) )
			Dval <- ( mean.dk / df  - ( 1 - 1/M) * sdk.square )/ ( 1+ ( 1 + 1/M) * sdk.square )
			df2 <- ( M - 1 )/ df^(3/M)  * ( 1  + M / ( M + 1/M) / sdk.square )^2
		}
		if (version == 1){

			##--- SAS code from Paul Allison, see text below
			  ##    m=ncol(g2);
			  ##    g=sqrt(g2);
			  ##    mg2=sum(g2)/m;
			  ##    r=(1+1/m)*(ssq(g)-(sum(g)**2)/m)/(m-1);
			  ##    F=(mg2/df - r*(m+1)/(m-1))/(1+r);
			  ##    DDF=(m-1)*(1+1/r)**2/df**(3/m);
			  ##    P=1-probf(f,df,ddf);			
			g2 <- dk
			m <- length(g2);
			g <- sqrt(g2);
			mg2 <- sum(g2)/m;
			r <- (1+1/m)*(sum(g^2)-(sum(g)^2)/m)/(m-1);
			Dval <- (mg2/df - r*(m+1)/(m-1))/(1+r);
			df2 <-  (m-1)*(1+1/r)**2/df^(3/m);
		}	
        pval <- stats::pf( Dval , df1 = df , df2 = df2 , lower.tail = FALSE)

		#--- chi square approximation
		chisq.approx <- Dval * df
		p.approx <- 1 - stats::pchisq( chisq.approx , df=df )
        res <- c( "D" = Dval , "p" = pval , "df" = df , "df2" = df2 )
        if (display){   
            cat("Combination of Chi Square Statistics for Multiply Imputed Data\n")
            cat(paste( "Using" , M , "Imputed Data Sets\n"))
            cat( paste( "F(",df,", ", round(df2,2),")", "=" , round( Dval , 3 ) , 
                            "     p=" , round(pval,5) , sep="") , "\n" )
                }
        invisible(res)
        }

		
		
  ##  
  ##  
  ##  
  ##  Homepage
  ##  http://statisticalhorizons.com/wp-content/uploads/2012/01/combchi.sas
  ##  
  ##  
  ##  
  ##  %macro combchi(df=,chi=,data=);
  ##  /******************************************************************************
  ##  Version 2.1, 12-17-2007.  This version allows one to specify the name of a data set 
  ##  containing chi-square values.  This data set should have a single variable (with
  ##  any name) containing the chi-square values on multiple records. This version also
  ##  corrects an error in the formula that has shown up in a couple of publications. 
  ##  The correct version of the formula is in the original (1991) article by Meng,
  ##  Raghunathan and Rubin.  Incorrect formulas are given in Meng and Rubin (1992) as
  ##  well as the book Statistical Analysis with Missing Data by Little and Rubin (2002). 
  ##  
  ##  Report any errors, problems
  ##  or questions to the e-mail address below. The latest version of this macro
  ##  can be found at the web site below:
  ##  
  ##  Author:  Paul D. Allison, University of Pennsylvania
  ##           allison@ssc.upenn.edu
  ##           http://www.ssc.upenn.edu/~allison/
  ##  
  ##  MACRO COMBCHI combines chi-square statistics from an analysis of several data
  ##  sets created by multiple imputation, using the method described on p. 115 of
  ##  
  ##  Schafer, J.L. (1997) Analysis of Incomplete Multivariate Data.  London: 
  ##     Chapman and Hall. 
  ##  
  ##  Note 
  ##  
  ##  The chi-square statistics can be either Wald statistics or likelihood ratio
  ##  statistics.  All that's needed are the several chi-square values and the
  ##  degrees of freedom. 
  ##  
  ##  COMBCHI requires the installation of IML.
  ##  
  ##  Example of usage:
  ##  
  ##  A. Suppose a 3 d.f. test on four data sets produces chi-squares of 5.8, 7.2,
  ##  6.1 and 8.5.  Submit the statement:
  ##  
  ##     %combchi(df=3, chi=5.8 7.2 6.1 8.5)
  ##  
  ##  The following output is printed
  ##  
  ##               F        DF       DDF
  ##  
  ##       2.0540138         3 342.22381
  ##  
  ##  
  ##                     P
  ##  
  ##                 0.1060954
  ##  
  ##  The macro calculates an F-statistic of 2.05 with 3 and 342 degrees of freedom.  
  ##  The associated p-value is .106.
  ##  
  ##  B. Alternatively, reading the chi-square values into a sas data set:
  ##  
  ##  data chidata;
  ##  input x;
  ##  datalines;
  ##  5.8
  ##  7.2
  ##  6.1
  ##  8.5
  ##  ;
  ##  %combchi(df=3, data=chidata)
  ##  
  ##  *****************************************************************************/
  ##  proc iml;
  ##    DF=&df;
  ##    %if &chi ^= and &data ^= %then %do;
  ##      print, "Error: Can't specify both CHI= and DATA=";
  ##      abort;
  ##    %end;
  ##    %if &chi ^= %then %do; g2={&chi}; %end;
  ##    %if &data ^= %then %do; 
  ##       use &data;
  ##       read all into g1;
  ##       g2=g1`; 
  ##    %end;
  ##    m=ncol(g2);
  ##    g=sqrt(g2);
  ##    mg2=sum(g2)/m;
  ##    r=(1+1/m)*(ssq(g)-(sum(g)**2)/m)/(m-1);
  ##    F=(mg2/df - r*(m+1)/(m-1))/(1+r);
  ##    DDF=(m-1)*(1+1/r)**2/df**(3/m);
  ##    P=1-probf(f,df,ddf);
  ##    print f df ddf;
  ##    print p;
  ##  run;
  ##  quit;
  ##  %mend combchi;
		
