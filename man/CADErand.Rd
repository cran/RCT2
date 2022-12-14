% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CADErand.R
\name{CADErand}
\alias{CADErand}
\title{Randomization-based method for the complier average direct effect and the complier average spillover effect}
\usage{
CADErand(data, individual = 1, ci = 0.95)
}
\arguments{
\item{data}{A data frame containing the relevant variables. The names for the variables should be: ``Z'' for the treatment assignment,  ``D''  for the actual received treatment, ``Y'' for the outcome, ``A'' for the treatment assignment mechanism and ``id'' for the cluster ID. The variable for the cluster id should be a factor.}

\item{individual}{A binary variable with TRUE for  individual-weighted estimators and FALSE for cluster-weighted estimators.}

\item{ci}{A numeric variable between 0 and 1 for the level of the confidence interval to be returned.}
}
\value{
A list of class \code{CADErand} which contains the following items:
\item{CADE}{ The point estimates of the CADE for each assignment mechanism.  }
\item{CASE}{ The point estimate of CASE for each assignment mechanism.  } 
\item{var.CADE1}{ The  variance estimate of CADE for each assignment mechanism.   } 
\item{var.CASE1}{ The  variance estimate of CASE for each assignment mechanism.   } 
\item{DEY1}{ The point estimate of DEY for each assignment mechanism.  }
\item{DED1}{ The point estimate of DED for each assignment mechanism.  }
\item{var.DEY1}{ The  variance estimate of DEY for each assignment mechanism.   } 
\item{var.DED1}{ The  variance estimate of DED for each assignment mechanism.   } 
\item{SEY1}{ The point estimate of SEY for each pairwise groups of assignment mechanisms.  } 
\item{SED1}{ The point estimate of SED for each pairwise groups of assignment mechanisms.  }
\item{var.SEY1}{ The  variance estimate of SEY for each pairwise groups of assignment mechanisms.   } 
\item{var.SED1}{ The  variance estimate of SED for each pairwise groups of assignment mechanisms.   } 
\item{lci.CADE}{ The left endpoint for the confidence intervals for the CADE from each assignment mechanism. }
\item{rci.CADE}{ The right endpoint for the confidence intervals for the CADE from each assignment mechanism. }
\item{lci.CASE}{ The left endpoint for the confidence intervals for the CASE from each assignment mechanism. }
\item{rci.CASE}{ The left endpoint for the confidence intervals for the CASE from each assignment mechanism. }
\item{lci.DEY}{ The left endpoint for the confidence intervals for the DEY from each assignment mechanism. }
\item{rci.DEY}{ The left endpoint for the confidence intervals for the DEY from each assignment mechanism. }
\item{lci.SEY}{ The left endpoint for the confidence intervals for the SEY from each pairwise groups of assignment mechanisms. }
\item{rci.SEY}{ The left endpoint for the confidence intervals for the SEY from each pairwise groups of assignment mechanism. }
\item{lci.DED}{ The left endpoint for the confidence intervals for the DED from each assignment mechanism. }
\item{rci.DED}{ The left endpoint for the confidence intervals for the DED from each assignment mechanism. }
\item{lci.SED}{ The left endpoint for the confidence intervals for the SED from each pairwise groups of assignment mechanism. }
\item{rci.SED}{ The left endpoint for the confidence intervals for the SED from each pairwise groups of assignment mechanism. }
}
\description{
This function computes the point estimates and variance estimates of the complier average direct effect (CADE)  and the complier average spillover effect (CASE).
The estimators calculated using this function are either individual weighted or cluster-weighted. The point estimates and variances of ITT effects are also included.
}
\details{
For the details of the method implemented by this function, see the
references.
}
\examples{
data(india)
india$id <- factor(india$id)
CADErand(india, 0.95) 

}
\references{
Kosuke Imai, Zhichao Jiang and Anup Malani (2018).
\dQuote{Causal Inference with Interference and Noncompliance in the Two-Stage Randomized Experiments}, \emph{Technical Report}. Department of Politics, Princeton
University.
}
\author{
Kosuke Imai, Department of Statistics, Harvard University
\email{imai@harvard.edu}, \url{https://imai.fas.harvard.edu/};
Zhichao Jiang, School of Public Health and Health Sciences, University of Massachusetts Amherst
\email{zhichaojiang@umass.edu};
Karissa Huang, Department of Statistics, Harvard College
\email{krhuang@college.harvard.edu}
}
\keyword{experiments}
\keyword{randomized}
\keyword{two-stage}
