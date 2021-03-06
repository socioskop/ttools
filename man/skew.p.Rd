% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tgears.R
\name{skew.p}
\alias{skew.p}
\title{calculate bootstrapped (non-parametric) ci and p-values for differences between two groups}
\usage{
skew.p(data, y, x, rep = 1000, stat = "median")
}
\arguments{
\item{y}{outcome variable to test differences on}

\item{x}{grouping variable (coded 1 and 0 for reference group)}

\item{rep}{number of bootstrap draws}

\item{stat}{statistic to compare differences on}

\item{d}{data that includes outcome and grouping variable}
}
\value{
returns rdf row with a p-value and lower and upper confidence interval limits
}
\description{
calculate bootstrapped (non-parametric) ci and p-values for differences between two groups
}
