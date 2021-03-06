% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttabulate.R
\name{ttabulate}
\alias{ttabulate}
\title{wraps up (align & stack) a set of twoway tables}
\usage{
ttabulate(
  data,
  xs,
  treat,
  weight = NULL,
  num = NA,
  cat = NA,
  bin = NA,
  dichotomize = NA,
  dates = NA,
  cens = 5,
  show.na = F,
  na.count = F,
  test = "auto",
  shapiro.p = 1e-04
)
}
\arguments{
\item{data}{data frame}

\item{xs}{list of variables that should be summarized}

\item{treat}{character variable indicating treatment/comparison groups}

\item{num}{dichotomize vector with names of those categorical variables in xs that should be dichotomized before summarizing}

\item{cat}{character vector with names of those variables in xs that are to be summarized as categorical variables}

\item{bin}{character vector with names of those variables in xs that are to be summarized as binary variables}
}
\description{
wraps up (align & stack) a set of twoway tables
}
