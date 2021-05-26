#' a set of generic functions that speed up coding 
#' 
#' check string data before converting to numeric
#'
#' returning true if character column can be converted to numeric without any losses
#' @export
is.ok.num <- function(x){
  (sum(!is.na(x))==sum(!is.na(as.numeric(as.character(x)))) & length(x[grepl(paste0(c(letters, "\\-"), collapse="|"), x)])==0)
}

#' check date column
#'
#' returning true is character column can be converted to date without any lost observations
#' @export
is.ok.date <- function(x){
  x <- x[!is.nan(x)]
  (sum(!is.na(x))==sum(!is.na(lubridate::as_date(x))))
}

#' short for conversion to character
#'
#' @export
as.chr <- function(x){as.character(x)}
is.chr <- function(x){is.character(x)}

#' short for conversion to numeric
#'
#' @export
as.num <- function(x){as.numeric(x)}
is.num <- function(x){is.numeric(x)}

#' short for conversion to dataframe
#'
#' @export
as.rdf <- function(x){as.data.frame(x)}

#' formats a numeric vector to a fixed string format with digits
#' @export
form.it <- function(x, digits=3, perc=F, max=NULL){
  if (!is.null(max)){
    x[x>max] <- Inf
  }
  x <- format(round(x, digits), nsmall=digits)
  if (perc==T){x <- paste0(x, "%")}
  return(x)
}

#' get cumulative distribution func
#' @return transforms a vector to its' quantiles
#' @export
get.q <- function(x){
  efunc <- ecdf(x)
  return(efunc(x))
}
