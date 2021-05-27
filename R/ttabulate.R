#' wraps up (align & stack) a set of twoway tables
#' 
#' @param data data frame
#' @param xs list of variables that should be summarized
#' @param treat character variable indicating treatment/comparison groups
#' @param num character vector with names of those variables in xs that are to be summarized as numeric variables
#' @param cat character vector with names of those variables in xs that are to be summarized as categorical variables
#' @param bin character vector with names of those variables in xs that are to be summarized as binary variables
#' @param num dichotomize vector with names of those categorical variables in xs that should be dichotomized before summarizing 
#' 
#' @importFrom magrittr "%>%"
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom stats chisq.test
#' @importFrom stats coef
#' @importFrom stats ecdf 
#' @importFrom stats na.omit 
#' @importFrom stats pnorm 
#' @importFrom stats sd 
#' @importFrom stats wilcox.test
#' 
#' @export
ttabulate <- function(data, xs, treat, num=NA, cat=NA, bin=NA, dichotomize=NA, cal.date=NA, cens=5, show.na=F){
  data <- as.rdf(data)
  t <- data.frame()
  for (x in xs){
    print(paste0("working on ", x))
    if (x %in% "n"){try(t <- dplyr::bind_rows(t, twoway_n  (data=data, x, treat)))}
    if (x %in% num){try(t <- dplyr::bind_rows(t, twoway_num(data=data, x, treat, digit.m = 2, digit.sd = 2)))}
    if (x %in% cal.date){try(t <- dplyr::bind_rows(t, twoway_num(data=data, x, treat, digit.m = 2, digit.sd = 2, cal.date==T)))}
    if (x %in% cat){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, show.na=show.na)))}
    if (x %in% dichotomize & !x %in% bin){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, force.two=T, show.na=show.na)))}
    if (x %in% bin &         !x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, bin=T, show.na=show.na)))}
    if (x %in% bin &          x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, force.two=T, bin=T, show.na=show.na)))}
  }
  return(t)
}