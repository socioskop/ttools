#' twoway table of n indicator data for groupwide comparison
#' 
#' @param data a dataframe with a group variable and a count of individuals (e.g. n=1).
#' @param x the n-count variable, likely equal to 1 for any row included
#' @param group a character variable with categories corresponding to the groups being compared
#' 
#' @export
twoway_n <- function(data, x, group){
  d <- data[!is.na(data[[group]]),]
  tab <- table(d[[x]], d[[group]], useNA="ifany")
  groups <- colnames(tab)
  if (nrow(tab)>1){stop("too many levels for n-count")}
  tab <- c(rbind(tab, form.it(prop.table(tab, 2)*100, 1)))
  tab <- c(x, NA, tab, NA)
  tab <- as.data.frame(t(tab))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  return(tab)
}
