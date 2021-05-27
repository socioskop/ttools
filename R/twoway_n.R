#' twoway table of n indicator data
#' 
#' @param data data frame with the group identifier and n-counter
#' @param x the n count, should be just a konstant k=1, unless data is aggregated
#' @param group the name of the variable which identifies groups/arms.
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