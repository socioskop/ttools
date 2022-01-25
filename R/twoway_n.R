#' twoway table of n indicator data
#' 
#' @param data data frame with the group identifier and n-counter
#' @param x the n count, should be just a konstant k=1, unless data is aggregated
#' @param group the name of the variable which identifies groups/arms.
#' 
#' @export
twoway_n <- function(data, x, group){
  
  # read data
  d <- data[!is.na(data[[group]]),]
  
  # ignore missings
  tab <- table(d[[x]], d[[group]])
  groups <- colnames(tab)
  if (nrow(tab)>1){stop("too many levels for n-count")}
  
  # build simple n-table
  tab <- c(rbind(tab, form.it(prop.table(tab, 2)*100, 1)))
  tab <- c(x, NA, tab, NA)
  tab <- as.data.frame(t(tab))
  
  # wrap up and return
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  return(tab)
}