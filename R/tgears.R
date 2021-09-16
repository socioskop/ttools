
#' calculate ci for skewed distribution point estimates
#' 
#' @param x variable to derive ci from
#' @param rep number of bootstrap draws
#' @param stat point estimate statistic to calculate ci for
#' @param alt one or two-sided testing
#' @param alpha alpha level
#' @return returns a vector of length 2 with lower and upper interval limits
#' @import boot  
#' 
#' @export
skew.ci <- function(x, rep=1000, stat="median", alt="two.sided", alpha=.95){
  
  if (stat=="median"){
    e <- boot::boot(data = x, statistic = function(x,i) median(x[i]), R = rep)
    e <- boot::boot.ci(e, type="perc")$percent[4:5] 
  } 
  
  return(e)
} 
  