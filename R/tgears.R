
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
  
#' calculate bootstrapped (non-parametric) ci and p-values for differences between two groups 
#' 
#' @param d data that includes outcome and grouping variable
#' @param y outcome variable to test differences on
#' @param x grouping variable (coded 1 and 0 for reference group)
#' @param rep number of bootstrap draws
#' @param stat statistic to compare differences on
#' @return returns rdf row with a p-value and lower and upper confidence interval limits
#' @import foreach  
#' 
#' @export
skew.p <- function(data, y, x, rep=1000, stat="median"){
  
  # resampling of observed difference
  diff1 <- foreach (i = 1:rep, .combine='c') %do% {
    d0 <- sample(data[[y]][data[[x]]==0], sum(data[[x]]==0), replace=T)
    d1 <- sample(data[[y]][data[[x]]==1], sum(data[[x]]==1), replace=T)
    get(stat)(d1)-get(stat)(d0)
  }
  
  # get non-parametric CIs of difference
  ci <- quantile(diff1, c(.025, .975))
  
  # implicit two-sided test of difference, including correction
  p <- (sum(diff1>=0)+1)/(rep+1)
  
  # correct reverse falses
  if (get(stat)(diff1)<0){
    p <- abs(p)
  } else {
    p <- 1-p
  }
  
  # return as rdf row, convert p to 2-sided
  return(data.frame(p=p*2, ci_lo=ci[1], ci_hi=ci[2]))
} 

#' simulate / experiment with bootstrapping (non-parametric) of differences between two groups 
#' 
#' @param d data that includes outcome and grouping variable
#' @param y outcome variable to test differences on
#' @param x grouping variable (coded 1 and 0 for reference group)
#' @param rep number of bootstrap draws
#' @param stat statistic to compare differences on
#' @return returns rdf row with a p-value and lower and upper confidence interval limits
#' @import foreach  
#' 
#' @export
skew.sim <- function(data, y, x, rep=1000, stat="median"){
  
  # null difference
  diff0 <- foreach (i = 1:rep, .combine='c') %do% {
    d0 <- sample(data[[y]], sum(data[[x]]==0), replace=T)
    d1 <- sample(data[[y]], sum(data[[x]]==1), replace=T)
    get(stat)(d1)-get(stat)(d0)
  }
  
  # resampling of observed difference
  diff1 <- foreach (i = 1:rep, .combine='c') %do% {
    d0 <- sample(data[[y]][data[[x]]==0], sum(data[[x]]==0), replace=T)
    d1 <- sample(data[[y]][data[[x]]==1], sum(data[[x]]==1), replace=T)
    get(stat)(d1)-get(stat)(d0)
  }
  
  # get CIs of difference
  ci <- quantile(diff1, c(.025, .975))
  
  # implicit two-sided test of difference
  p  <- (sum(abs(diff0)>=abs(get(stat)(data[[y]][data[[x]]==1])-get(stat)(data[[y]][data[[x]]==0])))+1)/(rep+1)
  
  # other approach, one sided-test
  p1 <- (sum(diff1>=0)+1)/(rep+1)
  
  # references, relevant for
  w <- wilcox.test(y~treat, data=data)$p.value
  t <- t.test(y~treat, data=data)$p.value
  t1 <- t.test(y~treat, data=data, alternative="less")$p.value # one-sided t-test as reference
  
  # correct reverse falses
  if (get(stat)(diff1)<0){
    p1 <- abs(p1)
  } else {
    p1 <- 1-p1
  }  
  
  # return as rdf row 
  return(data.frame(p=p, p1=p1*2, ci_lo=ci[1], ci_hi=ci[2], w=w, t=t, t1=t1))
} 
