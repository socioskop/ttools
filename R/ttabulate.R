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
ttabulate <- function(data, xs, treat, weight=NULL, num=NA, cat=NA, bin=NA, dichotomize=NA, cal.date=NA, cens=5, show.na=F, na.count=F, test="auto", shapiro.p=0.0001){
  data <- as.rdf(data)
  t <- data.frame()
  for (x in xs){
    print(paste0("working on ", x))
    
    # generate segment
    if (x %in% "n"){try(t <- dplyr::bind_rows(t, twoway_n  (data=data, x, treat)))}
    if (x %in% num & !x %in% cal.date           ){try(t <- dplyr::bind_rows(t, twoway_num(data=data, x, treat, weight=weight, digit.m = 2, digit.sd = 2, cal.date==F, test=test, shapiro.p=shapiro.p)))}
    if (x %in% cal.date                         ){try(t <- dplyr::bind_rows(t, twoway_num(data=data, x, treat, weight=weight, digit.m = 2, digit.sd = 2, cal.date==T, test=test, shapiro.p=shapiro.p)))}
    if (x %in% cat                              ){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, show.na=show.na)))}
    if (x %in% dichotomize & !x %in% bin        ){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, force.two=T, show.na=show.na)))}
    if (x %in% bin &         !x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, bin=T, show.na=show.na)))}
    if (x %in% bin &          x %in% dichotomize){try(t <- dplyr::bind_rows(t, twoway_chi(data=data, x, treat, cens=cens, force.two=T, bin=T, show.na=show.na)))}
    
    # add count of missings
    if (na.count==T){
      groups <- unique(na.omit(data[[treat]]))
      groups <- groups[order(groups)]
      nas <- as.character()
      for (g in groups){
        nas <- c(nas, sum(is.na(data[[x]][data[[treat]]==g & !is.na(data[[treat]])])))
      } 
      t$nas[nrow(t)] <- paste0(nas, collapse="/")  
    } 
  }
  
  # formatting
  if (na.count==T){
    t <- cbind(t[,colnames(t)!="nas"], t$nas)
    colnames(t)[colnames(t) %in% c("nas", "t$nas")] <- paste0("NA's for ", paste0(groups, collapse="/")) 
  } 
  
  # align formatting
  t <- t %>% dplyr::mutate_if(.predicate = is.ok.num & !is.ok.date, function(x) as.num(as.chr(x)))
  t <- t %>% dplyr::mutate_if(.predicate = is.factor, function(x)       (as.chr(x)))
  return(t)
}
