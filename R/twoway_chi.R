#' twoway table of categorical data
#' 
#' @param data dataset containing group factor and the x-variable to be summed up
#' @param x variable to be summed
#' @param bin logical indicator to only use last row of two-way table (FALSE is default)
#' @param cens positive integer deciding the lower threshold for when to clear cells in order to hide very detailed patient-level data
#' @param force.two logical to use for counts where values >1 should be considered 1 (then effectively testing and showing a binary variable).
#' @param show.na logical to use missing data as an explicit category (default FALSE)
#' @return returns a data.frame. Has standardized column names, ready to stack with output from other grit::twoway* functions.
#' 
#' @export
twoway_chi <- function(data, x, group, bin=F, cens, force.two=F, show.na=F) {
  # get data
  d <- data[!is.na(data[[group]]),]
  
  if (force.two==T){
    d[[x]] <- as.numeric(d[[x]]>=1)
  }
  
  if (is.null(cens)){cens <- -Inf}
  
  # generate table, p.value and labels
  tab <- table(as.character(d[[x]]), d[[group]], useNA=ifelse(show.na, "ifany", "no"))
  
  groups <- colnames(tab)
  if (nrow(tab)<2){stop("too few levels")}
  p <- form.it(chisq.test(tab)$p.value, 3)
  levels <- rownames(tab)
  
  # handle missings explicitly
  if (show.na==T){
    if (is.na(levels[length(levels)]) & sum(is.na(levels))==1) {levels[length(levels)] <- "N/A"}
  }
  
  # combine n and %
  tab <- matrix(rbind(tab, form.it(prop.table(tab, 2)*100, 1)), nrow=nrow(tab))
  
  # 
  if (sum(as.num(tab[,1])<=cens & as.num(tab[,1])>0)>=1 | sum(as.num(tab[,3])<=cens & as.numeric(tab[,3])>0)>=1){
    p <- paste0("n<", cens)
  }
  
  # add p value
  if (nrow(tab)>=2) {filling <- c(rep(NA, nrow(tab)-1), p)} else {filling <- p}
  if (nrow(tab)>=2 & bin==F){varname <- c(x, rep(NA, nrow(tab)-1))} else {varname <- rep(x, nrow(tab))}
  
  for (i in 1:nrow(tab)){
    tab[i,2][as.numeric(tab[i,1])<=cens] <- paste0("n<", cens)
    tab[i,4][as.numeric(tab[i,3])<=cens] <- paste0("n<", cens)
    tab[i,1][as.numeric(tab[i,1])<=cens] <- paste0("n<", cens)
    tab[i,3][as.numeric(tab[i,3])<=cens] <- paste0("n<", cens)
  }
  
  # wrap up, force regular colnames
  tab  <- as.data.frame(cbind(varname, levels, tab, filling))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  
  # pick last row if bin
  if (bin==T){tab <- tab[nrow(tab),]}
  return(tab)
}