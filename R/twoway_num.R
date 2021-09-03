#' twoway table of numeric data
#' 
#' @param data data frame
#' @param x the numeric variable to be summarized
#' @param group the variable which identifies treatment/comparison groups 
#' @param digit.m number of digits on mean estimates
#' @param digit.sd number of digits for estimates of the standard deviation
#' @param cal.date logical indicating that the x variable should be treated as a date. Will then show the mean value as an actual date.
#' 
#' @export 
twoway_num <- function(data, x, group, digit.m=1, digit.sd=1, cal.date=F){
  d <- data[!is.na(data[[group]]),]
  groups <- unique(na.omit(d[[group]]))
  
  # placeholder for groups
  tab <- rep(NA, 2*length(groups)+2)
  tab[1] <- x
  tab[2] <- NA
  
  # get mean and SD 
  k <- 3
  for (i in 1:length(groups)){
    if (cal.date==F){tab[k+0] <- form.it(mean(d[[x]][d[[group]]==groups[i]], na.rm=T), digit.m)
    } else {
      tab[k+0] <- as.character(lubridate::as_date(round(mean(d[[x]][d[[group]]==groups[i]], na.rm=T))))
    }
    tab[k+1] <- form.it(  sd(d[[x]][d[[group]]==groups[i]], na.rm=T), digit.sd)
    k <- k+2
  }
  
  # adding inferential test
  if (length(groups)>2){
    tab[length(tab)+1] <- tryCatch({form.it(pnorm(abs(coef(summary(MASS::polr(paste0("as.factor(", x, ")~as.factor(", group, ")"), data=d)))[1,3]), lower.tail = F)*2, 3)}, error=function(err) NA)
  } else {
    tab[length(tab)+1] <- tryCatch({form.it(wilcox.test(as.formula(paste0("as.numeric(", x, ")~as.factor(", group, ")")), data=na.omit(d[,c(x, group)]))$p.value, 3)}, error=function(err) NA)
  }
  tab <- as.data.frame(t(tab))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%")), "p.val")
  return(tab)
}
