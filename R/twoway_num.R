#' twoway table of numeric data
#' 
#' @param data data frame
#' @param x the numeric variable to be summarized
#' @param group the variable which identifies treatment/comparison groups 
#' @param digit.m number of digits on mean estimates
#' @param digit.sd number of digits for estimates of the standard deviation
#' @param cal.date logical indicating that the x variable should be treated as a date. Will then show the mean value as an actual date.
#' @import matrixStats 
#' @import lubridate  
#' 
#' @export 
twoway_num <- function(data, x, group, weight, digit.m=1, digit.sd=1, cal.date=F, test="auto", shapiro.p=.0001){
  
  # ensure rdf object, add/identify weight
  if (is.null(weight)){data$weight <- 1; weight <- "weight"} else {data$weight <- data[[weight]]; data[[weight]] <- NULL; weight <- "weight"}
  data <- as.rdf(data)[!is.na(data[[group]]),] 
  
  groups <- unique(na.omit(data[[group]]))
  
  # placeholder for groups
  tab <- rep(NA, 2*length(groups)+2)
  tab[1] <- x
  tab[2] <- NA
  
  # get mean and SD 
  k <- 3
  for (i in 1:length(groups)){
    
    # add mean, SD and median (in date format if x is Date)
    if (cal.date==F){
      tab[k+0] <- form.it(matrixStats::weightedMean  (data[[x]][data[[group]]==groups[i]], w=data$weight[data[[group]]==groups[i]], na.rm=T), digit.m)
      tab[k+1] <- form.it(matrixStats::weightedSd    (data[[x]][data[[group]]==groups[i]], w=data$weight[data[[group]]==groups[i]], na.rm=T), digit.sd)
      tab[k+2] <- form.it(matrixStats::weightedMedian(data[[x]][data[[group]]==groups[i]], w=data$weight[data[[group]]==groups[i]], na.rm=T), digit.sd)
    } else {
      tab[k+0] <- as.chr(lubridate::as_date(round(matrixStats::weightedMean  (as.num(data[[x]][data[[group]]==groups[i]]), w=data$weight[data[[group]]==groups[i]], na.rm=T))))
      #tab[k+1] <- form.it(matrixStats::weightedSd    (as.num(as.Date(as.chr(data[[x]][data[[group]]==groups[i]]))), w=data$weight[data[[group]]==groups[i]], na.rm=T), digit.sd)
      #tab[k+2] <- as.chr(lubridate::as_date(round(matrixStats::weightedMedian(as.num(as.Date(as.chr(data[[x]][data[[group]]==groups[i]]))), w=data$weight[data[[group]]==groups[i]], na.rm=T))))
    }
    
    # move on for next group
    k <- k+3
  }

  # decide which test to use
  if (test=="auto"){
    if(shapiro.test(resid(glm(paste0("as.num(", x, ")~factor(", group, ")"), data=data)))$p.value<shapiro.p){
      test <- "rank"
    } else {test <- "ttest"}
  }
  
  # adding inferential test
  if (test=="rank"){
    tab[length(tab)+1] <- tryCatch({form.it(
      ordinal:::anova.clm(ordinal::clm(paste0("as.factor(", x, ")~as.factor(", group, ")"), weights = data[["weight"]], data=data, link = "logit"))$`Pr(>Chisq)`, 3)}, error=function(err) NA)
  } else if (test=="ttest"){
    yy <- as.num(data[,x])
    xx <- data[,group]
    weights <- data[,weight]
    m <- glm(yy ~ xx, weights = weights)
    tab[length(tab)+1] <- tryCatch({form.it(as.numeric(na.omit(lmtest::lrtest(m)$`Pr(>Chisq)`)), 3)}, error=function(err) NA)
  }
  
  # wrap up
  tab <- as.data.frame(t(tab))
  colnames(tab) <- c("var", "level", rbind(paste0(groups, ".mean/n"), paste0(groups, ".sd/%"), paste0(groups, ".median")), "p.val")
  tab$test <- test
  return(tab)
}
