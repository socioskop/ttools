#' raw data pull for DST data
#' 
#' @param path path to raw data dir
#' @param reg base name of registry (data file base word, when [0-9] are removed)
#' @param years a vector of the years (e.g. 1991:2021) or other regular definitions of time periods covered by registries. For DREAM, this would be CCYYMM-CCYYMM.
#' @param pop a vector of identifiers, which the data extracted will be limited to
#' @param vars vector of variable names to be kept 
#' @param s.id name of the variable that contains the ids in source data
#' @param show.vars logical to enable printing of variable names in source data.
#' 
#' @importFrom foreach "%do%"
#'  
#' @export
grab_dst <- function(path, reg, years, pop, vars=NULL, s.id=NULL, show.vars=F){
  # reformat args
  if (substr(path, nchar(path), nchar(path))!="/"){path <- paste0(path, "/")}
  if (is.null(s.id)){s.id <- "pnr"}
  
  # get file list
  files <- list.files(path, pattern=reg)
  files <- files[grepl(paste0(as.character(years), collapse="|"), files)]
  
  # pull to list
  ff <- foreach::foreach(file = files) %do% {
    f <- haven::read_sas(paste0(path, file))
    colnames(f) <- tolower(colnames(f))
    if (show.vars==T & file==files[1]){print(colnames(f))}
    f$year <- paste0(stringr::str_extract_all(file, "[0-9]")[[1]][1:4], collapse="")
    f <- f[f[[s.id]] %in% pop,]
    if (!is.null(vars)){f <- f[,vars]}
    f
  }
  f <- dplyr::bind_rows(ff)
  return(f)
}