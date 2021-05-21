#' raw data pull for DST data
#' 
#' @param path path to raw data dir
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
  ff <- foreach(file = files) %do% {
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