#' Read in caribou data
#'
#' This is a wrapper function that reads in caribou data from csv files
#'
#' @param csv the directory to the csv file to read in
#'
#' @export
#'


caribouRead <- function(csv){
  raw <- read.csv(csv, header = TRUE, stringsAsFactors = FALSE)
  end <- which(raw$GROUP == "end")
  dat <- list()
  dat$df <- raw[1:(end-1), 1:3]
  dat[[1]]$COLLARS <- as.integer(dat[[1]]$COLLARS)
  dat[[1]]$CARIBOU <- as.integer(dat[[1]]$CARIBOU)
  dat[[1]]$YEAR <- stringr::str_extract(string = csv, pattern = "[:digit:]+")
  dat[[1]]$HERD <- stringr::str_extract(string = csv, pattern = "[:alpha:]+")
  dat$generalnotes <- raw[(end+1):nrow(raw),]
  missing <- suppressWarnings(na.omit(as.integer(
    sub(".*?MISSING COLLARS = .*?(\\d+).*", "\\1", dat$generalnotes)
  )))
  dat[[1]]$MISSINGCOLLARS <- rep(missing, end-1)
  dat[[1]]$TOTALCOLLARS <- sum(as.integer(dat[[1]]$COLLARS), missing)
  if(!is.null(raw$NOTES)){
    dat$casenotes <- raw$NOTES
  }
  suppressWarnings(
    for(i in 1:(end-1)){
      dat$df[i,2] <- ifelse(is.na(as.integer(dat$df[i,2])), 0, as.integer(dat$df[i,2]))
      dat$df[i,3] <- ifelse(is.na(as.integer(dat$df[i,3])), 0, as.integer(dat$df[i,3]))
    }
  )
  dat$df[['CARIBOU']] <- as.integer(dat$df[['CARIBOU']])
  dat$df <- dat$df[rev(order(dat$df[["CARIBOU"]])),]
  return(dat)
}
