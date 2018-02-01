.to_R_c <- function(x) {
  x <- substr(x, 2, nchar(x) - 1)
  return(unlist(strsplit(x, ', ')))
}
