



ma <- function(x){
  ma <- max(x)
  mi <- min(x)
  mz <- x == ma
  mze <- x == mi
  x[mz] <- 1
  x[mze] <- 0
  return(x)
}
  

choosey <- function(sig){
  k <- names(sig)
  l <- str_split(k, pattern = "")
  m <- map(l, length)
  n <- which(m < 5)
  o <- l[n]
  p <- map(o, function(x) x[-1])
  q <- unique(unlist(p))
  return(q)
}

balloon <- function(x) { 
  
  u <- unlist(base::strsplit(R.utils::intToBin(0:(2^x - 1)),
                             split = ""))
  
  v <- matrix(u, byrow = TRUE, ncol = x)
  
  return(v)
}

sm <- function(x){
  x[1:6, 1:6]
}
