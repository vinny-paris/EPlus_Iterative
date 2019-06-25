sm <- function(x){
  x[1:6, 1:6]
}


balloon <- function(x) { 
  
  u <- unlist(base::strsplit(R.utils::intToBin(0:(2^x - 1)),
                             split = ""))
  
  v <- matrix(u, byrow = TRUE, ncol = x)
  
  return(v)
}


response <- rnorm(1024)
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

sig <- subset(b, abs(b) > 2*pse)
q <- choosey(sig)

while(length(q) > 6){
  sig <- sig[sig > min(sig)]
  q <- choosey(sig)
}
  