library(RcppEigen)
library(Matrix)
library(stringr)
library(purrr)


#response is the energy consumption values per treatment

#design matrix is the design matrix from files

#data is the orginial param file

#new_groups is the max number of new groups you would like for the 
#next round of testing, defaults 6

Lenth <- function(response, design_matrix, data, new_groups = 6) {
  ind <- order(data$Group)[!duplicated(sort(data$Group))] 
  des <- design_matrix[,ind]
  x10 <- apply(des, 2, ma)
  rownames(x10) <- NULL
  colnames(x10) <- NULL
  
  
  #build the full design matrix with interactions built in
  f <- as.matrix(x10[,1])
  colnames(f) <- LETTERS[1]
  for(i in 2:dim(x10)[2]){
    j  <- cbind(1, f)
    colnames(j)[1] <- ""
    cc <- colnames(j)
    f_t <- (j + x10[,i]) %% 2
    colnames(f_t) <- paste(cc, sep = "", LETTERS[i])
    f <- cbind(f, f_t)
  }
  

  #calculate the betas and use lenths to find the inital sig. betas
  b <- lm(response ~ f)$coefficients
  
  s_initial <- 3.75 * median(abs(b))
  
  b_star <- subset(b, abs(b) < s_initial)
  pse <- 1.5 * median(abs(b_star))
  sig <- subset(b, abs(b) > 2*pse)
  
  q <- choosey(sig)
  
  #cut down group size to make sure each step is useful
  while(length(q) > new_groups){
    sig <- sig[abs(sig) > min(abs(sig))]
    q <- choosey(sig)
  }
  
  if(length(sig) == 0){
    q <- sig[abs(sig) == max(abs(sig))]
  }
  
  #convert letters into groups
  r <- unique(grep(paste(q, collapse = "|"), LETTERS, value = FALSE))
  
  return(r)
}




  