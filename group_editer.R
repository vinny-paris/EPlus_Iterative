#Build group factorial experiment design matrix

design_with_defaults <- function(data_summary, data){

  
  new_groups <- data_summary
  groups <- new_groups$Group
  grp_size <- length(unique(groups))
  
  #make the 0/1 factorial design
  fact <- balloon(grp_size)
  
  #expand the factorial design so every variable in the
  # same group has the same column
  exp_01_char <- fact[,groups]
  exp_01      <- apply(exp_01_char, 1:2, as.numeric)
  
  #transform the 0/1's to p20/p80's
  exp_0big <- sweep(exp_01, 2, data_summary$p80, "*")
  exp_lit1 <- sweep(abs(exp_01 - 1), 2, data_summary$p20, "*")
  exp_correct <- exp_0big + exp_lit1
  
  #name the cols
  namey <- apply(data_summary[,3:5], 1, paste, collapse = "_")
  colnames(exp_correct) <- namey
  
  d <- dim(exp_correct)[1]
  useless_groups <- subset(data, !data$Group %in% sig_groups)
  
  x <- dim(useless_groups)[1]
  y <- dim(exp_correct)[1]
  J_1 <- matrix(rep(1, x * y), nrow = y, ncol = x)
  def <- useless_groups$Default
  defin <- t(t(J_1) * def)
  other_namey <- apply(useless_groups[,3:5], 1, paste, collapse = "_")
  colnames(defin) <- other_namey
  
  
  expment <- data.frame(exp_correct, defin)
  
  return(expment)
}
