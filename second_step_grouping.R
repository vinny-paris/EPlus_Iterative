#takes in the param data matrix produced by created by "create_grouping_exp
#, the number of new groups you want, and the previous sig_groups you found 
#using Lenth. In return you get an updated version of the output of
#create_grouping_exp, that is a reduced, regrouped param data frame
#and a new design matrix for the next step in the process

grouping_it <- function(data, sig_groups = NULL, num_of_groups = 10){
  if(length(sig_groups) == 0) {
    sig_groups <- 1:max(data$Group)
    }
  new_groups <- subset(data, data$Group %in% sig_groups)
  

  new_groups <- create_grouping_exp(data = new_groups, num_of_groups = num_of_groups)
  names(new_groups) <- c("param_data", "design_matrix")
  
  return(new_groups)
}
