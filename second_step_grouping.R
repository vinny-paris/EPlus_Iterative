#takes in the param data matrix produced by created by "create_grouping_exp
#, the number of new groups you want, and the previous sig_groups you found 
#using Lenth. In return you get an updated version of the output of
#create_grouping_exp, that is a reduced, regrouped param data frame
#and a new design matrix for the next step in the process

grouping_it <- function(data, sig_groups = NULL, num_of_groups = 10){
  if(length(sig_groups) == 0) {
    sig_groups <- 1:max(data$Group)
    }
  new_groupss <- subset(data, data$Group %in% sig_groups)
  
  
 
  exp_data <- make_groups(group_size = num_of_groups, data = new_groupss) 
  design_matrix <- design_with_defaults(data_summary = exp_data, data = data) 
  
  new_groupss <- list(exp_data, design_matrix)
  
  names(new_groupss) <- c("param_data", "design_matrix")
  
  return(new_groupss)
}
