#Major function. Takes in the response vector, data and design_matrix
#(i.e. the param_data and design_matrix outputted by 
#create_grouping_exp fucntion), the number of groups you'd like (default
#is 10) and the max numer of old groups you want sutdied in the new 
#experiment iteration



iterative_grouping <- function(response, data, design_matrix, num_of_groups = 10, new_groups = 6){
  r <- Lenth(response, design_matrix = design_matrix, data = data, new_groups = new_groups)
  new_data <- grouping_it(data, sig_groups = r, num_of_groups = num_of_groups)
  return(new_data)
}
