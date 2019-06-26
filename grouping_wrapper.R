grouping_wrapper <- function(data, num_of_groups = 10, new_groups = 6, fractional_report = 25){
  
  #warning message
  if(num_of_groups <= new_groups){stop("The number of groups (num_of_groups)
  must be larger than the number of groups chosen for the next 
                                       iteration (new_groups)")}
  
  
  #create initial experiment's data and design_matrix
  exper <- create_grouping_exp(data = data, num_of_groups = num_of_groups)
  data  <- exper[[1]]
  design_matrix <- exper[[2]]
  
  #prepare the holding list and indexer
  final <- NULL
  i <- 1
  
  #loop to cut down the data
  while(dim(data)[1] > num_of_groups) {
    #response <- eplusr(design_matrix)
    response <- rnorm(dim(design_matrix)[1])
    g <- iterative_grouping(response, data, design_matrix, num_of_groups, new_groups)
    data <- g[[1]]
    design_matrix <- g[[2]]
    if(.75*fractional_report <= dim(data)[1] && dim(data)[1] <= 1.5*fractional_report){final[[2]] <- data}
    cat(paste("Current Iteration: ", i, "\n", sep = ""))
    i <- i + 1
  }
  {
    #collect first section
    final[[1]] <- data
    
    names(final) <- c("Proposal for Pure Grouping Experiment", "Proposal for Fractional Experiment") 
    
    cat("Grouping Experiment Completed")
    return(final)
  }
}
  
  