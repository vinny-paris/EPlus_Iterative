#data for dataframe in param file notation
#num_of_groups number of clustered groups to be evaluated at each step
#new_groups numer of clustered groups to be brought forward to next stage to be reclustered 
#fractional_report number of unique variables to be reported once withen a tolerance of the number given (2)
#groups_interest number of unique classes in data frame to be reported (3)
#output is a list of three data frames: First being the classic experiment end product, Second being correspoings to (2), and 
#Thrid corresponding to (3).

grouping_wrapper <- function(data, num_of_groups = 10, new_groups = 6, fractional_report = 25, groups_interest = 6){
  
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
    if(length(unique(data$Class)) > groups_interest){final[[3]] <- data}
    cat(paste("Current Iteration: ", i, "\n", sep = ""))
    i <- i + 1
  }
  {
    #collect first section
    final[[1]] <- data
    
    names(final) <- c("Proposal for Pure Grouping Orthogonal Experiment", "Proposal for Fractional Experiment", "Proposal for Large Class Fractional Experiment") 
    
    cat("Grouping Experiment Completed")
    return(final)
  }
}




  
  
