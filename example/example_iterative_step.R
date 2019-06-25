data <- read.csv("./data/data_update.csv", header = TRUE)
design_matrix <- read.csv("./data/design_matrix.csv", header = TRUE)[,-1]


l <- iterative_grouping(response, data, design_matrix)



