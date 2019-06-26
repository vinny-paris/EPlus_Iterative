data <- read.csv("./data/data_update.csv", header = TRUE)
design_matrix <- read.csv("./data/design_matrix.csv", header = TRUE)[,-1]

response1 <- rnorm(1024)
g <- iterative_grouping(response1, data, design_matrix)


response2 <- rnorm(1024)
h <- iterative_grouping(response2, g[[1]], g[[2]])

response3 <- rnorm(1024)
i <- iterative_grouping(response3, h[[1]], h[[2]])


