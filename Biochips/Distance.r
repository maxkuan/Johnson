library(igraph)
library(compare)
library(parallel)

START <- Sys.getenv("START")
END <- Sys.getenv("END")
TOTAL <- Sys.getenv("TOTAL")

network_filename <- "Biochips_Cited_Citing.net"
assignee_filename <- "Biochips_Assignee_All.csv"

input_path <- paste("./src/input/", sep = "")
output_path <- paste("/app/src/output/", START, "/", START, "_", END, ".csv", sep = "")

graph <- read.graph(file = paste(input_path, network_filename, sep = ""), format = "pajek")
assignee <- read.csv(file = paste(input_path, assignee_filename, sep = ""))
unreachable_constant <- -1

setA <- c(START:END)
setB <- c(1:TOTAL)

d <- matrix()

cpu.cores <- detectCores() - 1
cl <- makeCluster(cpu.cores)
clusterEvalQ(cl,library(igraph))
clusterEvalQ(cl,library(parallel))
clusterExport(cl, "graph")
clusterExport(cl,"unreachable_constant")
clusterExport(cl, "setB")

start_time <- Sys.time()

d <- parSapply(cl, setA, function(i) {
  d1 <- sapply(setB, function(j) {
    if(i > j) {
      d2 <- cbind(i, j, unreachable_constant)
    } else {
      temp <- get.shortest.paths(graph, i, j)
      d2 <- cbind(i, j, lengths(temp[[1]]) - 1)
    }
    return(d2)
  }, simplify = FALSE)
  return(d1)
}, simplify = FALSE)

stopCluster(cl)

d <- unlist(d, recursive = FALSE)

d <- matrix(unlist(d), ncol = 3, byrow = TRUE)

end_time <- Sys.time()

print(end_time - start_time)

print(output_path)

write.csv(d, file = output_path)