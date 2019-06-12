#Distance between two sets.
#Version:0.1.1
#Last Update:6/12/2019

#
#
#Packages required: igraph.
#
#Initialization steps in R:
#1. library("igraph")
#2. source(file = file.choose()) (e.g. Biochips.r)
#3. select pajek network file (e.g. Biochips_Cited_Citing.net)
#4. select assignee file (e.g. Biochips_Assignee_Pajek.csv)
#
#

#initailize packages
library("igraph")

#initialize the network
graph <- read.graph(file.choose(), format = "pajek")

#dat <- read.csv(file.choose())
#m <- as.matrix(dat)
#m <- m[,-1] #delete first column
#net <- graph.adjacency(m,mode="directed",weighted=TRUE)
#w <- E(net)$weight
#td <- 0

#initialize assignee
assignee <- read.csv(file.choose())

#initialize set A
#a <- read.csv(file.choose())

#initialize set B
#b <- read.csv(file.choose())

#Calculate a total distance between two vectors.
calculate_distance_between_two_sets <- function(setA, setB) {
  td <- 0
  r <- matrix(nrow = lengths(setA) * lengths(setB), ncol = 3)
  
  for(i in 1:lengths(setA)) {
    #print(i)

    for(j in 1:lengths(setB)) {
      #print(j)
      temp <- get.shortest.paths(graph, setA[[1]][i], setB[[1]][j])
      td <- td + lengths(temp[[1]]) - 1
      #print(td)
      
      r[(i - 1) * lengths(setB) + j,] <- c(setA[[1]][i], setB[[1]][j], lengths(temp[[1]]) - 1)
      #print((i - 1) * lengths(setB) + j)
    }
    #print(td)
  }
  
  record <<- r
  paste("The total between setA and setB is: ", td, sep="")
}

#calculate distance between two assignees
calculate_distance_between_two_assignees <- function(assigneeA, assigneeB) {
  set_a <- which(assignee == assigneeA, arr.ind = TRUE)
  set_a <- set_a[, -2]
  set_a <<- as.data.frame(assignee[set_a, 3])
  set_b <- which(assignee == assigneeB, arr.ind = TRUE)
  set_b <- set_b[, -2]
  set_b <<- as.data.frame(assignee[set_b, 3])
  #calculate_distance_between_two_sets(set_a, set_b)
}

#detect wheather exist cycles in a graph
detect_cycle <- function(g) {
  cy <- NULL
  
  for(v1 in V(g)) {
    for(v2 in neighbors(g, v1, mode="out")) {
      cy <- c(cy, lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p)))
    }
  }
  
  cycles <<- cy
}