#Distance between two sets.
#Version:0.1.0
#Last Update:6/5/2019

#
#
#Packages required: igraph.
#
#Initialization commands:
#library("igraph")
#source(file = file.choose())
#
#

#initailize packages
library("igraph")

#initialize the network
g <- read.graph(file.choose(), format = "pajek")

#dat <- read.csv(file.choose())
#m <- as.matrix(dat)
#m <- m[,-1] #delete first column
#net <- graph.adjacency(m,mode="directed",weighted=TRUE)
#w <- E(net)$weight
#td <- 0

#initialize set A
a <- read.csv(file.choose())

#initialize set B
b <- read.csv(file.choose())

#Calculate a total distance between two vectors.
distance_between_two_sets <- function(setA, setB) {
  td <- 0
  r <- matrix(nrow = lengths(setA) * lengths(setB), ncol = 3)
  
  for(i in 1:lengths(setA)) {
    #print(i)

    for(j in 1:lengths(setB)) {
      #print(j)
      temp <- get.shortest.paths(g, setA[[1]][i], setB[[1]][j])
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

