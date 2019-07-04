#Distance between two sets.
#Version:0.1.2
#Last Update:6/26/2019

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

#find distance between two sets
find_distance_between_two_sets <- function(setA, setB) {
  td <- 0
  
  for(i in 1:lengths(setA)) {
    for(j in 1:lengths(setB)) {
      temp <- get.shortest.paths(graph, setA[[1]][i], setB[[1]][j])
      td <- td + lengths(temp[[1]]) - 1
    }
  }
  
  return(td)
}

#Calculate a total distance between two vectors.
calculate_distance_between_two_sets <- function(setA, setB) {
  td <- 0
  r <- matrix(nrow = lengths(setA) * lengths(setB), ncol = 3)
  
  #setA to setB
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
    #print(lengths(setA))
    #print(lengths(setB))
  }
  
  a_to_b <<- as.data.frame(r)
  print(paste("The total distance from ", colnames(setA), " to ", colnames(setB), " is: ", td, sep=""))
  
  #setB to setA
  td <- 0
  
  for(i in 1:lengths(setB)) {
    #print(i)
    
    for(j in 1:lengths(setA)) {
      #print(j)
      temp <- get.shortest.paths(graph, setB[[1]][i], setA[[1]][j])
      td <- td + lengths(temp[[1]]) - 1
      #print(td)
      
      r[(i - 1) * lengths(setA) + j,] <- c(setB[[1]][i], setA[[1]][j], lengths(temp[[1]]) - 1)
      #print((i - 1) * lengths(setB) + j)
    }
    #print(td)
    #print(lengths(setA))
    #print(lengths(setB))
  }
  
  b_to_a <<- as.data.frame(r)
  print(paste("The total distance from ", colnames(setB), " to ", colnames(setA), " is: ", td, sep=""))
}

#show paths between setA and setB
show_paths <- function(setA, setB) {
  #show paths from setA to setB
  path <- NULL
  if(nrow(subset(a_to_b, distance > 0))) {
    path <- cbind(subset(a_to_b, distance > 0), path = 0)
    
    for(i in 1:nrow(path)) {
      path[i, 4] <- toString(get.shortest.paths(graph, path[i, 1], path[i, 2])[["vpath"]][[1]])
    }
    
    a_to_b_path <<- path
  }else {
    a_to_b_path <<- path
    print(paste("There is no path from ", colnames(setA), " to ", colnames(setB), sep=""))
  }
  
  #show paths from setB to setA
  path <- NULL
  if(nrow(subset(b_to_a, distance > 0))) {
    path <- cbind(subset(b_to_a, distance > 0), path = 0)
    
    for(i in 1:nrow(path)) {
      path[i, 4] <- toString(get.shortest.paths(graph, path[i, 1], path[i, 2])[["vpath"]][[1]])
    }
    
    b_to_a_path <<- path
  }else {
    b_to_a_path <<- path
    print(paste("There is no path from ", colnames(setB), " to ", colnames(setA), sep=""))
  }
}

#find assignee's patents
find_patent <- function(name) {
  set <- which(assignee == name, arr.ind = TRUE)
  set <- set[, -2]
  set <- as.data.frame(assignee[set, 3])
  colnames(set) <- name
  return(set)
}

#calculate distance between two assignees
calculate_distance_between_two_assignees <- function(assigneeA, assigneeB) {
  set_a <<- find_patent(assigneeA)
  set_b <<- find_patent(assigneeB)
  
  calculate_distance_between_two_sets(set_a, set_b)
  colnames(a_to_b) <<- c(assigneeA, assigneeB, "distance")
  colnames(b_to_a) <<- c(assigneeB, assigneeA, "distance")
  
  show_paths(set_a, set_b)
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

#find patent number with pajek label
#note that column name of pajek label must be "vaule"
find_patent_number <- function(pajek_label) {
  return(assignee[assignee$value==pajek_label,1])
}

#find pajek label with patent number
#note that column name of patent number must be "PN"
find_pajek_label <- function(patent_number) {
  return(assignee[assignee$PN==patent_number,3])
}

#find all patent with each assignee
find_all_patent_with_each_assignee <- function(as) {
  assignee_ls <- NULL
  assignees <- as[!duplicated((as$ASSIGNEE)),]
  assignee_row <- nrow(assignees)
  
  for(a in 1:assignee_row) {
    assignee_ls <- c(assignee_ls, as.data.frame(find_patent(toString(assignees[a,2]))))
  }
  
  return(assignee_ls)
}

#find all distance among assignees
find_all_distance_among_assignees <- function() {
  distance_m <- as.data.frame(matrix(, nrow = length(assignee_list), ncol = length(assignee_list)))
  
  for(i in 1:length(assignee_list)) {
    for(j in 1:length(assignee_list)) {
      distance_m[i,j] <- find_distance_between_two_sets(as.data.frame(assignee_list[[i]]), as.data.frame(assignee_list[[j]]))
      print(paste(assignee_list[[i]], assignee_list[[j]], distance_m[i,j], sep="   "))
      distance_m[j,i] <- find_distance_between_two_sets(as.data.frame(assignee_list[[j]]), as.data.frame(assignee_list[[i]]))
    }
  }
  
  return(distance_m)
}

#calculate distance between all assignee
calculate_distance_among_all_assignee <- function(a) {
  assignee_list <<- find_all_patent_with_each_assignee(a)
  distance_matrix <<- find_all_distance_among_assignees()
}
