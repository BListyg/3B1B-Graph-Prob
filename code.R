library(reshape2)
library(igraph)
library(dplyr)


row_sample <- function(x,n){
  if(x == n){
  return(
    c(x,
      sample(
        x = c(n-(n-1),n-1),
        size = 1,
        prob = c(0.5,0.5)
        )
      )
    )
  }
  else if(x == 1){
    return(
    c(x,
      sample(
        x = c(x+1,n),
        size = 1,
        prob = c(0.5,0.5)
        )
      )
    )
  }
  else{
    return(
      c(x,
        sample(
          x = c(x-1,x+1),
          size = 1,
          prob = c(0.5,0.5)
          )
        )
      )
    }
}

g <- function(n_nodes){
    apply(matrix(c(1:n_nodes)),
      1,
      row_sample,
      n = n_nodes) %>% 
  t %>% 
  graph_from_data_frame %>% 
  degree(mode = 'in')
}

check_g <- function(n_nodes){
  table(g(n_nodes) < 1)[2]
}

replicate(n = 10000, check_g(8)) %>% mean(x = ., na.rm = T)









