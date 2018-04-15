##############  Factorial_loop
Factorial_loop <- function(n){
  
  stopifnot(n >= 0)
  if(n == 0) return(1)
  
  fact <- 1
  for(i in n:1){
    
    fact <- fact*i
  }
  return(fact)
}


##############  Factorial_reduce
require(purrr)
Factorial_reduce <- function(n){
  stopifnot(n >= 0)
  if(n == 0) return(1)
  purrr::reduce(seq(n), function(x, y){
    
    x * y
  })
}

##############  Factorial_func
Factorial_func <- function(n){
  
  stopifnot(n >= 0)
  if(n == 0) return(1)
  if(n == 1) return(1)
  return( n * Factorial_func(n - 1))
}

##############  Factorial_mem

fact_tbl <- c(1, rep(NA, 23))
Factorial_mem <- function(n){
  
  stopifnot(n >= 0)
  if(n == 0){
    
    1
  }
  if(!is.na(fact_tbl[n])){
    
    fact_tbl[n]
  } else {
    
    fact_tbl[n - 1] <<- Factorial_mem(n - 1)
    n * Factorial_mem(n - 1)
  }
  
}

map_dbl(1:10, Factorial_mem)
##############


library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)


factor_loop_data <- map(1:15, function(x){microbenchmark(Factorial_loop(x), times = 1000)$time})
names(factor_loop_data) <- paste0(letters[1:15], 1:15)
factor_loop_data <- as.data.frame(factor_loop_data)

factor_loop_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))


factor_reduce_data <- map(1:15, function(x){microbenchmark(Factorial_reduce(x), times = 1000)$time})
names(factor_reduce_data) <- paste0(letters[1:15], 1:15)
factor_reduce_data <- as.data.frame(factor_reduce_data)

factor_reduce_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))



factor_func_data <- map(1:15, function(x){microbenchmark(Factorial_func(x), times = 1000)$time})
names(factor_func_data) <- paste0(letters[1:15], 1:15)
factor_func_data <- as.data.frame(factor_func_data)

factor_func_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))


factor_mem_data <- map(1:15, function(x){microbenchmark(Factorial_mem(x), times = 1000)$time})
names(factor_mem_data) <- paste0(letters[1:15], 1:15)
factor_mem_data <- as.data.frame(factor_mem_data)

factor_mem_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))


timeingTable <- data.frame(cbind(factor_func_data$num, factor_loop_data$med_time, factor_reduce_data$med_time, factor_func_data$med_time, factor_mem_data$med_time))
names(timeingTable)[2:ncol(timeingTable)] <- c('loop', 'reduce', 'recursive', 'mem')


library(reshape2)
library(ggplot2)
melted <- melt(timeingTable, id.vars = 'X1')

ggplot(melted, aes(x = X1, y = value, group = variable, color = variable)) + 
  geom_line()
