# ZACHARY A. LUTE ; PROBLEM SET 2 ; MSE 524 ; OCT 18, 2016

rm(list=ls())

selsort <- function(v){
  for(i in 1:length(v)){
    currentmin = min(v[i:length(v)])
    for(j in 1:length(v)){
      if(v[j] == currentmin){
        temp = v[i]
        v[i] = v[j]
        v[j] = temp
      }
    }
  }
  return(v)
}

bubblesort <- function(v){
  for(m in 1:length(v)){
    counter = 0
    repeat{
      for(i in 1:(length(v)-1)){
        j = i + 1
        if(v[i] > v[j]){
          temp = v[i]
          v[i] = v[j]
          v[j] = temp
          counter = counter + 1
        }
      }
      if(counter == 0){
        return(v)
      }else{
        break
      }
    }
  }
  return(v)
}

v = rnorm(5000)
selsort(v)
bubblesort(v)
v2 <- 1:5000
v2[10] <- -4
v2[4900] <- 20000
selsort(v2)
bubblesort(v2)

# bubblesort(v) Run-Time is:    user  system   elapsed 
#                              42.549   0.467  43.271 
# selsort(v) Run-Time is:       user  system  elapsed 
#                               13.78    0.10   13.92 
# sort(v) Run-Time is:          user  system  elapsed 
#                               0.001   0.000   0.001
# bubblesort is the slowest function for sorting v because it requires a comparison
# of data of type numeric. This fact means that the probability that the function will
# need to swap the placement of the numbers is near 1. Therefore, it must perform the
# maximum number of ops that would be possible for bubblesort's method and does so in
# O(n^2). The number of ops for bubblesort is greater than those for selsort so even
# though they both take O(n^2), bubblesort takes longer than selsort.

# bubblesort(v2) Run-Time is:   user  system  elapsed 
#                               0.051   0.001   0.051 
# selsort(v2) Run-Time is:      user  system  elapsed 
#                               13.219   0.102  13.344 
# sort(v2) Run-Time is:         user  system   elapsed 
#                               0.001   0.000   0.000 
# selsort takes the longest to sort v2 because bubblesort is so much more efficient
# in this case. v2 is a vector of discreet values that are almost in order already.
# This means that it does not need to perform many ops (those involved in switching)
# or complete as many full iterations of the nested loops in order to sort v2 because
# the function loop breaks when no more swaps are necessary. For bubblesort, sorting
# v2 takes nearer to O(n) than O(n^2). selsort takes the same number of ops to sort v 
# as it does to sort v2 so it takes approximately the same time even though v is 
# nearly sorted; big O notation is still O(n^2) for selsort(v2).