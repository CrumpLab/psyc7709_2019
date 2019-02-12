# fizz buzz

# for loop
Rprof(tmp <- tempfile())

answer <- 1:100
for(i in 1:100){
  if(i%%3 == 0) answer[i] <-"fizz"
  if(i%%5 == 0) answer[i] <-"buzz"
  if(i%%5 == 0 & i%%3 ==0) answer[i] <-"fizzbuzz"
}
print(answer)

Rprof()
summaryRprof(tmp)

# vector indexing
Rprof(tmp <- tempfile())

sequence <- 1:100
answer <- sequence
answer[sequence[sequence%%3==0]] <- "fizz"
answer[sequence[sequence%%5==0]] <- "buzz"
answer[sequence[sequence%%5==0 & sequence%%3==0]] <- "fizzbuzz"
answer

Rprof()
summaryRprof(tmp)

# vector option 2
answer <- 1:100
answer[which(1:100%%3==0)] <- "fizz"
answer[which(1:100%%5==0)] <- "buzz"
answer[which(1:100%%5==0 & 1:100%%3==0)] <- "fizzbuzz"
answer




