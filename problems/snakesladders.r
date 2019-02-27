#snakes and ladders

SLboard <- c(0,0,8,0,0,11,0,0,9,2,0,0,0,-10,0,0,0,0,-11,0,0,-2,0,-8,0)
Games <- c()
for(i in 1:10000){
  currentPosition <- 1
  count <- 0
  while(currentPosition < 25){
    count <- count+1
    diceRoll <- sample(seq(1,6),1)
    if(currentPosition+diceRoll < 25){
      currentPosition <- (currentPosition+diceRoll+SLboard[currentPosition+diceRoll])
    }else{
      currentPosition <- currentPosition+diceRoll
    }
  }
  Games[i]<-count
}
mean(Games)

## take 2

sl_board <- c(1,2,11,4,5,17,7,8,18,12,11,12,13,4,15,16,17,18,
              8,20,21,20,23,16,25,26,27,28,29,30,31,32)
game_outcomes <-c()
for(sims in 1:10000){
  count <- 0
  position <- 1
  while(position < 25){
    count <- count+1
    roll <- sample(seq(1,6),1)
    position <- sl_board[position+roll]
  }
game_outcomes[sims] <- count
}
mean(game_outcomes)

## take 3

sl_board <- c(1,2,11,4,5,17,7,8,18,12,11,12,13,4,15,16,17,18,
              8,20,21,20,23,16,25,26,27,28,29,30,31,32)

run_game <- function(a_board){
  count <- 0
  position <- 1
  while(position < 25){
    count <- count+1
    position <- a_board[position+sample(seq(1,6),1)]
  }
  return(count)
}

mean(replicate(10000,run_game(sl_board)))

## take 4

sl_board <- c(1,2,11,4,5,17,7,8,18,12,11,12,13,4,15,16,17,18,8,20,21,20,23,16,25,26,27,28,29,30,31,32)
run_game <- function(a_board){
  markers <-c(0,1) #count,position
  while(markers[2] < 25) markers <- c(markers[1]+1,a_board[markers[2]+sample(seq(1,6),1)])
  return(markers[1])}
mean(replicate(50000,run_game(sl_board)))


## take 5

b<- c(1,2,11,4,5,17,7,8,18,12,11,12,13,4,15,16,17,18,8,20,21,20,23,16,25,26,27,28,29,30,31,32)
r <- function(a){m <-c(0,1) 
  while(m[2]<25) m<-c(m[1]+1,a[m[2]+sample(1:6,1)])
  return(m[1])}
mean(replicate(10000,r(b)))


## jeff's code

move <- 0
count <- replicate(10000, 0)
game_grid <- data.frame(c(3,6,9,10,14,19,22,24),c(11,17,18,12,4,8,20,16))
for (i in 1:10000) {
  spot <- 1
  while (spot <= 25) {
    move <- sample(1:6, 1)
    spot <- spot + move
    for (i in 1:8) {
      if (spot == game_grid[i,1]) {
        spot <- game_grid[i,2]
      }
    }
    count[i] = count[i] + 1
  }
}
avg_moves <- mean(count)
print(avg_moves)


## random twitter code 1

M=matrix(rep(0,625),nrow=25)
for(x in 1:23){M[x,(x+1):min(24,(x+6))]=1/6}
M[20:24,25]=c(1/6*seq(2,6))
a=c(3,10,9,6,22,24,14,19)
b=c(11,12,18,17,20,16,4,8)
M[,b]=M[,b]+M[,a]
M[,a]=0
S=c(1,rep(0,24))
q=50
v=rep(0,q)
for(t in 1:q){
  S=S%*%M
  print(S)
  v[t]=S[25]
  }
sum(seq(1,q)*v)


###

# roll t

M=matrix(rep(0,625),nrow=25)
for(x in 1:23){M[x,(x+1):min(24,(x+6))]=1/6}
M <- M[1:12,1:12]
M[7:12,]<-0

S<-rep(0,12)
S[1]<-1
q=10
v=rep(0,q)
for(t in 1:q){
  S=S%*%M
  print(S)
  v[t]=S[2]
}
sum(seq(1,q)*v)


M<-read.table("problems/umbrella.txt")
M <-as.matrix(M)
S=rep(0,9)
S[5]<-1
q=1000
v=rep(0,q)
for(t in 1:q){
  S=S%*%M
  S
  v[t]=S[9]
}
sum(seq(1,q)*v)
hist(seq(1,q)*v)

