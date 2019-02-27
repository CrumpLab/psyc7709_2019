
## Matt's Code

Rprof(tmp <- tempfile())


board <- matrix(0, ncol=3, nrow=3)

evaluate_board <- function(x){
  eval_columns <- colSums(x)
  eval_rows <- rowSums(x)
  eval_diags <- c(sum(c(board[1,1],board[2,2],board[3,3])),
                  sum(c(board[1,3],board[2,2],board[3,1])))
  all_sums <- c(eval_columns,eval_rows,eval_diags)
  find_winner <- all_sums[abs(all_sums)==3]
  if (length(find_winner) > 0){
    outcome <- 1
  } else {
    outcome <- 0
  }
  return(outcome)
}

save_outcome<-c()
choices <- c(1,-1,1,-1,1,-1,1,-1,1)
for(sims in 1:10000){

  board_index <- sample(1:9)
  player_choices <- rep(0,9)
  for(i in 1:9){
    player_choices[board_index[i]]<-choices[i]
    board <- matrix(player_choices, ncol=3, nrow=3)
    game_outcome <- evaluate_board(board)
    if (game_outcome == 1){
      break
    }
  }
  
  save_outcome[sims] <- game_outcome
}

sum(save_outcome)/10000


Rprof()
summaryRprof(tmp)


# Jeff's code

Rprof(tmp <- tempfile())

# Count the number of times the game is won
win_count <- 0
# Create a data frame of each of the possible ways one can win
winning_pos <- data.frame(c(1,2,3),c(1,4,7),c(1,5,9),c(4,5,6),c(7,8,9),c(2,5,8),c(3,6,9),c(3,5,7))
# Initialize a variable that signals breaks from multiple nested loops
end_loop <-FALSE
for (i in 1:10000) {
  # Initialize a count of completed necessary moves to win for each player; 
  # reset to 0 for each simulation
  count_p1 <- 0
  count_p2 <- 0
  # Creates a randomly filled board; no spot can be filled twice
  board <- sample(1:9, 9, replace=FALSE)
  # Assigns every other move to player 1 and player 2;
  # player 2 concatenation with 0 enables vector to be used in for(l in 1:5) loop
  board_p1 <- board[c(1,3,5,7,9)]
  board_p2 <- c(board[c(2,4,6,8)],0)
  # Check each possible way to win
  for (j in 1:8) {
    # Check if each of the three required moves is filled
    for (k in 1:3) {
      # For each of player 1's moves, check if it matches one of the 3 required moves
      for (l in 1:5) {
        # If the player 1 move matches a required move, add to the player count
        if (board_p1[l] == winning_pos[k,j]) {
          count_p1 <- count_p1 + 1
        }
        # If the player 1 move matches a required move, add to the player count
        if (board_p2[l] == winning_pos[k,j]) {
          count_p2 <- count_p2 + 1
        }
      }
      # If all 3 required moves to win are completed, add to win count,
      # assign end_loop to TRUE to signal to break out of the for(j in 1:8) loop,
      # and break out of the for(k in 1:3) loop
      if (count_p1 == 3) {
        win_count <- win_count + 1
        end_loop <- TRUE
        break
      }
      # The above break ensures that if the board is randomly filled so that both 
      # players win, the if statement below will not run, and win count will not 
      # be increased again
      if (count_p2 == 3) {
        win_count <- win_count + 1
        end_loop <- TRUE
        break
      }
    }
    # Resets completed necessary moves to 0; this ensures that 2 required moves from
    # the first possible way to win are not carried over to the second possible way to win
    count_p1 <- 0
    count_p2 <- 0
    # Breaks the for(j in 1:8) loop; once a winning combination is found, the rest will not
    # be checked. This ensures that if the game is won in 2 different ways, it will only 
    # be counted as 1 win
    if (end_loop == TRUE) {
      end_loop <- FALSE
      break
    }
  }
}
# Divides the win count by 10,000 to create a probability that the game is won
win_prob <- win_count/10000
print(win_prob)

Rprof()
summaryRprof(tmp)


# number of states


  for(t1 in 1:9){
    board <-matrix(0,nrow=3,ncol=3)
    board[t1]<-1
    board1 <- board
    for(t2 in 1:9){
      board <- board1
      if(t2 != t1) board[t2]<- -1
      print(board)
    }
  }

t1 <- 9
t2 <- t1*8
t3 <- t2*7
t4 <- t3*6
t5 <- t4*5
t6 <- t5*4
t7 <- t6*3
t8 <- t7*2
t9 <- t8


bigram_probs <- matrix(0,nrow=26,ncol=26)
for(i in 1:26){
  rnums<-runif(26,0,1)
  bigram_probs[i,]<-rnums/sum(rnums)
}

row.names(bigram_probs)<-unlist(strsplit("abcdefghijklmnopqrstuvwxyz",split=""))
colnames(bigram_probs)<-unlist(strsplit("abcdefghijklmnopqrstuvwxyz",split=""))

sample_word <- "cat"
letters <- unlist(strsplit(sample_word,split=""))
probs<-c()
for(i in 1:length(letters)-1){
  probs[i] <- bigram_probs[letters[i],letters[i+1]]
}
prod(probs)



a <- c(1,2,3,4,5)
b <- c(-1,2,3,4,6)
plot(a,b) #a=x axis, b=y axis
cor(a,b)  # pearson's r
cor(a,b)^2  # r^2 amount of variance explained
df<-data.frame(a,b)

plot(a,b)
coef(lm(b~a, data=df))
plot(b,a)
coef(lm(b~a, data=df))

library(ggplot2)
ggplot(df, aes(x=a,y=b))+
  geom_point()+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm",se=FALSE)+
  coord_cartesian(xlim=c(-2,10),ylim=c(-2,10))

ggplot(df, aes(x=b,y=a))+
  geom_point()+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm",se=FALSE)+
  coord_cartesian(xlim=c(-2,10),ylim=c(-2,10))+
  scale_x_continuous(breaks=-2:10)+
  scale_y_continuous(breaks=-2:10)+
  theme(text = element_text(size=20))

coef(lm(a~b))

b=5

(5*.59) +1.32


