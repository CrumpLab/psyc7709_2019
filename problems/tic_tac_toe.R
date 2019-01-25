

board <- matrix(0, ncol=3, nrow=3)

evaluate_board <- function(x){
  eval_columns <- colSums(x)
  eval_rows <- rowSums(x)
  eval_diags <- c(sum(c(board[1,1],board[2,2],board[3,3])),
                  sum(c(board[1,3],board[2,2],board[3,1])))
  all_sums <- c(eval_columns,eval_rows,eval_diags)
  find_winner <- all_sums[abs(all_sums)==3]
  if (length(find_winner) > 0){
    outcome <- "win"
  } else {
    outcome <- "claw"
  }
  return(outcome)
}

save_outcome<-c()
choices <- c(1,-1,1,-1,1,-1,1,-1,1)
for(sims in 1:1000){

  board_index <- sample(1:9)
  player_choices <- rep(0,9)
  for(i in 1:9){
    player_choices[board_index[i]]<-choices[i]
    board <- matrix(player_choices, ncol=3, nrow=3)
    game_outcome <- evaluate_board(board)
    if (game_outcome == "win"){
      break
    }
  }
  
  save_outcome <- c(save_outcome,game_outcome)
}

table(save_outcome)


