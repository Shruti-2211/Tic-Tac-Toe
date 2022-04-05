# 1. Draw tic-tac-toe board.
# 2. Add a playing loop and a board variable.
# 3. Alternate player ("X", "O") and require move on empty.
# 4. Check for win.
# 5. Add random computer player.

rm(list=ls())               # clear all defined objects

#Tic-Tac-Toe Board

par(pty="s")                    # square plot type
x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

#Empty Matrix (3*3) - match squares to axes - flip y axis to match matrix format
symbols(x, y, squares=rep(1, times=9), inches=FALSE, xlim=c(0,4), ylim=c(4,0))

#initialize board with E on console
board = matrix(rep("E", times=9), nrow=3, ncol=3)

# Return whether player has won

won = function(board, player) {
  for (row in 1:3) {
    if (sum(board[row, ] == player) == 3) { # win in row
      return(TRUE)
    }
  }  
  for (col in 1:3)  {
    if(sum(board[ ,col] == player) == 3) {# win in column
      return(TRUE) 
    }
  }
  #Left Diagonal win
  
  if(sum((board[1,1] == player), (board[2,2] == player), (board[3,3] == player)) == 3){
    return(TRUE)
  }
  #Right Diagonal Win
  
  if(sum((board[1,3] == player), (board[2,2] == player), (board[3,1] == player)) == 3){
    return(TRUE)
  }
  return(FALSE)
}

#Initialize Player
player = "X"
for (i in 1:9) { # loop through 9 turns
  if (player == "X") {
    repeat { # get user input on empty square
      index = identify(x, y, n=1, plot=FALSE)
      row = y[index]
      col = x[index]
      if (board[row, col] == "E") {
        break
      }
    }
  } else { # computer chooses randorandom empty square
    open.indices = which(c(board)=="E")
    index = sample(x=open.indices, size=1)
    row = y[index]
    col = x[index]
  }
  board[row, col] = player
  text(x=x[index], y=y[index], labels=player)
  print(board)
  if (won(board, player)) {
    text(x=2, y=.2, labels=paste(player, "won!"), col="red")
    break
  }
  player = ifelse(player == "X", "O", "X")
}

