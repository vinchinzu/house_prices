

get_symbols<- function(){
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size=3, replace=TRUE,
  prob = c(0.03, 0.03, 0.06, 0.1, .25, .01, .52))
}

get_symbols()


symbols <- c("B", "C", "BB")
all(symbols %in% c("B", "BB", "BBB"))

symmbols <- c("7", "7", "7")

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B" = 10, "C" = 10 , "0" = 0)

payouts["DD"]
unname(payouts["DD"])

symbols <- c("B", "C", "BB")
symbols =="C"
sum(symbols=="C")




score <- function(symbols) {
# identify case
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

#get prize
if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 45, "BB" = 25, "B" = 10, "C" = 15 , "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5  
} else {
  #count cherries
  cherries <- sum(symbols =="C")
  prize <- c(0,2,5)[cherries + 1]
}
#adjust for diamonds
diamonds <- sum(symbols=="DD")
prize * 2 ^ diamonds

}



get_symbols<- function(){
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size=3, replace=TRUE,
         prob = c(0.08, 0.05, 0.11, 0.15, .20, .06, .35))
}

play <- function(){
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}




p <- as.data.frame(y)
ggplot(p, aes(x=y)) + geom_density()

p %>% count(y)


slot_display <- function(prize){
  symbols <- attr(prize, "symbols")
  symbols <- paste(symbols, collapse= " ")
  string <- paste(symbols, prize, sep = "\n$")
  cat(string)
}

slot_display(play())

test <- function(){
games <- 10000
y <- replicate(games, play())
sum(y)/games
}

test()

sets <- replicate(100, test())
summary(sets)
sets %>% qplot()


print(pi)

print(play())
play()




print.slots <- function(x, ...){
  slot_display(x)
}
one_play


play <- function(){
  symbols <- get_symbols()
  structure(score(symbols, symbols = symbols ))
}
play()

class(play())

play <- function(){
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize

}
play()

die <- 1:6

rolls <- expand.grid(die, die)

rolls$value <- rolls$Var1  + rolls$Var2


prob <- c("1" = 1/8, "2"= 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8 , "6" = 3/8)
prob[rolls$Var1]

 rolls$prob1 <- prob[rolls$Var1]
 rolls$prob2 <- prob[rolls$Var2]
 rolls$prob <- rolls$prob1 * rolls$prob2
 sum(rolls$value * rolls$prob)
 
 
 
 wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
 
combos <-  expand.grid(wheel, wheel, wheel, stringsAsFactors = F)


prob = c("DD" = 0.03, "7" =  0.03, "BBB"= 0.06, "BB" = 0.1, "B" = .25,  "C" = .01, "0" = .52)

combos$prob1 <- prob[combos$Var1] 
combos$prob2 <- prob[combos$Var2] 
combos$prob3 <- prob[combos$Var3] 

combos$prob  <- combos$prob1 * combos$prob2 * combos$prob3

combos %>% arrange(desc(prob))


sum(combos$prob)


for (value in c("My", "first", "for ", "loop")){
  print(value)
}
}


chars <- vector(length=4)

words <- c("My", "first", "for", "loop")

for (i in 1:4){
  chars[i] <- words[i]
}

combos$prize <- NA





for (i in 1:nrow(combos)) {
  symbols <- c(combos[i,1], combos[i,2], combos[i,3])
  combos$prize[i] <- score(symbols)
}

symbols
score(symbols)

head(combos)

sum(combos$prize * combos$prob)
combos


score <- function(symbols){
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols =="C")
  slots <- symbols[symbols !="DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
if (diamonds == 3){
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 45, "BB" = 25, "B" = 10, "C" = 15 , "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)){
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0,2,5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  prize * 2^diamonds
}  
  
combos$prize[is.na(combos$prize)] <- 2


plays_till_broke <- function(start_with){
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()[[1]]
    n <- n + 1
  }
  n
}


cash <- 100
start_with <- 100
plays_till_broke(100)


plays_till_broke <- function(start_with){
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()[[1]]
    n <- n + 1
    if (cash <=0) {
      break
    }
  }
  n
}


plays_till_broke(100)


cash <- cash - 1 + play()

cash
