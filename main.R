library(tidyr)
library(ggplot2)

run_games <- function(n) {
  df <- data.frame(
    car = sample(1:3, n, replace=T),
    choice = sample(1:3, n, replace=T)
  )
  
  show <- integer(n)
  for (i in 1:n) {
    others <- 1:3
    others <- others[(others != df$car[i]) & (others != df$choice[i])]
    # argh, sample on a vector of length 1 samples from 1:x.
    if (length(others) == 1)
      show[i] <- others
    else
      show[i] <- sample(others, 1)
  }
  
  df$show <- show
  return(df)
}

do_score <- function(games) {
  n <- nrow(games)
  scores <- data.frame(stay = logical(n), switch = logical(n))
  for (i in 1:n) {
    scores$stay[i] <- games$choice[i] == games$car[i] # stick with existing choice
    all <- 1:3
    scores$switch[i] <- all[all != games$choice[i] & all != games$show[i]] == games$car[i]
  }

  return(scores)
}

show_convergence <- function(scores) {
  n <- nrow(scores)
  convergence <- data.frame(
    i = 1:n,
    stay = cumsum(scores$stay) / 1:n,
    switch = cumsum(scores$switch) / 1:n
  ) 

  return(convergence)
}

plot <- function(conv) {
  ggplot(data=conv, aes(x=i, y=p_win, color=strategy)) +
    ggtitle("Monty Hall Problem - stay or switch?") +
    scale_y_continuous("Win Probability", breaks=c(0, 0.333, 0.667, 1)) +
    geom_line()
}


go <- function() {
  games <- run_games(10000)
  score <- do_score(games)
  convergence <- show_convergence(score)
  conv2 <- gather(convergence, strategy, p_win, stay, switch)
  plot(conv2)
}


