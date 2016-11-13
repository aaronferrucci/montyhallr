# an array of 3 items, two FALSE and one TRUE
doors <- function() {
  return(sample(c(TRUE, FALSE, FALSE), 3))
}

choose <- function() {
  sample(doors(), 1)
}

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

