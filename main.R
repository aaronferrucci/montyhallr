# an array of 3 items, two FALSE and one TRUE
doors <- function() {
  return(sample(c(TRUE, FALSE, FALSE), 3))
}

choose <- function() {
  sample(doors(), 1)
}