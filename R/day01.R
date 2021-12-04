# Day 01: Sonar Sweep

f01a <- function(x) {
  numbers = as.numeric(x)
  return(sum(sign(diff(numbers))>0, na.rm = T))
}

f01b <- function(x) {
  numbers = as.numeric(x)
  window = numbers + dplyr::lag(numbers, 1) + dplyr::lag(numbers, 2)
  out = sum(sign(diff(window))>0, na.rm = T)
  return(out)
}

