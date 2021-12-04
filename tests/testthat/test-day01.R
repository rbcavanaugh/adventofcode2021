testthat::test_that("day 1a is correct", {
  example = c(
    "199",
    "200",
    "208",
    "199",
    "210",
    "200",
    "207",
    "240",
    "269",
    "260",
    "263"
  )

increasing <- function(numbers){
  numer = as.numeric(numbers)
  return(sum(sign(diff(numer))>0))
}

testthat::expect_equal(increasing(example), 7)
})

testthat::test_that("day 1b is correct", {
  numbers2 = c(
    "199",
    "200",
    "208",
    "210",
    "200",
    "207",
    "240",
    "269",
    "260",
    "263"
  )


  increasing_window <- function(x){
    numbers = as.numeric(x)
    window = numbers + dplyr::lag(numbers, 1) + dplyr::lag(numbers, 2)
    out = sum(sign(diff(window))>0, na.rm = T)
    return(out)
  }

  testthat::expect_equal(increasing_window(numbers2), 5)
})

