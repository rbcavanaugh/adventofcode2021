library(tibble)
library(dplyr)
library(tidyr)
library(testthat)
test_that("day 3 works", {

  a = c(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )


  calcPowerConsumption <- function(x){

      chars = seq(0,nchar(x[1]),1)

      dat = tibble(
        x = x
      ) %>%
        separate(x, into = paste0("column_", chars), sep = "") %>%
        select(-column_0)

      getGammaBit <- function(m){
        ifelse(
          sum(m=="1")>sum(m=="0"), 1,0)
      }

      getEpsilonBit <- function(m){
        ifelse(
          sum(m=="1")>sum(m=="0"), 0,1)
      }

      gamma = dat %>%
        summarize(across(.cols = everything(), getGammaBit)) %>%
        unite(gamma, sep = "") %>%
        mutate(gamma = strtoi(gamma, base = 2)) %>%
        pull()

      epsilon = dat %>%
        summarize(across(.cols = everything(), getEpsilonBit)) %>%
        unite(gamma, sep = "") %>%
        mutate(gamma = strtoi(gamma, base = 2)) %>%
        pull()

      powerConsumption = gamma*epsilon

      return(powerConsumption)

  }

  testthat::expect_equal(calcPowerConsumption(a), 198)


})


library(tibble)
library(dplyr)
library(tidyr)
library(testthat)
test_that("day 3 works", {

  a = c(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  calc_generator_score <- function(a){
    chars = seq(0,nchar(a[1]),1)

    dat = tibble(
      a = a
    ) %>%
      separate(a, into = paste0("column_", chars), sep = "") %>%
      select(-column_0)

    dat_o2 = dat

    for (i in 1:nchar(a[1])){
      if (sum(dat_o2[[i]]=="1")>=sum(dat_o2[[i]]=="0")){
        dat_o2 = dat_o2[which(dat_o2[[paste0("column_", i)]]=="1"),]
      } else {
        dat_o2 = dat_o2[which(dat_o2[[paste0("column_", i)]]=="0"),]
      }
    }
   # print(dat_o2)
    o2 = dat_o2 %>%
      unite(o2, sep = "") %>%
      mutate(o2 = strtoi(o2, base = 2)) %>%
      pull()


    dat_co2 = dat
    for (i in 1:nchar(a[1])){
      #print("begin loop")
      if(nrow(dat_co2>1)){
        #print("more than one row")
          if (sum(dat_co2[[i]]=="1")>=sum(dat_co2[[i]]=="0")){
            dat_co2 = dat_co2[which(dat_co2[[paste0("column_", i)]]=="0"),]
            #print("keep 0s")
          } else {
            dat_co2 = dat_co2[which(dat_co2[[paste0("column_", i)]]=="1"),]
            #print("keep 1s")
          }
      } else {
        #print("No more filtering to do")
      }
      #print(dat_co2)
      if(nrow(dat_co2)==1){
        #print("Loop over")
        break
      }

    }
    #print(dat_co2)
    co2 = dat_co2 %>%
      unite(co2, sep = "") %>%
      mutate(co2 = strtoi(co2, base = 2)) %>%
      pull()


    generator_rating = co2*o2
    # cat("The o2 rating is", o2, "\n")
    # cat("The Co2 rating is", co2, "\n")
    return(generator_rating)
  }

  cat("The generator rating is", calc_generator_score(a), "\n")

  testthat::expect_equal(calc_generator_score(a), 230)

})
