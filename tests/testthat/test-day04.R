library(tibble)
library(dplyr)
library(tidyr)
library(testthat)
test_that("day 4 works", {
  x = readLines(here::here("data", "example04.txt"))

  bingo_numbers = as.numeric(strsplit(x[1], split = ",")[[1]])
  #bingo_numbers = c(18, 8, 26, 23, 20, bingo_numbers)
  bingo_boards = x[-c(1,2)]
  num_boards = (length(bingo_boards)+1)/6

  split_boards = strsplit(bingo_boards, split = " ")
  for(i in 1:length(split_boards)){
    split_boards[[i]] = as.numeric(split_boards[[i]])
    split_boards[[i]] = split_boards[[i]][!is.na(split_boards[[i]])]
  }

  split_boards = Filter(length, split_boards)

  dat = as.data.frame(matrix(unlist(split_boards), ncol = 5, byrow = T)) %>%
    mutate(board = rep(seq(1,num_boards, 1), each = 5))

  dat_answer = dat

  for(i in 1:length(bingo_numbers)){

    dat[,1:5][dat[,1:5] == bingo_numbers[i]] <- 100

    # check rows
    row_check = dat %>%
      rowwise() %>%
      mutate(rows = sum(c(V1, V2, V3, V4, V5)))

    column_check = dat %>%
      group_by(board) %>%
      summarize(across(1:5, sum))

    if(any(row_check$rows==500)){
      cat("Bingo! (rows)", "\n")
      winning_board = row_check[row_check$rows==500,]$board
      final_number = bingo_numbers[i]
      cat("The winning board was", winning_board, "\n")
      break
    } else if (any(column_check==500)) {
      cat("Bingo! (columns)", "\n")
      winning_board = filter(column_check, if_any(V1:V5, ~.x==500))$board
      final_number = bingo_numbers[i]
      cat("The winning board was", winning_board, "\n")
      break
     }
  }



  dat[dat==100] <- NA
  dat_NA <- dat %>%
    filter(board == winning_board) %>%
    select(-board)
  cat("The sum of the non-selected items is", sum(dat_NA, na.rm = T), "\n")
  cat("The final bingo number is", final_number, "\n")
  answer = sum(dat_NA, na.rm = T)*final_number
  cat("The final answer is", answer, "\n")

  expect_equal(answer, 4512)

})


test_that("day 3 works", {
  x = readLines(here::here("data", "example04.txt"))

  bingo_numbers = as.numeric(strsplit(x[1], split = ",")[[1]])
  #bingo_numbers = c(3, 9, 19, 14, 20, bingo_numbers)
  bingo_boards = x[-c(1,2)]
  num_boards = (length(bingo_boards)+1)/6

  split_boards = strsplit(bingo_boards, split = " ")
  for(i in 1:length(split_boards)){
    split_boards[[i]] = as.numeric(split_boards[[i]])
    split_boards[[i]] = split_boards[[i]][!is.na(split_boards[[i]])]
  }

  split_boards = Filter(length, split_boards)

  dat = as.data.frame(matrix(unlist(split_boards), ncol = 5, byrow = T)) %>%
    mutate(board = rep(seq(1,num_boards, 1), each = 5))

  dat_answer = dat
  boards = unique(dat$board)

  for(i in 1:length(bingo_numbers)){

    dat[,1:5][dat[,1:5] == bingo_numbers[i]] <- 100

    # check rows
    row_check = dat %>%
      rowwise() %>%
      mutate(rows = sum(c(V1, V2, V3, V4, V5)))

    column_check = dat %>%
      group_by(board) %>%
      summarize(across(1:5, sum))

      if(any(row_check$rows==500) | any(column_check==500)){
        winning_board =
          unique(
            c(
              row_check[row_check$rows==500,]$board,
              filter(column_check, if_any(V1:V5, ~.x==500))$board
              )
            )
        #print(winning_board)
        remaining_boards = boards[-winning_board]
        if(length(remaining_boards)==1){
          cat("The final board is", remaining_boards, "\n")
          final_board = remaining_boards
        }
        if(length(remaining_boards)==0){
          final_number = bingo_numbers[i]
          break
        }
      }
  }

  dat[dat==100] <- NA
  dat_NA <- dat %>%
    filter(board == final_board) %>%
    select(-board)
  cat("The sum of the non-selected items is", sum(dat_NA, na.rm = T), "\n")
  cat("The final bingo number is", final_number, "\n")
  answer = sum(dat_NA, na.rm = T)*final_number
  cat("The final answer is", answer, "\n")

  expect_equal(answer, 1924)

})
