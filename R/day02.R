#' Day 02: Dive!

f02a <- function(x) {
    df <- tibble(
      input = x
    ) %>%
      separate(input,
               into = c("direction", "magnitude"),
               sep = " "
      ) %>%
      mutate(magnitude = as.numeric(magnitude),
             change_horizontal = ifelse(direction == "forward", magnitude, NA),
             change_depth = ifelse(direction == "up", magnitude*(-1),
                                   ifelse(direction == "down", magnitude,
                                          NA
                                   )
             )
      ) %>%
      summarize(horizontal_position = sum(change_horizontal, na.rm = T),
                vertical_position = sum(change_depth, na.rm = T))

    out = df[[1,1]]*df[[1,2]]

    return(out)
}


f02b <- function(x) {
  track_position = tibble(horizontal_position = 0,
                          depth = 0,
                          aim = 0,
                          input = NA,
                          final_position = NA)
  for(i in 1:length(x)){
    new_numbers = f02_helper(x[i], track_position[i,])
    track_position = bind_rows(track_position, new_numbers)
  }

  out = tail(track_position,1)$final_position

  return(out)
}


f02_helper <- function(input, tibble_of_positions) {

    aim = tibble_of_positions$aim
    depth = tibble_of_positions$depth
    horizontal_position = tibble_of_positions$horizontal_position

    # down X increases your aim by X units.
    # up X decreases your aim by X units.
    # forward X does two things:
    #   It increases your horizontal position by X units.
    #   It increases your depth by your aim multiplied by X.

    change_num = readr::parse_number(input)
    if(grepl("forward", input)){
      horizontal_position = horizontal_position + change_num
      depth = depth + aim*change_num
    } else if (grepl("up", input)){
      aim = aim - change_num
    } else if (grepl("down", input)){
      aim = aim + change_num
    } else {
      print("error: direction not found")
    }

    out = tibble(
      horizontal_position = horizontal_position,
      depth = depth,
      aim = aim,
      input = input,
      final_position = horizontal_position*depth
    )

    return(out)

}


