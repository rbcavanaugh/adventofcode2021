
# Sources solution functions
files.sources = list.files(here::here("R"), pattern = "day", full.names = T)
sapply(files.sources, source)
rm(files.sources)

# Sources input files
data.sources = list.files(here::here("data"), pattern = "input", full.names = T)
for(i in 1:length(data.sources)){
  assign(paste0("x",i), readLines(data.sources[i]))
}

aoc_solutions <- list(
  day01a = f01a(x1),
  day01b = f01b(x1),
  day02a = f02a(x2),
  day02b = f02b(x2),
  day03a = f03a(x3),
  day03b = f03b(x3),
  day04a = f04a(x4),
  day04b = f04b(x4),
  day05a = NA,
  day05b = NA,
  day06a = NA,
  day06b = NA,
  day07a = NA,
  day07b = NA,
  day08a = NA,
  day08b = NA,
  day09a = NA,
  day09b = NA,
  day10a = NA,
  day10b = NA,
  day11a = NA,
  day11b = NA,
  day12a = NA,
  day12b = NA,
  day13a = NA,
  day13b = NA,
  day14a = NA,
  day14b = NA,
  day15a = NA,
  day15b = NA,
  day16a = NA,
  day16b = NA,
  day17a = NA,
  day17b = NA,
  day18a = NA,
  day18b = NA,
  day19a = NA,
  day19b = NA,
  day20a = NA,
  day20b = NA,
  day21a = NA,
  day21b = NA,
  day22a = NA,
  day22b = NA,
  day23a = NA,
  day23b = NA,
  day24a = NA,
  day24b = NA,
  day25a = NA,
  day25b = NA
)
