files.sources = list.files(here::here("R"), pattern = "day", full.names = T)
sapply(files.sources, source)
rm(files.sources)

x1 <- readLines(here::here("data", "input01.txt"))

p1 <- f01a(x1)
p2 <- f01b(x1)
