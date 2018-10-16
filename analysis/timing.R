library(tictoc)

up <- 10000000

tic()

tic()
for (i in 1:up) {
  a <- i^2
}
toc()


tic()
for (j in 1:(3*up)) {
  b <- j^2
}
toc()

tic()
for (k in 1:up) {
  f <- k^2
}
toc()

toc()