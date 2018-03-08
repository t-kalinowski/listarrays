library(microbenchmark)
library(ggplot2)

d <- c(200000, 26, 26)
x1 <- array(1:prod(d), d)
x2 <- aperm(x, c(2, 1, 3))
x3 <- aperm(x, c(2, 3, 1))


r <- microbenchmark(
  x1[2,,],
  x2[,2,],
  x3[,,2]
)
r
autoplot(r)

idx <- slice.index(x, 2)

split(x, idx)

x[as.vector(idx == 1)]



xa <- x3
xm <- xa
dim(xm) <- c(prod(dim(xa)[-3]), dim(xa)[3])

identical(
  as.vector(xa[,,6L]),
  as.vector(xm[,6L])
)

identical(
  xa[,,6L],
  array(xm[,6L], dim = c(256, 2))
)

r <- microbenchmark(
  xa[,,6L],
  array(xm[,6L], dim = c(256, 2))
)
r
autoplot(r)


d <- c(368000   , 128    ,  2)
X <- array(1:prod(d), d)
Xn <- provideDimnames(X)
system.time(s <-  split_along_rows(X))
system.time(sn <-  split_along_rows(Xn))
# > dim(X)
# [1] 368000    128      2
# tl1 <- system.time(rl1 <- unlist(apply(X, 1, list), recursive = FALSE) %>% lapply(drop_dimnames))
# tl2 <- system.time(rl2 <-  split_along_rows(X))
# > identical(rl1, rl2)
# [1] TRUE
# > tl1
# user  system elapsed
# 3.027   0.297   3.301
# > tl2
# user  system elapsed
# 55.272   0.297  55.194

# it's because `[` is so slow for arrays
# Xa <- test$X
# Xm <- Xa
# dim(Xm) <- c(dim(X)[1], prod(dim(X)[-1]))
# tXm <- t(Xm)
# tXa <- Xa
# dim(tXa) <- rev(dim(Xa))
#
# library(microbenchmark)
# autoplot(microbenchmark(
#   Xa[6,,],
#   Xm[6,],
#   tXm[,6],
#   tXa[,,6]
# ))
# Unit: microseconds
# expr        min        lq    mean  median     uq    max neval cld
# Xa[6, , ]   3.317 3.6125 4.10827 3.8530 4.0535 16.036   100   c
# Xm[6, ]     1.460 1.5690 2.07374 1.8020 1.8850 16.223   100 a
# tXm[, 6]    1.490 1.5720 1.67813 1.6185 1.6955  2.747   100 a
# tXa[, , 6]  2.210 2.4320 2.62682 2.4965 2.6385  5.238   100  b
#
#
#apply() forces things into a matrix, then restores dim (maybe) after looping
#with f() on all elements