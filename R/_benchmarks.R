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








library(microbenchmark)
library(ggplot2)
library(purrrays)


d <- c(1e6, 256, 4)
# d <- c(1e3, 1e3, 1e3)
# d <- rep_len(100, 4)

x <- array(1:prod(d), d)

# m <- x
# dim(m) <- c(d[1], prod(d[-1]))


r <- microbenchmark(
  x[6L,,,],
  extract_rows(x, 6L),
  x[,6L,,],
  x[,,6L,],
  x[,,,6L]
)

# "
# Unit: milliseconds
#                 expr       min        lq      mean    median        uq       max neval cld
#          x[6L, , , ] 218.96975 220.49474 225.35855 222.41473 225.75812 265.66405   100   c
#  extract_rows(x, 6L) 219.39489 220.50527 224.73063 222.12046 224.93120 272.19305   100   c
#          x[, 6L, , ]  43.60040  43.97835  46.03738  44.60172  46.01396  55.37137   100  b
#          x[, , 6L, ]  36.65887  36.87579  38.77977  37.31790  38.10893  87.00333   100 a
#          x[, , , 6L]  36.60612  36.83140  39.05195  37.40731  42.66195  48.34859   100 a
# "


r
autoplot(r)
# the case of extracting on the first dim is by far the slowest

r <- microbenchmark(
  # x[6L,,,],
  # extract_rows(x, 6L),
  # x[,6L,,],
  x[,,6L,],
  extract_dim(x, 3L, 6L)
); print(r); autoplot(r)


library(purrrays)



library(microbenchmark)
library(ggplot2)

library(purrrays)


d <- c(1e6, 256, 4)
# d <- c(1e3, 1e3, 1e3)
# d <- rep_len(100, 4)

x <- array(1:prod(d), d)

gc()
(t1 <- system.time(split_along_rows(x)))
gc()
(t2 <- system.time(unlist(apply(x, 1L, list), recursive = FALSE)))
### massive performance improvement, but still not faster than base::apply...
# "
# > (t1 <- system.time(slp <- split_along_rows(x)))
#    user  system elapsed
#  13.070   1.264  14.333
# > (t2 <- system.time(sla <- unlist(apply(x, 1L, list), recursive = FALSE)))
#    user  system elapsed
#  11.500   1.384  12.882
# "







(t3 <- system.time(split_along_dim(x, 3L)))
(t4 <- system.time(split_along_dim(x, 4L)))

(t1 <- system.time(sr <- split_along_dim(x, 1L)))


# "
# > (t1 <- system.time(sr <- split_along_dim(x, 1L)))
#    user  system elapsed
#  44.502   0.000  44.497
# > (t2 <- system.time(split_along_dim(x, 2L)))
#    user  system elapsed
#   9.105   0.976  10.080
# > (t3 <- system.time(split_along_dim(x, 3L)))
#    user  system elapsed
#   7.753   0.812   8.564
# > (t4 <- system.time(split_along_dim(x, 4L)))
#    user  system elapsed
#   7.657   0.892   8.548
# >
# > (ta <- system.time(sra <- unlist(apply(x, 1L, list), recursive = FALSE)))
#    user  system elapsed
#  24.286   2.108  26.391
# > identical(sra, sr)
# [1] TRUE
# "

# most of the time is spent in [
222 %>% set_units("milliseconds") %>% `*`(200) %>% set_units(seconds)
37 %>% set_units("milliseconds") %>% `*`(200) %>% set_units(seconds)


# xa <-

# "
# >   system.time(aperm(x, c(2:4, 1)))
#    user  system elapsed
#  19.372   0.960  20.330
# "
library(purrrays)
# identical(xa[,,,1], x[1,,,])


d <- c(1000000, 128, 2)
x <- array(seq_len(prod(d)), d)


(ta_base <- system.time(xa <- aperm(x, c(2:3, 1))))
(t1 <- system.time(sr <- split_along_dim(x, 1L)))
(ta <- system.time(sra <- unlist(apply(x, 1L, list), recursive = FALSE)))
(tas <- system.time(sa <- split_along_dim(aperm(x, c(2:3, 1)), 3)))


# "
# > (ta_base <- system.time(xa <- aperm(x, c(2:3, 1))))
#    user  system elapsed
#   1.085   0.181   1.265
# > (t1 <- system.time(sr <- split_along_dim(x, 1L)))
#    user  system elapsed
# 150.911   0.108 150.999
# > (ta <- system.time(sra <- unlist(apply(x, 1L, list), recursive = FALSE)))
#    user  system elapsed
#   6.134   0.384   6.518
# > (tas <- system.time(sa <- split_along_dim(aperm(x, c(2:3, 1)), 3)))
#    user  system elapsed
# 141.127   0.512 141.622
# > identical(sra, sr)
# [1] TRUE
# > identical(sr, sa)
# [1] TRUE
# "

# aperm() on it's own is not the solution, need to walk through apply some more
# apply uses aperm to make it a 2dim matrix, then it goes along the cols and
# extracts a vector, and uses that to reconstruct an array of the appropriate
# dim
