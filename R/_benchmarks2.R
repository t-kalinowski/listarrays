

library(purrrays)

# identical(xa[,,,1], x[1,,,])




coerce_to_matrix_and_construct_array_lapply <- function(x) {
  # x <- aperm(x, c(2, 3, 1))
  dx <- dim(x)
  dim(x) <- c(prod(dx[1:2]), dx[3])
  out_d <- dx[1:2]

  lapply(1:(dx[3]),
         function(i, out_d) array(x[,i], dim = out_d),
         out_d = out_d)
}

coerce_to_matrix_and_construct_array_forloop <- function(x) {
  # x <- aperm(x, c(2, 3, 1))
  dx <- dim(x)
  dim(x) <- c(prod(dx[1:2]), dx[3])
  out_d <- dx[1:2]

  out <- vector("list", dx[3])
  for(i in 1:dx[3])
    out[[i]] <-  array(x[,i], dim = out_d)

  out
}

coerce_to_matrix_and_setdim_forloop <- function(x) {
  # x <- aperm(x, c(2, 3, 1))
  dx <- dim(x)
  dim(x) <- c(prod(dx[1:2]), dx[3])
  out_d <- dx[1:2]

  out <- vector("list", dx[3])
  for(i in 1:dx[3]) {
    tmp <- x[,i]
    dim(tmp) <- out_d
    out[[i]] <- tmp
  }

  out
}

extract_array_lapply <- function(x) {
  lapply(1:(dim(x)[3]), function(i) x[,,i])
}

extract_array_forloop <- function(x) {
  dl <- dim(x)[3]
  out <- vector("list", dl)
  for (i in 1:dl)
    out[[i]] <- x[,,i]

  out
}



# identical(ret1, ret2)
# library(microbenchmark)

# microbenchmark::microbenchmarkcoerce_to_matrix_and_construct_array(x)))
# invisible(gc())
# (system.time(ret2 <- extract_array(x)))


library(purrrays)
library(microbenchmark)

d <- c(128, 2, 50000) # ret
d <- c(128, 2, 100000) # ret2
x <- array(seq_len(prod(d)), d)

# ret
"
Unit: milliseconds
                                            expr       min       lq     mean   median       uq      max neval   cld
                        extract_array_forloop(x)  94.24776 100.7578 126.6719 123.7389 142.8082 202.6116   100 a
                         extract_array_lapply(x) 116.34175 123.3835 153.2532 154.4360 175.1678 233.3923   100  b
   unlist(apply(x, 3L, list), recursive = FALSE) 189.91524 234.6574 245.0132 247.5631 262.0867 312.0611   100     e
 coerce_to_matrix_and_construct_array_forloop(x) 136.40165 183.5426 194.7696 202.4543 211.3444 255.2951   100   c
  coerce_to_matrix_and_construct_array_lapply(x) 152.63257 191.9913 207.2963 212.4071 228.9074 270.8777   100    d
"

# ret2
"
Unit: milliseconds
                                            expr      min       lq     mean   median       uq      max neval   cld
                        extract_array_forloop(x) 190.8096 213.5683 264.6107 261.7274 292.0500 440.9020   100 a
                         extract_array_lapply(x) 238.6594 257.8603 326.5870 305.9939 374.3090 546.1097   100  b
   unlist(apply(x, 3L, list), recursive = FALSE) 390.1535 498.3157 535.7301 555.6845 586.3143 720.2637   100     e
 coerce_to_matrix_and_construct_array_forloop(x) 280.5220 366.3524 425.6407 452.0445 479.7059 549.7276   100   c
  coerce_to_matrix_and_construct_array_lapply(x) 322.4289 419.3139 466.1534 490.3810 519.8526 568.8951   100    d
"

# the for loop is clearly faster than lapply
# extracting on the array is faster than extracting on a matrix and reconstructing the array


ret2 <- microbenchmark(
  extract_array_forloop(x),
  extract_array_lapply(x),
  unlist(apply(x, 3L, list), recursive = FALSE),
  coerce_to_matrix_and_construct_array_forloop(x),
  coerce_to_matrix_and_construct_array_lapply(x)
); print(ret2); autoplot.microbenchmark(ret2)

print(ret); autoplot.microbenchmark(ret)


# d <- c(100000, 128, 2)
# d <- c(128, 2, 5000000)
# x <- array(seq_len(prod(d)), d)

# invisible(gc())
# (system.time(extract_array_forloop(x)))
# invisible(gc())
# (system.time(extract_array_lapply(x)))
# invisible(gc())
# (system.time(unlist(apply(x, 3L, list), recursive = FALSE)))
# invisible(gc())
# (system.time(coerce_to_matrix_and_construct_array_forloop(x)))
# invisible(gc())
# (system.time(coerce_to_matrix_and_construct_array_lapply(x)))
# invisible(gc())
#


ret3 <- microbenchmark(
  extract_array_forloop(x),
  coerce_to_matrix_and_construct_array_forloop(x),
  coerce_to_matrix_and_setdim_forloop(x)
); print(ret3); autoplot.microbenchmark(ret3)
"
Unit: milliseconds
                                            expr      min       lq     mean   median       uq      max neval cld
                        extract_array_forloop(x) 194.1895 204.1778 242.1264 226.8430 257.5138 439.1329   100 a
 coerce_to_matrix_and_construct_array_forloop(x) 294.8810 437.7739 448.3762 454.1948 467.2349 511.5948   100   c
          coerce_to_matrix_and_setdim_forloop(x) 201.2936 337.2924 358.5759 378.9272 391.9592 445.3143   100  b
"