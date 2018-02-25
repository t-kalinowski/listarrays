


#
# print_rowwise <- function(x, nrows = 4) {
#
#   dims <- dim(x)
#   ndims <- length(dims)
#   cat(ndims, "dimensional array of shape:", parens(pcc(dims)), "\n")
#
#   total_rows <- dims[[1]]
#   if(total_rows > nrows)
#     cat("Printing first", nrows, "rows (along the first dimension)\n")
#
#   coms <- commas(length(dims)-1L)
#
#   for(i in seq_len(min(c(nrows, total_rows)))) {
#
#     if(is.null(rownames(x)))
#       cat(p0("[", i, coms, "]\n"))
#     else
#       cat(p0("[", rownames(x)[i], coms, "]\n"))
#
#     print.default(extract_rows(x, i))
#     cat("\n")
#   }
#   invisible(x)
# }
