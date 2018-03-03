#
#
# # named dim names, so that things like this work
# seq_along_dim(X, "channels")
# seq_along_dim(X, "timesteps")
# seq_along_dim(X, "samples")
#
#
# split_on_rows (f = )
# f can be a single scalar integerish, and the data is split equally into that many groups,
# or f can be a single number less than 0, in which case the data is split into 2 groups proportionally (e.g., f = 0.2, makes a 20-80% split). Or f can be a vector of numbers < 1, in which case it must sum up to 1 and the data will be split into groups that size (e.g., c(.2, .4. .4) makes a 20 40 40 % split)
#
#
# pad_expand_dim()?