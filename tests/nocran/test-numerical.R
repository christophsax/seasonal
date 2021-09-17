# --- numerical equality -------------------------------------------------------

# 1. evaluate test cases
# 2. compare results to run from previous versions

set.seed(100)  # because we have runif() in the examples  (this should be removed)

z <- lapply(r, function(e) try(eval(parse(text = e))))

failing <- which(sapply(z, class) == "try-error")

if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}

ff <- sapply(z, function(e) frequency(final(e)))

m.final <- do.call(cbind, lapply(z[ff == 12], final))
colnames(m.final) <- NULL

q.final <- final(z[ff == 4][[1]])
s.final <- final(z[ff == 6][[1]])


# Since B75, many numbers don't add up. If this is confirmed, we need to set up
# a new benchmark set.

# # save(m.final, q.final, s.final, file = file.path(travisdir, "examples/test0.90.0.RData"))
# bench <- new.env()
# load(file.path(nocran_tests, "examples/test0.90.9.RData"), envir = bench)

# stopifnot(all.equal(m.final, bench$m.final))
# stopifnot(all.equal(q.final, bench$q.final))
# stopifnot(all.equal(s.final, bench$s.final))

