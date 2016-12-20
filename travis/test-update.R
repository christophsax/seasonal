
# --- update -------------------------------------------------------------------

# since this seems not contrevsial, we do this only for 5 models

# 1. evaluate test cases
# 2. run update on seas model
# 3. check if equal

test_update <- function(x){
  ccc <- parse(text = x)

  cc <- ccc[[length(ccc)]]

  set.seed(100)  # because we have runif() in the examples  (this should be removed)

  if (length(ccc) > 1){
    for (i in 1:(length(ccc)-1)){
       eval(ccc[[i]], envir = globalenv())
    }
  }
  
  a <- eval(cc)
  all.equal(final(a), final(update(a)))
}

ll <- lapply(r[5:10], function(e) try(test_update(e), silent = TRUE))

failing <- which(sapply(ll, class) == "try-error")

if (length(failing[!failing %in% c(47L, 52L, 53L, 60L, 98L)]) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}



