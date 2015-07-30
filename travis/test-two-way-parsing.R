
# --- two way parsing ----------------------------------------------------------

# 1. evaluate test cases
# 2. import spc files
# 3. evaluate parsed calls 
# 4. compare series

tmpdir <-  file.path(tempdir(), "x13test")
if (!file.exists(tmpdir)) dir.create(tmpdir)

test_parse <- function(x){
  ccc <- parse(text = x)

  cc <- ccc[[length(ccc)]]

  if (length(ccc) > 1){
    for (i in 1:(length(ccc)-1)){
       eval(ccc[[i]])
    }
  }
  
  cc$out <- TRUE
  cc$dir <- tmpdir
  a <- suppressMessages(eval(cc))
  
  bb <- import.spc(file.path(tmpdir, "iofile.spc"))
  b <- lapply(bb, eval)$call
  all.equal(final(a), final(b))
}

ll <- lapply(r, function(e) try(test_parse(e)))


failing <- which(sapply(ll, class) == "try-error")

if (length(failing) > 0){
  stop("failing cases: ", paste(failing, collapse = ", "))
}




