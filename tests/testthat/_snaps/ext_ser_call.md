# ext_ser_call()

    Code
      ext_ser_call(pp, "x")
    Output
      [1] "x <- import.ts(file.dat)"

# ext_ser_call() defaults to format "free"

    Code
      ext_ser_call(pp, "x")
    Output
      [1] "x <- import.ts(file.dat, format = \"free\", start = c(2014, 1), frequency = 12, name = \"xyz\")"

