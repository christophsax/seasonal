ParseCurly <- function(txt){
  require(stringr)
  mat <- str_match_all(txt, '(?:([a-z]+))\\{(.*?)\\}')[[1]]
  z <- mat[,3]
  names(z) <- mat[,2]
  z <- str_replace_all(z, pattern = '\\s+', " ")
  str_trim(z)
}

ParseRound <- function(txt){
  require(stringr)
  mat <- str_match_all(txt, '(?:([a-z]+))\\s?=\\s?\\((.*?)\\s?\\)')[[1]]
  z <- str_trim(str_replace_all(mat[,3], pattern = '\\s+', " "))
  z <- str_split(z, pattern = " ")
  names(z) <- mat[,2]
  z
}
