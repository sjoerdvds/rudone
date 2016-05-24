make_call <- function(fun, args){
  args <- substr(args, 5, nchar(args))
  paste0(fun, args)
}

trim_whitespace <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}
