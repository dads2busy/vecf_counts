is_blank <- function(x) {
  return(is.null(x) |
           length(x) == 0 |
           is.na(x) |
           x == "")
}