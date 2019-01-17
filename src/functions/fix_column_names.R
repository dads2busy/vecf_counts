fix_column_names <- function(dt) {
  names <- colnames(dt)
  names <- tolower(names)
  names <- gsub("'", "", names)
  names <- gsub("[[:punct:] ]", "_", names)
  names <- gsub("_+", "_", names)
  names <- make.unique(names, sep = "_")
  colnames(dt) <- names
  dt
}
