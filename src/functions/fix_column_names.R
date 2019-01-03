fix_column_names <- function(dt) {
  colnames(dt) <- gsub("\\.", "_", tolower(make.names(colnames(dt), unique = T, allow_ = T)))
  dt
}