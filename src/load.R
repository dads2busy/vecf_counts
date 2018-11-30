q2_vdss <- data.table::fread("data/original/q2/DSS/DSS Customers By Year.csv")
q2_vdoe <- data.table::fread("data/original/q2/DOE/Student Records.csv")

colnames(q2_vdss) <- gsub("\\.", "_", tolower(make.names(colnames(q2_vdss), unique = T, allow_ = T)))
colnames(q2_vdoe) <- gsub("\\.", "_", tolower(make.names(colnames(q2_vdoe), unique = T, allow_ = T)))

con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
DBI::dbWriteTable(con, "q2_dss_customers_by_year", q2_vdss, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q2_doe_student_records", q2_vdoe, row.names = F, overwrite = T)
