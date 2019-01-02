fixcols <- function(dt) {
  colnames(dt) <- gsub("\\.", "_", tolower(make.names(colnames(dt), unique = T, allow_ = T)))
  dt
}

# query 2
q2_vdss <- data.table::fread("data/original/q2/DSS/DSS Customers By Year.csv")
q2_vdoe <- data.table::fread("data/original/q2/DOE/Student Records.csv")

colnames(q2_vdss) <- gsub("\\.", "_", tolower(make.names(colnames(q2_vdss), unique = T, allow_ = T)))
colnames(q2_vdoe) <- gsub("\\.", "_", tolower(make.names(colnames(q2_vdoe), unique = T, allow_ = T)))

con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
DBI::dbWriteTable(con, "q2_dss_customers_by_year", q2_vdss, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q2_doe_student_records", q2_vdoe, row.names = F, overwrite = T)

# query 3
q3_dss_customers_by_year <- fixcols(data.table::fread("data/original/q3/DSS/DSS Customers By Year.csv"))
q3_dss_foster_care_customers_by_year <- fixcols(data.table::fread("data/original/q3/DSS/DSS FOSTER CARE CUSTOMER By Year.csv"))
q3_dss_snap_customers_by_year <- fixcols(data.table::fread("data/original/q3/DSS/DSS SNAP Customers by Year.csv"))
q3_dss_tanf_customers_by_year <- fixcols(data.table::fread("data/original/q3/DSS/DSS TANF Customer by Year.csv"))
q3_doe_unique_student_listings <- fixcols(data.table::fread("data/original/q3/DOE/Unique Students Listing.csv"))
q3_doe_vpi_plus <- fixcols(data.table::fread("data/original/q3/DOE/VPI+.csv"))
q3_ocs_services_by_year <- fixcols(data.table::fread("data/original/q3/OCS/OCS Services By Year.csv"))

con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
DBI::dbWriteTable(con, "q3_dss_customers_by_year", q3_dss_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_dss_foster_care_customers_by_year", q3_dss_foster_care_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_dss_snap_customers_by_year", q3_dss_snap_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_dss_tanf_customers_by_year", q3_dss_tanf_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_doe_unique_student_listings", q3_doe_unique_student_listings, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_doe_vpi_plus", q3_doe_vpi_plus, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q3_ocs_services_by_year", q3_ocs_services_by_year, row.names = F, overwrite = T)

# query 4
q4_dss_customers_by_year <- fixcols(data.table::fread("data/original/q4/DSS/DSS Customers By Year.csv"))
q4_dss_foster_care_customers_by_year <- fixcols(data.table::fread("data/original/q4/DSS/DSS FOSTER CARE CUSTOMER By Year.csv"))
q4_dss_snap_customers_by_year <- fixcols(data.table::fread("data/original/q4/DSS/DSS SNAP Customers by Year.csv"))
q4_dss_tanf_customers_by_year <- fixcols(data.table::fread("data/original/q4/DSS/DSS TANF Customer by Year.csv"))
q4_doe_unique_student_listings <- fixcols(data.table::fread("data/original/q4/DOE/Unique Students Listing.csv"))
q4_doe_vpi_plus <- fixcols(data.table::fread("data/original/q4/DOE/VPI+.csv"))
q4_ocs_services_by_year <- fixcols(data.table::fread("data/original/q4/OCS/OCS Services By Year.csv"))

con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
DBI::dbWriteTable(con, "q4_dss_customers_by_year", q4_dss_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_dss_foster_care_customers_by_year", q4_dss_foster_care_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_dss_snap_customers_by_year", q4_dss_snap_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_dss_tanf_customers_by_year", q4_dss_tanf_customers_by_year, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_doe_unique_student_listings", q4_doe_unique_student_listings, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_doe_vpi_plus", q4_doe_vpi_plus, row.names = F, overwrite = T)
DBI::dbWriteTable(con, "q4_ocs_services_by_year", q4_ocs_services_by_year, row.names = F, overwrite = T)