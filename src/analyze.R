con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
vdss <- data.table::setDT(DBI::dbGetQuery(con, "select * from q2_dss_customers_by_year where calender_year_number = 2016"))
vdoe <- data.table::setDT(DBI::dbGetQuery(con, "select * from q2_doe_student_records"))

vdoe_unq <- unique(vdoe[school_year==2016, .(unique_id, associated_id, school_year)])

vdss_vdoe_lj <- merge(vdss, vdoe_unq, by = "unique_id", all.x = T)

vdss_vdoe_lj[is.na(associated_id), .N]

