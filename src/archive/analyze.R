# query 2
con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
vdss <- data.table::setDT(DBI::dbGetQuery(con, "select * from q2_dss_customers_by_year where calender_year_number = 2016"))
vdoe <- data.table::setDT(DBI::dbGetQuery(con, "select * from q2_doe_student_records"))

vdoe_unq <- unique(vdoe[school_year==2016, .(unique_id, associated_id, school_year)])

vdss_vdoe_lj <- merge(vdss, vdoe_unq, by = "unique_id", all.x = T)

vdss_vdoe_lj[is.na(associated_id), .N]

# query 3
con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
vdss <- data.table::setDT(DBI::dbGetQuery(con, "select * from q3_dss_customers_by_year"))
vdss_snap <- data.table::setDT(DBI::dbGetQuery(con, "select * from q3_dss_snap_customers_by_year"))
vocs <- data.table::setDT(DBI::dbGetQuery(con, "select * from q3_ocs_services_by_year where program_year = 2016"))
vdoe <- data.table::setDT(DBI::dbGetQuery(con, "select * from q3_doe_unique_student_listings"))
vdoe_vpip <- data.table::setDT(DBI::dbGetQuery(con, "select * from q3_doe_vpi_plus"))

vocs_unq <- vocs[, .N, by = list(unique_id)][order(-N), .(unique_id, service_records = N)]
vdoe_unq <- unique(vdoe[school_year==2016, .(unique_id, associated_id, school_year)])

vdss_vocs_lj <- merge(vdss, vocs_unq, by = "unique_id", all.x = T)
vdss_vocs_vdoe_lj <- merge(vdss_vocs_lj, vdoe_unq, by = "unique_id", all.x = T)

vdss_vdoe_lj[!is.na(associated_id), .N]

# VDSS
unique_inds <- vdss[age_group_code < 6, unique(unique_id)]
gender_mutli <-
  vdss[age_group_code < 6 &
         !is.na(gender_code), length(unique(gender_code)), unique_id][V1 > 1, .N]
birth_year_mutli <-
  vdss[age_group_code < 6 &
         !is.na(year_of_birth), length(unique(year_of_birth)), unique_id][V1 > 1, .N]
birth_month_mutli <-
  vdss[age_group_code < 6 &
         !is.na(month_of_birth), length(unique(month_of_birth)), unique_id][V1 > 1, .N]
ethnicity_mutli <-
  vdss[age_group_code < 6 &
         !is.na(ethnicity_code) &
         ethnicity_code != 0, length(unique(ethnicity_code)), unique_id][V1 > 1, .N]
race_white_mutli <-
  vdss[age_group_code < 6 &
         !is.na(customer_race_is_white_indicator) &
         customer_race_is_white_indicator %in% c("Y", "N"), length(unique(customer_race_is_white_indicator)), unique_id][V1 > 1, .N]
race_black_mutli <-
  vdss[age_group_code < 6 &
         !is.na(customer_race_is_black_indicator) &
         customer_race_is_black_indicator %in% c("Y", "N"), length(unique(customer_race_is_black_indicator)), unique_id][V1 > 1, .N]


test <- vdss_snap$unique_id %in% vdss$unique_id
length(test[test == FALSE])
