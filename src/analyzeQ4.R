library(data.table)
# query 4
con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")

dss <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_dss_customers_by_year"))
# eliminate duplicates
dss <- unique(dss)
# remove those that have multiple entries and different data for same year
dss_grt1 <- dss[, .N, by = list(unique_id, calender_year_number)][N>1, .(unique_id)]
dss <- dss[!unique_id %in% dss_grt1]
# fix spelling of calendar
names(dss)[names(dss) == "calender_year_number"] <- "calendar_year_number"


dss_foster <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_dss_foster_care_customers_by_year"))
dss_foster <- unique(dss_foster)
dss_foster_grt1 <- dss_foster[, .N, by = list(unique_id, calendar_year_number)][N>1, .(unique_id)]
dss_foster <- dss_foster[!unique_id %in% dss_foster_grt1]

ddf <- merge(dss, dss_foster, by = c("unique_id", "calendar_year_number"), all.x = T)


vdss_snap <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_dss_snap_customers_by_year"))
vocs <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_ocs_services_by_year where program_year = 2016"))
vdoe <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_doe_unique_student_listings"))
vdoe_vpip <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_doe_vpi_plus"))

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
