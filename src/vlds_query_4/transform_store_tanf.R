# Convert to single record per unique_id + study_group_id + calendar_year_number

# FUNCTIONS ----
R.utils::sourceDirectory("src/functions")

# CODE ----

#. Check for ids with multiple entries per year and location ----
id_yr_loc_grt1 <- cust_by_yr[, .N, by = list(unique_id, county_fips_code, zip_code, calendar_year_number)][N>1, .(unique_id)]

if (nrow(id_yr_loc_grt1) > 0) {
  cust_by_yr <- cust_by_yr[!unique_id %in% id_yr_loc_grt1]
}

cust_by_yr[, county_fips_code_no := paste("county_fips_code", seq_len(.N), sep="_"), by=c("unique_id", "study_group_id", "calendar_year_number")]
cust_by_yr[, zip_code_no := paste("zip_code", seq_len(.N), sep="_"), by=c("unique_id", "study_group_id", "calendar_year_number")]
fips <- dcast(cust_by_yr, unique_id + study_group_id + calendar_year_number ~ county_fips_code_no, value.var=c("county_fips_code"))
zips <- dcast(cust_by_yr, unique_id + study_group_id + calendar_year_number ~ zip_code_no, value.var=c("zip_code"))
fnl <- merge(fips, zips, by=c("unique_id", "study_group_id", "calendar_year_number"))
fnl[!is.na(county_fips_code_6)]


con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
DBI::dbWriteTable(con, "q4_dss_tanf_customers_by_year", fnl, row.names = F, overwrite = T)

tst_tbl <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_dss_tanf_customers_by_year"))
