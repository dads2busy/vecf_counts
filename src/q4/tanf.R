con <- sdalr::con_db(dbname = "vecf", user = "aschroed", host = "localhost", port = 5433, pass = "Iwnftp$2")
cust_by_yr <- data.table::setDT(DBI::dbGetQuery(con, "select * from q4_dss_tanf_customers_by_year"))
id_yr_grt1 <- cust_by_yr[, .N, by = list(unique_id, calendar_year_number)][N>1, .(unique_id)]

tst <- cust_by_yr[unique_id=="5XZVAQQ",]
tst <- cust_by_yr[1:500,]
tst <- cust_by_yr

# dcast(tst, unique_id + study_group_id + calendar_year_number ~ county_fips_code + zip_code, value.var = c("county_fips_code", "zip_code"))
# 
# dcast(tst, unique_id + study_group_id + calendar_year_number ~ county_fips_code, value.var = c("county_fips_code"))
# 
# dcast(tst, unique_id + study_group_id + calendar_year_number ~ paste0("county_fips_code_", tst[, seq_len(.N), by=c("unique_id", "study_group_id", "calendar_year_number")]$V1), value.var = c("county_fips_code"))


tst[, county_fips_code_no := paste("county_fips_code", seq_len(.N), sep="_"), by=c("unique_id", "study_group_id", "calendar_year_number")]
tst[, zip_code_no := paste("zip_code", seq_len(.N), sep="_"), by=c("unique_id", "study_group_id", "calendar_year_number")]
fips <- dcast(tst, unique_id + study_group_id + calendar_year_number ~ county_fips_code_no, value.var=c("county_fips_code"))
zips <- dcast(tst, unique_id + study_group_id + calendar_year_number ~ zip_code_no, value.var=c("zip_code"))
fnl <- merge(fips, zips, by=c("unique_id", "study_group_id", "calendar_year_number"))
fnl[!is.na(county_fips_code_6)]
