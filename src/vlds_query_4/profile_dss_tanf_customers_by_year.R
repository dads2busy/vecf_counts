# FUNCTIONS ----
R.utils::sourceDirectory("src/functions")

# DATASETS
cust_by_yr <- q4_dss_tanf_customers_by_year
va_zip_county <- data.table::setDT(readxl::read_excel("data/original/ZIP_COUNTY_1st Quarter 2016.xlsx"))[substr(COUNTY, 1, 2) %in% c("51", "24", "11", "37", "54", "21"), .(zip = ZIP, fips = COUNTY)]

# COLUMN PROFILES ----

#. unique_id ----
#.. completeness ----
pct_complete_unique_id <- (nrow(cust_by_yr) - cust_by_yr[is_blank(county_fips_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_unique_id <- nrow(unique(cust_by_yr[, .(unique_id)]))
#.. valid values ----
wrong_id_length <- cust_by_yr[nchar(unique_id) != 7, .N]
pct_valid_values_unique_id <- (nrow(cust_by_yr) - wrong_id_length) / nrow(cust_by_yr)


#. county_fips_code ----
#.. completeness ----
pct_complete_county_fips_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(county_fips_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_county_fips_code <- nrow(unique(cust_by_yr[, .(county_fips_code)]))
#.. valid values ----
va_fips_codes <- va_zip_county[, .(county_code = substr(fips, 3, 5))]
out_of_state <- data.table::data.table(county_code = "900")
valid_values_county_fips_code <- data.table::rbindlist(list(va_fips_codes, out_of_state))
pct_valid_values_county_fips_code <- (nrow(cust_by_yr) - cust_by_yr[!county_fips_code %in% valid_values_county_fips_code$county_code, .N]) / nrow(cust_by_yr)
#.. consistency (longitudinal) ----
#   percentage of customers whose county fips code doesn't change in a single year
cust_chng_county_fips_code <- unique(cust_by_yr[, county_fips_code, by = list(unique_id, calendar_year_number)][, .N, by = list(unique_id, calendar_year_number)][N>1, .(unique_id)])
pct_cust_no_chng_county_fips_code <- (nrow(unique(cust_by_yr[, .(unique_id)])) - nrow(cust_chng_county_fips_code)) / nrow(unique(cust_by_yr[, .(unique_id)]))


#. zip_code ----
#.. completeness ----
pct_complete_zip_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(zip_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_zip_code <- nrow(unique(cust_by_yr[, .(zip_code)]))
#.. valid values ----
va_zip_codes <- va_zip_county[, .(zip_code = zip)]
valid_values_zip_code <- va_zip_codes
pct_valid_values_zip_code <- (nrow(cust_by_yr) - cust_by_yr[!zip_code %in% valid_values_zip_code$zip_code, .N]) / nrow(cust_by_yr)
#.. consistency (longitudinal) ----
#   percentage of customers whose county fips code doesn't change in a single year
cust_chng_zip_code <- unique(cust_by_yr[, zip_code, by = list(unique_id, calendar_year_number)][, .N, by = list(unique_id, calendar_year_number)][N>1, .(unique_id)])
pct_cust_no_chng_zip_code <- (nrow(unique(cust_by_yr[, .(unique_id)])) - nrow(cust_chng_zip_code)) / nrow(unique(cust_by_yr[, .(unique_id)]))
#.. consistency (record) ----
#   agreement between county fips and zip codes
vzc <- va_zip_county[, .(county_fips_code = substr(fips, 3, 5), zip_code = zip)]
cust_cz <- unique(cust_by_yr[, .(county_fips_code, zip_code)])
zf <- merge(cust_cz, vzc)
pct_fips_zip_agreement <- (nrow(cust_cz) - nrow(zf)) / nrow(cust_cz)


#. calendar_year_number ----
#.. completeness ----
pct_complete_calendar_year_number <- (nrow(cust_by_yr) - cust_by_yr[is_blank(calendar_year_number), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_calendar_year_number <- nrow(unique(cust_by_yr[, .(calendar_year_number)]))
#.. valid values ----
valid_values_calendar_year_number <- seq("2007", "2017")
pct_valid_values_calendar_year_number <- (nrow(cust_by_yr) - cust_by_yr[!calendar_year_number %in% valid_values_calendar_year_number, .N]) / nrow(cust_by_yr)


# DUPLICATION ----
# because people move, there will be significant duplication if just considering unique_id and calendar_year_number
# a location column also needs to be included (e.g. county_fips_code) when checking for duplication
duplicated_entries <-  cust_by_yr[, .N, by = .(unique_id, calendar_year_number, county_fips_code)][N>1, .(unique_id)]
pct_unduplicated_entries <- (nrow(cust_by_yr) - nrow(duplicated_entries)) / nrow(cust_by_yr)
