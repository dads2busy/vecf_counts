# FUNCTIONS ----
R.utils::sourceDirectory("src/functions")
source(file = "src/functions/fix_column_names.R")
source(file = "src/functions/is_blank.R")

# DATASETS
cust_by_yr <- q4_dss_customers_by_year
colnames(cust_by_yr)
# don't know that i need this  va_zip_county <- data.table::setDT(readxl::read_excel("data/original/ZIP_COUNTY_1st Quarter 2016.xlsx"))[substr(COUNTY, 1, 2) %in% c("51", "24", "11", "37", "54", "21"), .(zip = ZIP, fips = COUNTY)]

# COLUMN PROFILES ----

#. unique_id ----
pct_complete_unique_id <- (nrow(cust_by_yr) - cust_by_yr[is_blank(unique_id), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_unique_id <- nrow(unique(cust_by_yr[, .(unique_id)]))
#.. valid values ----
wrong_id_length <- cust_by_yr[nchar(unique_id) != 7, .N]
pct_valid_values_unique_id <- (nrow(cust_by_yr) - wrong_id_length) / nrow(cust_by_yr)

#. age columns ---- 
#.. completeness ----
pct_complete_age_class_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_class_code), .N]) / nrow(cust_by_yr)
pct_complete_age_group_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_group_code), .N]) / nrow(cust_by_yr)
pct_complete_age_type_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_type_code), .N]) / nrow(cust_by_yr)
pct_complete_month_of_birth <- (nrow(cust_by_yr) - cust_by_yr[is_blank(month_of_birth), .N]) / nrow(cust_by_yr)
pct_complete_year_of_birth <- (nrow(cust_by_yr) - cust_by_yr[is_blank(year_of_birth), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_age_class_code <- nrow(unique(cust_by_yr[, .(age_class_code)]))
unq_count_age_group_code <- nrow(unique(cust_by_yr[, .(age_group_code)]))
unq_count_age_type_code <- nrow(unique(cust_by_yr[, .(age_type_code)]))
unq_count_month_of_birth <- nrow(unique(cust_by_yr[, .(month_of_birth)]))
unq_count_year_of_birth <- nrow(unique(cust_by_yr[, .(year_of_birth)]))
#.. valid values ----
pct_valid_values_age_class_code <- (nrow(cust_by_yr) - cust_by_yr[age_class_code == "90", .N]) / nrow(cust_by_yr)
pct_valid_values_age_group_code <- (nrow(cust_by_yr) - cust_by_yr[age_group_code == "90", .N]) / nrow(cust_by_yr)
pct_valid_values_age_type_code <- (nrow(cust_by_yr) - cust_by_yr[age_type_code == "90", .N]) / nrow(cust_by_yr)
# pct_valid_values_month_of_birth <- (nrow(cust_by_yr) - cust_by_yr[age_class_code == "90", .N]) / nrow(cust_by_yr)
# pct_valid_values_year_of_birth <- (nrow(cust_by_yr) - cust_by_yr[age_class_code == "90", .N]) / nrow(cust_by_yr)


#. school type ----
pct_complete_unique_id <- (nrow(cust_by_yr) - cust_by_yr[is_blank(unique_id), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_unique_id <- nrow(unique(cust_by_yr[, .(unique_id)]))
#.. valid values ----
wrong_id_length <- cust_by_yr[nchar(unique_id) != 7, .N]
pct_valid_values_unique_id <- (nrow(cust_by_yr) - wrong_id_length) / nrow(cust_by_yr)

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

IDeval <- tibble::tibble("Column" = "ID", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                         "Value" = c(scales::percent(pct_complete_unique_id), unq_count_unique_id, scales::percent(pct_valid_values_unique_id)))
AGEeval <- 
  rbind(
    tibble::tibble("Column" = "Age Class", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(scales::percent(pct_complete_age_class_code), unq_count_age_class_code, scales::percent(pct_valid_values_age_class_code))),
    tibble::tibble("Column" = "Age Group", "Measure"=c("% Complete", "# Unique", "% Valid "),
               "Value" = c(scales::percent(pct_complete_age_group_code), unq_count_age_group_code, scales::percent(pct_valid_values_age_group_code))),
    tibble::tibble("Column" = "Age Type", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(scales::percent(pct_complete_age_group_code), unq_count_age_type_code, scales::percent(pct_valid_values_age_type_code))),
    tibble::tibble("Column" = "Month of Birth", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(scales::percent(pct_complete_age_group_code), unq_count_month_of_birth, scales::percent(1))),
    tibble::tibble("Column" = "Year of Birth", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(scales::percent(pct_complete_year_of_birth), unq_count_year_of_birth, scales::percent(1)))
    ) #rbind


ZIPeval <- tibble::tibble("Column" = "ZIP", "Measure" = c("% Complete", "# Unique", "% Valid", "% Same/Yr", "Zip-Fip Match"), 
                          "Value" = c(scales::percent(pct_complete_zip_code), unq_count_zip_code, scales::percent(pct_valid_values_zip_code), scales::percent(pct_cust_no_chng_zip_code), scales::percent(pct_fips_zip_agreement)))
Caleval <- tibble::tibble("Column" = "CALYR", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                          "Value" = c(scales::percent(pct_complete_calendar_year_number), unq_count_calendar_year_number, scales::percent(pct_valid_values_calendar_year_number)))
Overall <- tibble::tibble("Column" = "Dupes", "Measure" = "% Non-Dupes", 
                          "Value" = scales::percent(pct_unduplicated_entries))
SummaryEval <- rbind(IDeval, FIPSeval, ZIPeval, Caleval, Overall)
SummaryEval
