#' FUNCTIONS ----
#+
library(data.table)
library(dataplumbr)

#' DSS RECORDS ----
#+ dss_customers_load_rename
dss_customers_by_year <- fread("data/original/q4/DSS/DSS Customers By Year.csv", colClasses = "character")
colnames(dss_customers_by_year) <- standard_col_names(colnames(dss_customers_by_year))

#' fix misspelling
#+
colnames(dss_customers_by_year)[colnames(dss_customers_by_year) == "calender_year_number"] <- "calendar_year_number"

#+ dss_customers_subset
dss_customers_by_year_sub <- 
  dss_customers_by_year[, .(unique_id,
                            calendar_year_number,
                            snap_case_indicator,
                            tanf_case_indicator,
                            foster_care_case_indicator,
                            cust_race_is_amer_indian_alaska_native_ind,
                            cust_race_is_hawaiian_pacific_islander_ind,
                            customer_race_is_asisan_indicator,
                            customer_race_is_black_indicator,
                            customer_race_is_other_indicator,
                            customer_race_is_white_indicator,
                            ethnicity_code)]

#+ dss_customers_subset_create_race_eth
dss_customers_by_year_sub[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                             cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                             customer_race_is_asisan_indicator == "Y" |
                             customer_race_is_other_indicator == "Y") & ethnicity_code == 0, race_eth := "other hispanic or latino not reported"]

dss_customers_by_year_sub[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                             cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                             customer_race_is_asisan_indicator == "Y" |
                             customer_race_is_other_indicator == "Y") & ethnicity_code == 1, race_eth := "other hispanic or latino"]

dss_customers_by_year_sub[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                             cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                             customer_race_is_asisan_indicator == "Y" |
                             customer_race_is_other_indicator == "Y") & ethnicity_code == 2, race_eth := "other not hispanic or latino"]

dss_customers_by_year_sub[customer_race_is_black_indicator == "Y" & ethnicity_code == 0, race_eth := "black hispanic or latino not reported"]
dss_customers_by_year_sub[customer_race_is_black_indicator == "Y" & ethnicity_code == 1, race_eth := "black hispanic or latino"]
dss_customers_by_year_sub[customer_race_is_black_indicator == "Y" & ethnicity_code == 2, race_eth := "black not hispanic or latino"]

dss_customers_by_year_sub[customer_race_is_white_indicator == "Y" & ethnicity_code == 0, race_eth := "white hispanic or latino not reported"]
dss_customers_by_year_sub[customer_race_is_white_indicator == "Y" & ethnicity_code == 1, race_eth := "white hispanic or latino"]
dss_customers_by_year_sub[customer_race_is_white_indicator == "Y" & ethnicity_code == 2, race_eth := "white not hispanic or latino"]

dss_customers_by_year_sub[is.na(race_eth) & ethnicity_code == 0, race_eth := "race not reported hispanic or latino not reported"]
dss_customers_by_year_sub[is.na(race_eth) & ethnicity_code == 1, race_eth := "race not reported hispanic or latino"]
dss_customers_by_year_sub[is.na(race_eth) & ethnicity_code == 2, race_eth := "race not reported not hispanic or latino"]


#' OCS RECORDS ----
#+ ocs_services_load_rename
ocs_services_by_year <- fread("data/original/q4/OCS/OCS Services By Year.csv", colClasses = "character")
colnames(ocs_services_by_year) <- standard_col_names(colnames(ocs_services_by_year))

#+ ocs_services_group_by_yr
ocs_customers_by_year <- ocs_services_by_year[, .(ocs_service_entries = .N), list(unique_id, program_year)]

#+ join_dss_ocs
colnames(ocs_customers_by_year)[colnames(ocs_customers_by_year) == "program_year"] <- "calendar_year_number"
dss_ocs_cust_by_year <- merge(dss_customers_by_year_sub, ocs_customers_by_year, by = c("unique_id", "calendar_year_number"), all.x = TRUE)

#+ create_ocs_service_indicator
dss_ocs_cust_by_year[!is.na(ocs_service_entries), ocs_indicator := "Y"]
dss_ocs_cust_by_year[is.na(ocs_service_entries), ocs_indicator := "N"]

#' VPI+ RECORDS ----
#+ vpip_load_rename
doe_vpip <- fread("data/original/q4/DOE/VPI+.csv", colClasses = "character")
colnames(doe_vpip) <- standard_col_names(colnames(doe_vpip))

#+ vpip_group_by_yr
doe_vpip_by_year <- doe_vpip[, .(vpip_service_entries = .N), list(unique_id, beginning_school_year)]

#+ join dss_ocs_vpip
colnames(doe_vpip_by_year)[colnames(doe_vpip_by_year) == "beginning_school_year"] <- "calendar_year_number"
dss_ocs_vpip_cust_by_year <- merge(dss_ocs_cust_by_year, doe_vpip_by_year, by = c("unique_id", "calendar_year_number"), all.x = TRUE)

#+ create_vpip_service_indicator
dss_ocs_vpip_cust_by_year[!is.na(vpip_service_entries), vpip_indicator := "Y"]
dss_ocs_vpip_cust_by_year[is.na(vpip_service_entries), vpip_indicator := "N"]


#' CREATE ALL COMBINATIONS OF SERVICE VARIABLE
#+ create_services_variable
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "N", services := "snap only"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "N", services := "tanf only"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "N", services := "foster only"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "N", services := "ocs only"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "Y", services := "vpip only"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "N", services := "snap & tanf"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "N", services := "snap & foster"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "N", services := "snap & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "Y", services := "snap & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "N", services := "tanf & foster"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "N", services := "tanf & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "Y", services := "tanf & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "N", services := "foster & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "Y", services := "foster & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "N", services := "snap & tanf & foster"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "N", services := "snap & tanf & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpip_indicator == "Y", services := "snap & tanf & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "N", services := "snap & foster & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "Y", services := "snap & foster & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "snap & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "N", services := "tanf & foster & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "Y", services := "tanf & foster & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "tanf & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "foster & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "N", services := "snap & tanf & foster & ocs"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpip_indicator == "Y", services := "snap & tanf & foster & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "snap & tanf & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "snap & foster & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "tanf & foster & ocs & vpip"]
dss_ocs_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpip_indicator == "Y", services := "snap & tanf & foster & ocs & vpip"]

#+ create_contingency_table
contingency_table <- ftable(dss_ocs_vpip_cust_by_year$services, dss_ocs_vpip_cust_by_year$race_eth, dss_ocs_vpip_cust_by_year$calendar_year_number)

print(contingency_table)


