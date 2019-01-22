# FUNCTIONS ----
library(data.table)
library(dataplumbr)

# CODE ----
# DSS Customers
dss_customers_by_year <- fread("data/original/q4/DSS/DSS Customers By Year.csv", colClasses = "character")
colnames(dss_customers_by_year) <- standard_col_names(colnames(dss_customers_by_year))
colnames(dss_customers_by_year)[colnames(dss_customers_by_year) == "calender_year_number"] <- "calendar_year_number"

# DSS Customers Subset
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

# OCS Services
ocs_services_by_year <- fread("data/original/q4/OCS/OCS Services By Year.csv", colClasses = "character")
colnames(ocs_services_by_year) <- standard_col_names(colnames(ocs_services_by_year))

ocs_customers_by_year <- ocs_services_by_year[, .(ocs_service_entries = .N), list(unique_id, program_year)]
colnames(ocs_customers_by_year)[colnames(ocs_customers_by_year) == "program_year"] <- "calendar_year_number"

# DSS Subset, OCS Combined
dss_oss_cust_by_year <- merge(dss_customers_by_year_sub, ocs_customers_by_year, by = c("unique_id", "calendar_year_number"), all.x = TRUE)

dss_oss_cust_by_year[!is.na(ocs_service_entries), ocs_indicator := "Y"]
dss_oss_cust_by_year[is.na(ocs_service_entries), ocs_indicator := "N"]

# VPI+
doe_vpi_plus <- fread("data/original/q4/DOE/VPI+.csv", colClasses = "character")
colnames(doe_vpi_plus) <- standard_col_names(colnames(doe_vpi_plus))
doe_vpi_plus_by_year <- doe_vpi_plus[, .(vpi_plus_service_entries = .N), list(unique_id, beginning_school_year)]
colnames(doe_vpi_plus_by_year)[colnames(doe_vpi_plus_by_year) == "beginning_school_year"] <- "calendar_year_number"

dss_oss_vpip_cust_by_year <- merge(dss_oss_cust_by_year, doe_vpi_plus_by_year, by = c("unique_id", "calendar_year_number"), all.x = TRUE)

dss_oss_vpip_cust_by_year[!is.na(vpi_plus_service_entries), vpi_plus_indicator := "Y"]
dss_oss_vpip_cust_by_year[is.na(vpi_plus_service_entries), vpi_plus_indicator := "N"]


# Create Service Variables
dss_oss_vpip_cust_by_year[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                             cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                             customer_race_is_asisan_indicator == "Y" |
                             customer_race_is_other_indicator == "Y") & ethnicity_code == 0, race_eth := "other hispanic or latino not reported"]

dss_oss_vpip_cust_by_year[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                            cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                            customer_race_is_asisan_indicator == "Y" |
                            customer_race_is_other_indicator == "Y") & ethnicity_code == 1, race_eth := "other hispanic or latino"]

dss_oss_vpip_cust_by_year[(cust_race_is_amer_indian_alaska_native_ind == "Y" |
                             cust_race_is_hawaiian_pacific_islander_ind == "Y" |
                             customer_race_is_asisan_indicator == "Y" |
                             customer_race_is_other_indicator == "Y") & ethnicity_code == 2, race_eth := "other not hispanic or latino"]



dss_oss_vpip_cust_by_year[customer_race_is_black_indicator == "Y" & ethnicity_code == 0, race_eth := "black hispanic or latino not reported"]
dss_oss_vpip_cust_by_year[customer_race_is_black_indicator == "Y" & ethnicity_code == 1, race_eth := "black hispanic or latino"]
dss_oss_vpip_cust_by_year[customer_race_is_black_indicator == "Y" & ethnicity_code == 2, race_eth := "black not hispanic or latino"]

dss_oss_vpip_cust_by_year[customer_race_is_white_indicator == "Y" & ethnicity_code == 0, race_eth := "white hispanic or latino not reported"]
dss_oss_vpip_cust_by_year[customer_race_is_white_indicator == "Y" & ethnicity_code == 1, race_eth := "white hispanic or latino"]
dss_oss_vpip_cust_by_year[customer_race_is_white_indicator == "Y" & ethnicity_code == 2, race_eth := "white not hispanic or latino"]

dss_oss_vpip_cust_by_year[is.na(race_eth) & ethnicity_code == 0, race_eth := "race not reported hispanic or latino not reported"]
dss_oss_vpip_cust_by_year[is.na(race_eth) & ethnicity_code == 1, race_eth := "race not reported hispanic or latino"]
dss_oss_vpip_cust_by_year[is.na(race_eth) & ethnicity_code == 2, race_eth := "race not reported not hispanic or latino"]


dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "snap only"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "tanf only"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "foster only"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "snap & tanf"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "snap & foster"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "tanf & foster"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "snap & tanf & foster"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "N" & vpi_plus_indicator == "N", services := "other"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "snap & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "tanf & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "foster & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "snap & tanf & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "N" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "snap & foster & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "tanf & foster & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "Y" & tanf_case_indicator == "Y" & foster_care_case_indicator == "Y" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "snap & tanf & foster & ocs"]
dss_oss_vpip_cust_by_year[snap_case_indicator == "N" & tanf_case_indicator == "N" & foster_care_case_indicator == "N" & ocs_indicator == "Y" & vpi_plus_indicator == "N", services := "other & ocs"]
MORE
#### FIGURE OUT ALL COMBINATIONS OF SERVICES -- FORGETTING A BUNCH ####


contingency_table <- ftable(dss_oss_vpip_cust_by_year$services, dss_oss_vpip_cust_by_year$race_eth, dss_oss_vpip_cust_by_year$calendar_year_number)




# # there are no multi-race entries in the dss dataset
# dss_customers_by_year_sub[, all_race := paste0(
#   cust_race_is_amer_indian_alaska_native_ind,
#   cust_race_is_hawaiian_pacific_islander_ind,
#   customer_race_is_asisan_indicator,
#   customer_race_is_other_indicator,
#   customer_race_is_black_indicator,
#   customer_race_is_white_indicator
# )]
# 
# sum(gregexpr("Y", dss_customers_by_year_sub$all_race, fixed=TRUE)[[1]] > 1)
# 
# dss_customers_by_year_sub[length(gregexpr("Y", all_race)[[1]]) > 0, .N]
