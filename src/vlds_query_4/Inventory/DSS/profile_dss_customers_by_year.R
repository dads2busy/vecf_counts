# FUNCTIONS ----
R.utils::sourceDirectory("src/functions")
source(file = "src/functions/fix_column_names.R")
source(file = "src/functions/is_blank.R")

# DATASETS
cust_by_yr <- q4_dss_customers_by_year
colnames(cust_by_yr)

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
pct_valid_values_age_class_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_class_code)|age_class_code == "90", .N]) / nrow(cust_by_yr)
pct_valid_values_age_group_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_group_code)|age_group_code == "90", .N]) / nrow(cust_by_yr)
pct_valid_values_age_type_code <- (nrow(cust_by_yr) - cust_by_yr[is_blank(age_class_code)|age_type_code == "90", .N]) / nrow(cust_by_yr)
invalid_month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
pct_valid_values_month_of_birth <- (nrow(cust_by_yr) - cust_by_yr[is_blank(month_of_birth)|!month_of_birth %in% invalid_month, .N]) / nrow(cust_by_yr)
pct_valid_values_year_of_birth <- (nrow(cust_by_yr) - cust_by_yr[is_blank(year_of_birth)|!as.integer(year_of_birth) >= 1900, .N]) / nrow(cust_by_yr) 


#. school type ----
pct_complete_school_type <- (nrow(cust_by_yr) - cust_by_yr[is_blank(school_type_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_school_type <- nrow(unique(cust_by_yr[, .(school_type_code)]))
#.. valid values ----
wrong_sch_type <- cust_by_yr[is_blank(school_type_code) | school_type_code == "0", .N]
pct_valid_values_sch_type <- (nrow(cust_by_yr) - wrong_sch_type) / nrow(cust_by_yr)

#. ethnicity ----
pct_complete_ethnicity <- (nrow(cust_by_yr) - cust_by_yr[is_blank(ethnicity_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_ethnicity <- nrow(unique(cust_by_yr[, .(ethnicity_code)]))
#.. valid values ----
invalid_eth <- c("0", "3", "90", "93")
pct_valid_values_ethnicity <- (nrow(cust_by_yr) - cust_by_yr[ethnicity_code %in% invalid_eth, .N]) / nrow(cust_by_yr)

#. gender ----
pct_complete_gender <- (nrow(cust_by_yr) - cust_by_yr[is_blank(gender_code), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_gender <- nrow(unique(cust_by_yr[, .(gender_code)]))
#.. valid values ----
invalid_gend <- c("1", "2")
pct_valid_values_gender <- (nrow(cust_by_yr) - cust_by_yr[!gender_code %in% invalid_gend, .N]) / nrow(cust_by_yr)

#. race columns ---- 
#.. completeness ----
pct_complete_race_amer_ind <- (nrow(cust_by_yr) - cust_by_yr[is_blank(cust_race_is_amer_indian_alaska_native_ind), .N]) / nrow(cust_by_yr)
pct_complete_race_asian <- (nrow(cust_by_yr) - cust_by_yr[is_blank(customer_race_is_asisan_indicator), .N]) / nrow(cust_by_yr)
pct_complete_race_black <- (nrow(cust_by_yr) - cust_by_yr[is_blank(customer_race_is_black_indicator), .N]) / nrow(cust_by_yr)
pct_complete_race_pacific <- (nrow(cust_by_yr) - cust_by_yr[is_blank(cust_race_is_hawaiian_pacific_islander_ind), .N]) / nrow(cust_by_yr)
pct_complete_race_other <- (nrow(cust_by_yr) - cust_by_yr[is_blank(customer_race_is_other_indicator), .N]) / nrow(cust_by_yr)
pct_complete_race_white <- (nrow(cust_by_yr) - cust_by_yr[is_blank(customer_race_is_white_indicator), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_race_amer_ind <- nrow(unique(cust_by_yr[, .(cust_race_is_amer_indian_alaska_native_ind)]))
unq_count_race_asian <- nrow(unique(cust_by_yr[, .(customer_race_is_asisan_indicator)]))
unq_count_race_black <- nrow(unique(cust_by_yr[, .(customer_race_is_black_indicator)]))
unq_count_race_pacific <- nrow(unique(cust_by_yr[, .(cust_race_is_hawaiian_pacific_islander_ind)]))
unq_count_race_other <- nrow(unique(cust_by_yr[, .(customer_race_is_other_indicator)]))
unq_count_race_white <- nrow(unique(cust_by_yr[, .(customer_race_is_white_indicator)]))
#.. valid values ----
pct_valid_values_race_amer_ind <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(cust_race_is_amer_indian_alaska_native_ind)])) / nrow(cust_by_yr)
pct_valid_values_race_asian <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(customer_race_is_asisan_indicator)])) / nrow(cust_by_yr)
pct_valid_values_race_black <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(customer_race_is_black_indicator)])) / nrow(cust_by_yr)
pct_valid_values_race_pacific <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(cust_race_is_hawaiian_pacific_islander_ind)])) / nrow(cust_by_yr)
pct_valid_values_race_other <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(customer_race_is_other_indicator)])) / nrow(cust_by_yr)
pct_valid_values_race_white <- (nrow(cust_by_yr) - nrow(cust_by_yr[is_blank(customer_race_is_white_indicator)])) / nrow(cust_by_yr)


#. foster indicator ----
pct_complete_foster <- (nrow(cust_by_yr) - cust_by_yr[is_blank(foster_care_case_indicator), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_foster <- nrow(unique(cust_by_yr[, .(foster_care_case_indicator)]))
#.. valid values ----
invalid_ind <- c("N", "Y")
pct_valid_values_foster <- (nrow(cust_by_yr) - cust_by_yr[!foster_care_case_indicator %in% invalid_ind, .N]) / nrow(cust_by_yr)

#. SNAP indicator ----
pct_complete_snap <- (nrow(cust_by_yr) - cust_by_yr[is_blank(snap_case_indicator), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_snap <- nrow(unique(cust_by_yr[, .(snap_case_indicator)]))
#.. valid values ----
pct_valid_values_snap <- (nrow(cust_by_yr) - cust_by_yr[!snap_case_indicator %in% invalid_ind, .N]) / nrow(cust_by_yr)

#. TANF indicator ----
pct_complete_TANF <- (nrow(cust_by_yr) - cust_by_yr[is_blank(tanf_case_indicator), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_TANF <- nrow(unique(cust_by_yr[, .(tanf_case_indicator)]))
#.. valid values ----
pct_valid_values_TANF <- (nrow(cust_by_yr) - cust_by_yr[!tanf_case_indicator %in% invalid_ind, .N]) / nrow(cust_by_yr)

#. calendar year ----
pct_complete_cal <- (nrow(cust_by_yr) - cust_by_yr[is_blank(calender_year_number), .N]) / nrow(cust_by_yr)
#.. unique value count ----
unq_count_cal <- nrow(unique(cust_by_yr[, .(calender_year_number)]))
#.. valid values ----
pct_valid_values_cal <- (nrow(cust_by_yr) - cust_by_yr[!as.integer(calender_year_number) %in% 2013:2016, .N]) / nrow(cust_by_yr)


# DUPLICATION ----
# because people move, there will be significant duplication if just considering unique_id and calendar_year_number
# a location column also needs to be included (e.g. county_fips_code) when checking for duplication
duplicated_entries <-  cust_by_yr[, .N, by = .(unique_id, calendar_year_number, county_fips_code)][N>1, .(unique_id)]
pct_unduplicated_entries <- (nrow(cust_by_yr) - nrow(duplicated_entries)) / nrow(cust_by_yr)

IDeval <- tibble::tibble("Column" = "ID", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                         "Value" = c(pct_complete_unique_id, 
                                     unq_count_unique_id, pct_valid_values_unique_id))
AGEeval <- 
  rbind(
    tibble::tibble("Column" = "Age Class", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_age_class_code, 
                               unq_count_age_class_code, pct_valid_values_age_class_code)),
    tibble::tibble("Column" = "Age Group", "Measure"=c("% Complete", "# Unique", "% Valid "),
               "Value" = c(pct_complete_age_group_code, 
                           unq_count_age_group_code, pct_valid_values_age_group_code)),
    tibble::tibble("Column" = "Age Type", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_age_group_code, 
                               unq_count_age_type_code, pct_valid_values_age_type_code)),
    tibble::tibble("Column" = "Month of Birth", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_age_group_code, 
                               unq_count_month_of_birth, pct_valid_values_month_of_birth)),
    tibble::tibble("Column" = "Year of Birth", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_year_of_birth, 
                               unq_count_year_of_birth, pct_valid_values_year_of_birth))
    ) #rbind  

SCHeval <- tibble::tibble("Column" = "School Type", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                          "Value" = c(pct_complete_school_type, unq_count_school_type, pct_valid_values_sch_type))

ETHeval <- tibble::tibble("Column" = "Ethnicity", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                          "Value" = c(pct_complete_ethnicity, unq_count_ethnicity, pct_valid_values_ethnicity))

GENDeval <- tibble::tibble("Column" = "Gender", "Measure" = c("% Complete", "# Unique", "% Valid "), 
                          "Value" = c(pct_complete_gender, unq_count_gender, pct_valid_values_gender))
RACEeval <- 
  rbind(
    tibble::tibble("Column" = "Race: Amer Ind", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_amer_ind, unq_count_race_amer_ind, 
                               pct_valid_values_race_amer_ind)),
    tibble::tibble("Column" = "Race: Asian", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_asian, unq_count_race_asian, 
                               pct_valid_values_race_asian)),
    tibble::tibble("Column" = "Race: Black", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_black, unq_count_race_black, 
                               pct_valid_values_race_black)),
    tibble::tibble("Column" = "Race: Pacific", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_pacific, unq_count_race_pacific, 
                               pct_valid_values_race_pacific)),
    tibble::tibble("Column" = "Race: Other", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_other, unq_count_race_other, 
                               pct_valid_values_race_other)),
    tibble::tibble("Column" = "Race: White", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_race_white, unq_count_race_white, 
                               pct_valid_values_race_white))
  ) #rbind

SERVICEINDeval <-
  rbind(
    tibble::tibble("Column" = "Foster", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_foster, unq_count_foster, 
                               pct_valid_values_foster)),
    tibble::tibble("Column" = "SNAP", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_snap, unq_count_snap, 
                               pct_valid_values_snap)),
    tibble::tibble("Column" = "TANF", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_TANF, unq_count_TANF, 
                               pct_valid_values_TANF)),
    tibble::tibble("Column" = "Service Year", "Measure"=c("% Complete", "# Unique", "% Valid "),
                   "Value" = c(pct_complete_cal, unq_count_cal, 
                               pct_valid_values_cal))
  ) #rbind

SummaryEval <- rbind(IDeval, AGEeval, SCHeval, ETHeval, RACEeval, GENDeval, SERVICEINDeval) %>% reshape2::recast(Column~Measure)
SummaryEval 

