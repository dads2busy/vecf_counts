# Load results from VLDS query 4

# FUNCTIONS ----
R.utils::sourceDirectory("src/functions")

# CODE ----
q4_dss_customers_by_year <- fix_column_names(data.table::fread("data/original/q4/DSS/DSS Customers By Year.csv", colClasses = "character"))
# fix spelling of calendar
names(q4_dss_customers_by_year)[names(q4_dss_customers_by_year) == "calender_year_number"] <- "calendar_year_number"
q4_dss_foster_care_customers_by_year <- fix_column_names(data.table::fread("data/original/q4/DSS/DSS FOSTER CARE CUSTOMER By Year.csv", colClasses = "character"))
q4_dss_snap_customers_by_year <- fix_column_names(data.table::fread("data/original/q4/DSS/DSS SNAP Customers by Year.csv", colClasses = "character"))
q4_dss_tanf_customers_by_year <- fix_column_names(data.table::fread("data/original/q4/DSS/DSS TANF Customer by Year.csv", colClasses = "character"))
q4_doe_unique_student_listings <- fix_column_names(data.table::fread("data/original/q4/DOE/Unique Students Listing.csv", colClasses = "character"))
q4_doe_vpi_plus <- fix_column_names(data.table::fread("data/original/q4/DOE/VPI+.csv", colClasses = "character"))
q4_ocs_services_by_year <- fix_column_names(data.table::fread("data/original/q4/OCS/OCS Services By Year.csv", colClasses = "character"))

