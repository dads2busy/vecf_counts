# Basic Demographic Longitudinal Inconsistencies

#' FUNCTIONS ----
#+ load_libraries, warning=FALSE, message=FALSE
library(data.table)
library(dataplumbr)
#library(tidycensus)
library(sf)
library(ggplot2)
library(here)
library(knitr)
library(dplyr)
#library(kableExtra)

#' DSS RECORDS ----
#' Load DSS records and standardize column names
#+ dss_customers_load_rename, cache=TRUE, message=FALSE
dss_customers_by_year <- fread(here("data/original/q4/DSS/DSS Customers By Year.csv"), colClasses = "character")
colnames(dss_customers_by_year) <- standard_col_names(colnames(dss_customers_by_year))

#' fix a misspelling for future joining
#+ fix_misspelling
colnames(dss_customers_by_year)[colnames(dss_customers_by_year) == "calender_year_number"] <- "calendar_year_number"

#' subset to study age groups
#+ subset_age_groups
dss_customers_by_year_und_6 <- dss_customers_by_year[age_group_code %in% c("05", "04", "03", "02", "01")]
cat_chart <- dss_customers_by_year_und_6 %>% inspectdf::inspect_cat() %>% inspectdf::show_plot(high_cardinality = 1)
cat_chart

#' find demographics that change when they shouldn't
#+ changing_dmgs
gender_mutli <-
  dss_customers_by_year_und_6[!is.na(gender_code), .(cnt = length(unique(gender_code)), dmg = "multi gender"), unique_id]

gender_mutli <-
  dss_customers_by_year_und_6 %>%
  group_by(unique_id) %>%
  filter(!is.na(gender_code)) %>%
  summarise(cnt = length(unique(year_of_birth)), dmg = "multi birth yr")

birth_year_mutli <-
  dss_customers_by_year_und_6[!is.na(year_of_birth), .(cnt = length(unique(year_of_birth)), dmg = "multi gender"), unique_id]

birth_month_mutli <-
  dss_customers_by_year_und_6[!is.na(month_of_birth), .(cnt = length(unique(month_of_birth)), dmg = "multi gender"), unique_id]

ethnicity_mutli <-
  dss_customers_by_year_und_6[!is.na(ethnicity_code) & ethnicity_code != 0, .(cnt = length(unique(ethnicity_code)), dmg = "multi gender"), unique_id]

race_white_mutli <-
  dss_customers_by_year_und_6[!is.na(customer_race_is_white_indicator) &
         customer_race_is_white_indicator %in% c("Y", "N"), .(cnt = length(unique(customer_race_is_white_indicator)), dmg = "multi gender"), unique_id]

race_black_mutli <-
  dss_customers_by_year_und_6[!is.na(customer_race_is_black_indicator) &
         customer_race_is_black_indicator %in% c("Y", "N"), .(cnt = length(unique(customer_race_is_black_indicator)), dmg = "multi gender"), unique_id]

dmg_multi <- data.table::rbindlist(list(gender_mutli, birth_year_mutli, birth_month_mutli, ethnicity_mutli, race_white_mutli, race_black_mutli))

dmg_multi_wide <- dcast(dmg_multi, unique_id ~ dmg, value.var = "cnt")

dmg_multi_wide_c <- dmg_multi_wide[, lapply(.SD, as.character), by=unique_id]

cat_multi <- dmg_multi_wide_c %>% inspectdf::inspect_cat() %>% inspectdf::show_plot(high_cardinality = 1)
cat_multi


#' chart inconsistencies
#+ inconsistencies
unique_n <- dss_customers_by_year_und_6[, .(unique_id), unique_id][, .N]

dmg_df <-
  data.frame(
    demographic = c(
      "gender",
      "birth_year",
      "birth_month",
      "ethnicity",
      "race_black",
      "race_white"
    ),
    multiples = c(
      100*(gender_mutli[cnt > 1, .N] / unique_n),
      100*(birth_year_mutli[cnt > 1, .N] / unique_n),
      100*(birth_month_mutli[cnt > 1, .N] / unique_n),
      100*(ethnicity_mutli[cnt > 1, .N] / unique_n),
      100*(race_black_mutli[cnt > 1, .N] / unique_n),
      100*(race_white_mutli[cnt > 1, .N] / unique_n)
    )
  )

ggplot(data=dmg_df, aes(x=demographic, y=multiples)) +
  geom_bar(stat = "identity") +
  ggtitle("Children with Inconsistent Demographic Entries")
  



