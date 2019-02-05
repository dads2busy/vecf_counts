# Load the tidycensus package into your R session
library(tidycensus)

# Define your Census API key and set it with census_api_key()
api_key <- "a3ad0a45671574b1ee2650264216f403c96c0f20"
census_api_key(api_key, install = TRUE)

# Check your API key
Sys.getenv("CENSUS_API_KEY")

# Get an ACS dataset for Census tracts in Texas by setting the state
tx_income <- get_acs(geography = "tract",
                     variables = "B19013_001",
                     state = "TX")

# Inspect the dataset
head(tx_income)


v15 <- load_variables(2017, "acs5", cache = TRUE)

View(v15)

va_pop_co <- get_acs(geography = "county",
                     variables = "B01001_001",
                     state = "VA")
head(va_pop_co)
