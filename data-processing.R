
# packages
source("./install-packages.R")

processing_packages <- c("dplyr", "tidyr", "stringr")

package_load(cran_packages = processing_packages)



##### ESL Rates #####

esl_rates <- read.csv(file.path("data", "esl_rates.csv"))

esl_rates <- esl_rates %>% # rename column names for consistency across datasets
  rename(
    `country` = geo,
    `year` = TIME_PERIOD,
    `ESL` = OBS_VALUE, 
    `level` = sex
  ) %>% 
  select(c(country, year, ESL, level)) # only relevant variables 

# wide format with separate columns for gender and total
esl_wide <- esl_rates %>%
  pivot_wider(
    names_from = level,
    values_from = ESL,
    names_prefix = "ESL_"
  ) %>%
  rename(
    ESL_total = ESL_Total,
    ESL_female = ESL_Females,
    ESL_male = ESL_Males
  )


##### Completion Rates #####

data_comp <- read.csv(file.path("data", "comp_rates.csv"))

# subset of European countries
data_comp <- data_comp[data_comp$country %in% c("Portugal", "Spain", "Italy", "Greece", "France", 
                                                    "Albania", "Algeria", "Germany", "Croatia", "Estonia", 
                                                    "Lithuania", "Romania", "Hungary", "Austria", 
                                                    "Czechia", "Bulgaria", "North Macedonia", "Netherlands", 
                                                    "Poland", "Serbia", "Slovenia", "Slovakia"), ] 

data_comp <- data_comp[data_comp$category %in% c("Total", "Sex", "Location"), ] 

 # create variable names and reshape to wide format
comp_wide <- data_comp %>%
  mutate(
    comp_label = case_when(
      category == "Sex" ~ paste0("comp_upsec_", tolower(sex)),
      category == "Location" ~ paste0("comp_upsec_", tolower(location)),
      category == "Wealth" ~ paste0("comp_upsec_", tolower(gsub(" ", "_", wealth))),
      category == "Total" ~ "comp_upsec_total"
    )
  ) %>%
  select(country, year, comp_label, comp_upsec_v2_m) %>%
  filter(!is.na(comp_label), !is.na(comp_upsec_v2_m)) %>%
  pivot_wider(
    names_from = comp_label,
    values_from = comp_upsec_v2_m
  )


##### Unemployment Rates #####

unemp_data <- read.csv(file.path("data", "unemp_rate_data.csv"))

unemp_data <- unemp_data %>%
  rename(Series = `Series.Name`, 
         country = `Country.Name`) %>% 
  select(-c(Country.Code, Series.Code))

unemp_data[unemp_data == ".."] <- NA # replace '..' with NA (as it is denoted in the other datasets) 

# rename columns to keep only years 
colnames(unemp_data) <- gsub("^X(\\d{4})\\..*", "\\1", colnames(unemp_data))


unemp_long <- unemp_data %>% # to long format for processing
  pivot_longer(
    cols = -c(country, Series),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(value)) 

# clean duplicates and then back to wide format
unemp_wide <- unemp_long %>%
  filter(
    country != "", !is.na(country),
    Series != "", !is.na(Series),
    !is.na(value)
  ) %>%
  group_by(country, year, Series) %>%
  summarise(value = mean(as.numeric(value)), .groups = "drop") %>%
  pivot_wider(
    names_from = Series,
    values_from = value
  )

# rename variables
unemp_wide <- unemp_wide %>%
  select(
    country,
    year,
    gov_exp_per_stud = `Government expenditure per student, secondary (% of GDP per capita)`,
    gov_exp_edu = `Government expenditure on education, total (% of GDP)`,
    gdp_pc = `GDP per capita (current US$)`,
    unemp_int_total = `Unemployment with intermediate education (% of total labor force with intermediate education)`,
    unemp_int_female = `Unemployment with intermediate education, female (% of female labor force with intermediate education)`,
    unemp_int_male = `Unemployment with intermediate education, male (% of male labor force with intermediate education)`,
    unemp_total = `Unemployment, total (% of total labor force) (modeled ILO estimate)`
  )


##### Education Statistics #####

educ_data <- read.csv(file.path("data", "educ_stat_data.csv"))

educ_data <- educ_data %>%
  rename(country = `Country.Name`) %>% 
  select(-c(Country.Code, Series.Code))

educ_data[educ_data == ".."] <- NA # for consistent notation of missing values 

colnames(educ_data) <- gsub("^X(\\d{4})\\..*", "\\1", colnames(educ_data))

educ_data <- educ_data[1:312,1:22] # only relevant data range (excl. metadata)

educ_data_pct <- educ_data %>%
  filter(
    str_detect(Series, "\\(\\%\\)") &
      !str_detect(Series, regex("out-of-school", ignore_case = TRUE))
  )

# to long format
educ_long <- educ_data_pct %>%
  pivot_longer(
    cols = matches("^(19|20)\\d{2}$"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))

# get combined information from gender and indicator column
educ_long <- educ_long %>%
  mutate(
    gender = case_when(
      str_detect(Series, "female") ~ "female",
      str_detect(Series, "male") ~ "male",
      str_detect(Series, "both sexes") ~ "total",
      TRUE ~ NA_character_
    ),
    indicator = case_when(
      str_detect(Series, "Gross enrolment ratio.*upper secondary") ~ "ger_upsec",
      str_detect(Series, "Total net enrolment rate.*upper secondary") ~ "ner_upsec",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(indicator)) %>%
  mutate(
    colname = paste(indicator, gender, sep = "_")
  )

# back to wide format 
educ_wide <- educ_long %>%
  select(country, year, colname, value) %>%
  pivot_wider(
    names_from = colname, # separate columns for each indicator-gender combination
    values_from = value
  )


##### Combining and Saving #####

combined_data <- educ_wide %>%
  full_join(unemp_wide, by = c("country", "year")) %>%
  full_join(esl_wide, by = c("country", "year")) %>%
  full_join(comp_wide, by = c("country", "year"))

# remove countries where at least one variable is only NA across all years
cleaned_data <- combined_data %>%
  group_by(country) %>%
  filter(
    !any(
      sapply(across(-year), function(col) all(is.na(col)))
    )
  ) %>%
  ungroup()

write.csv(cleaned_data, file.path("data", "final_data.csv"), row.names = FALSE)


