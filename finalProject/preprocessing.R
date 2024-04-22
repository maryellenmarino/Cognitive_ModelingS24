# Load Packages -----------------------------------------------------------

library(tidyverse) # data manipulation
library(reshape2) # data manipulation
library(janitor) # clean_names()
library(tidymodels) # split data
library(lubridate) # date manipulation (just in case)


# Clean Data --------------------------------------------------------------

# load data and code names
nhanes <- read.csv("nhanes.csv") %>% clean_names()
# data code book
codes <- read.csv("nhanes_codebook.csv") %>% clean_names() %>%
  select(-class)
# columns that are actually coded
coded_cols <- read.csv("nhanes_cols_w_codes.csv") %>% clean_names()
coded_cols[11] <- FALSE

# Compute if patient has diabetes or not. 
# Diagnosis is positive if  stubject says yes to the diabetes question or had 
#     blood glucose >=126
nhanes <- nhanes %>% 
  mutate(diagnosis = nhanes[,23] == 'DIQ010-1' | 
           nhanes[,3] >= 126)

# drop rows with no diagnosis
nhanes.d <- nhanes %>% filter(!is.na(diagnosis))

# uncode columns in characters
nhanes.d[,16] <- as.character(nhanes.d[,16])
nhanes.d[,29] <- as.character(nhanes.d[,29])

# Clean up the coded values. Ask Hannah what this does

# loop through each column in the main data frame
for (col in colnames(nhanes.d)) {
  # check if the column exists and is not NA
  if (col %in% colnames(coded_cols) && 
      !is.na(coded_cols[[col]]) && 
      length(coded_cols[[col]]) > 0) {
    # check if the column is coded
    if (coded_cols[[col]] == TRUE) {
      # trim spaces in values of the current column
      nhanes.d[[col]] <- trimws(nhanes.d[[col]])
      # loop through each unique code in the column
      for (curr.code in unique(nhanes.d[[col]])) {
        if (curr.code %in% codes$code) {
          # find the matching value for the code in the data frame
          name <- codes$value[codes$code == curr.code]
          # replcae the code with its calye in the data frame
          nhanes.d[[col]][nhanes.d[[col]] == curr.code] <- name
        } else {
          # replaced unmatched codes with NA
          nhanes.d[[col]][nhanes.d[[col]] == curr.code] <- NA
        }
      }
    }
  }
}

# convert character columns to factors for later
nhanes.d[,5] <- as.factor(nhanes.d[,5])
nhanes.d[,8] <- as.factor(nhanes.d[,8])
nhanes.d[,9] <- as.factor(nhanes.d[,9])
nhanes.d[,12] <- as.factor(nhanes.d[,12])
nhanes.d[,16] <- as.factor(nhanes.d[,16])
nhanes.d[,17] <- as.factor(nhanes.d[,17])
nhanes.d[,18] <- as.factor(nhanes.d[,18])
nhanes.d[,19] <- as.factor(nhanes.d[,19])
nhanes.d[,20] <- as.factor(nhanes.d[,20])
nhanes.d[,23] <- as.factor(nhanes.d[,23])
nhanes.d[,24] <- as.factor(nhanes.d[,24])
nhanes.d[,25] <- as.factor(nhanes.d[,25])
nhanes.d[,28] <- as.factor(nhanes.d[,28])
nhanes.d[,29] <- as.factor(nhanes.d[,29])

# remove columns with >50% NA values
nhanes2 <- nhanes.d[,colSums(is.na(nhanes.d))/nrow(nhanes.d) <= 0.5]

# omit NAs, remove nondescriptive columns, and remove columns with empty factor 
#     levels, remove columns that were used in determining diagnosis
nhanes3 <- na.omit(nhanes2) %>% 
  select(-study_id, -human_id, -blood_glucose_mg_per_dc, -subject_has_diabetes) %>%
  # removed due to only having one level
  select(-subject_health_insurance) %>%
  droplevels()

write.csv(nhanes3, "nhanes_cleaned.csv")
