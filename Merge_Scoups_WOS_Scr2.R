library(dplyr)
library(readr)
library(stringr)

# Load standardized datasets
scopus_std <- read_csv("F:/Research/Affiliatation/30102025/data/scopus_standardized.csv")
wos_std    <- read_csv("F:/Research/Affiliatation/30102025/data/wos_standardized.csv")

# Fix type conflicts: convert all columns to character
scopus_std <- scopus_std %>% mutate(across(everything(), as.character))
wos_std    <- wos_std %>% mutate(across(everything(), as.character))

# Combine datasets
combined_df <- bind_rows(scopus_std, wos_std)

# Save merged output
write_csv(combined_df, "F:/Research/Affiliatation/30102025/data/combined_scopus_wos.csv")





#Remove duplicates



# Load combined dataset
combined_df <- read_csv("F:/Research/Affiliatation/30102025/data/combined_scopus_wos.csv")

# Clean Title: remove extra spaces and lowercase
combined_df <- combined_df %>%
  mutate(
    Title_clean = str_squish(str_to_lower(Title))
  )

# Create a key for duplicates: DOI > PMID > Title
combined_df <- combined_df %>%
  mutate(
    dup_key = coalesce(DOI, PMID, Title_clean)
  )

# Remove duplicates based on dup_key
combined_unique <- combined_df %>%
  filter(!is.na(dup_key)) %>%   # keep only records with at least one key
  distinct(dup_key, .keep_all = TRUE) %>%
  select(-dup_key, -Title_clean)  # remove helper columns

# Save cleaned dataset
write_csv(combined_unique, "F:/Research/Affiliatation/30102025/data/combined_scopus_wos_unique.csv")