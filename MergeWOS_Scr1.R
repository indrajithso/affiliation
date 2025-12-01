############################################################
###  MERGE MULTIPLE WEB OF SCIENCE FILES (XLS / XLSX)    ###
###        CLEAN, STANDARDIZE & REMOVE DUPLICATES         ###
############################################################

library(readxl)
library(dplyr)
library(purrr)
library(stringr)

###----------------------------------------------------------
### 1. Set folder containing WOS files
###----------------------------------------------------------
path <- "F:/Research/Affiliatation/30102025/data/wos"   # <-- change folder path

# List all Excel files in folder
files <- list.files(path, pattern = "\\.xls$|\\.xlsx$", full.names = TRUE)
cat("Number of WOS files found:", length(files), "\n")

###----------------------------------------------------------
### 2. Load and merge ALL files (row-wise)
###----------------------------------------------------------
wos_raw <- files %>% map_df(~ read_excel(.x))
cat("Merged total rows:", nrow(wos_raw), "\n")

###----------------------------------------------------------
### 3. Define STANDARDIZED field mapping
###----------------------------------------------------------
wos_mapping <- c(
  "Article Title"              = "Title",
  "Authors"                    = "Authors",
  "Author Full Names"          = "Author.Full.Names",
  "Publication Year"           = "Year",
  "Publication Date"           = "Pub.Date",
  "Document Type"              = "Document.Type",
  "Source Title"               = "Journal",
  "Abstract"                   = "Abstract",
  "Author Keywords"            = "Keywords",
  "Keywords Plus"              = "Keywords.Plus",
  "Affiliations"               = "Affiliations",
  "Addresses"                  = "Addresses",
  "Email Addresses"            = "Email",
  "Times Cited, WoS Core"      = "Times.Cited",
  "Times Cited, All Databases" = "Times.Cited.All",
  "DOI"                        = "DOI",
  "DOI Link"                   = "DOI.Link",
  "Pubmed Id"                  = "PMID",
  "ISSN"                       = "ISSN",
  "eISSN"                      = "eISSN",
  "Volume"                     = "Volume",
  "Issue"                      = "Issue",
  "Start Page"                 = "Start.Page",
  "End Page"                   = "End.Page",
  "Publisher"                  = "Publisher",
  "Research Areas"             = "Research.Areas",
  "Web of Science Index"       = "WoS.Index",
  "WoS Categories"             = "WoS.Categories",
  "UT (Unique WOS ID)"         = "WOS.ID"
)

# Rename columns that exist
wos <- wos_raw %>% rename(any_of(wos_mapping))
cat("Columns after renaming:", length(colnames(wos)), "\n")

###----------------------------------------------------------
### 4. Ensure key columns exist and are character
###----------------------------------------------------------
ensure_char_col <- function(df, colname) {
  if (!colname %in% names(df)) {
    df[[colname]] <- NA_character_
  } else {
    df[[colname]] <- as.character(df[[colname]])
  }
  df
}

wos <- wos %>%
  ensure_char_col("DOI") %>%
  ensure_char_col("PMID") %>%
  ensure_char_col("Title") %>%
  ensure_char_col("Year")

###----------------------------------------------------------
### 5. Smart deduplication: DOI → PMID → Title+Year
###----------------------------------------------------------
wos_clean <- wos %>%
  mutate(
    # Create a priority-based dedupe ID
    dedupe_id = coalesce(DOI, PMID, paste0(str_trim(Title), "_", Year))
  ) %>%
  distinct(dedupe_id, .keep_all = TRUE) %>%
  select(-dedupe_id)

cat("Rows after smart deduplication:", nrow(wos_clean), "\n")

###----------------------------------------------------------
### 6. Save output
###----------------------------------------------------------
out <- "F:/Research/Affiliatation/30102025/data/WoS/Merged_WoS_2024_clean.csv"
write.csv(wos_clean, out, row.names = FALSE)
cat("Saved:", out, "\n")