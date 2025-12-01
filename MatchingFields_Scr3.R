# -------------------------------------------------------------
# 1. Load Libraries
# -------------------------------------------------------------
library(dplyr)
library(stringr)
library(readr)

# -------------------------------------------------------------
# 2. Read Scopus & Web of Science datasets
# -------------------------------------------------------------
scopus <- read_csv("F:/Research/Affiliatation/30102025/data/scopus.csv")
wos    <- read_csv("F:/Research/Affiliatation/30102025/data/wos.csv")

# -------------------------------------------------------------
# 3. Define Standard Field Mapping
# -------------------------------------------------------------

# --- Scopus Mapping ---
scopus_map <- c(
  "eid"               = "Record_ID",
  "pubmed_id"         = "PMID",
  "doi"               = "DOI",
  "title"             = "Title",
  "authors"           = "Authors",
  "author_names"      = "Authors_Full",
  "year"              = "Year",
  "source_title"      = "Journal",
  "volume"            = "Volume",
  "issue"             = "Issue",
  "page_range"        = "Pages",
  "publisher"         = "Publisher",
  "abstract"          = "Abstract",
  "author_keywords"   = "Author_Keywords",
  "index_keywords"    = "Index_Keywords",
  "affiliations"      = "Affiliations",
  "funding_sponsor"   = "Funding_Agency",
  "grant_id"          = "Grant_Number",
  "cited_by"          = "Times_Cited",
  "references"        = "References"
)

# --- Web of Science Mapping ---
wos_map <- c(
  "UT"    = "Record_ID",
  "PMID"  = "PMID",
  "DI"    = "DOI",
  "TI"    = "Title",
  "AU"    = "Authors",
  "AF"    = "Authors_Full",
  "PY"    = "Year",
  "SO"    = "Journal",
  "VL"    = "Volume",
  "IS"    = "Issue",
  "BP"    = "Pages",
  "PU"    = "Publisher",
  "AB"    = "Abstract",
  "DE"    = "Author_Keywords",
  "ID"    = "Index_Keywords",
  "C1"    = "Affiliations",
  "FU"    = "Funding_Agency",
  "FG"    = "Grant_Number",
  "TC"    = "Times_Cited",
  "CR"    = "References"
)

# -------------------------------------------------------------
# 4. Rename Columns Automatically
# -------------------------------------------------------------
rename_safe <- function(df, mapping) {
  common <- intersect(names(df), names(mapping))
  df %>% rename_with(~ mapping[.x], all_of(common))
}

scopus_std <- rename_safe(scopus, scopus_map)
wos_std    <- rename_safe(wos, wos_map)

# -------------------------------------------------------------
# 5. Add missing columns SAFELY (no across()!)
# -------------------------------------------------------------
# Identify full list of fields
all_fields <- union(names(scopus_std), names(wos_std))

# Add missing columns to Scopus
missing_scopus <- setdiff(all_fields, names(scopus_std))
for (col in missing_scopus) {
  scopus_std[[col]] <- NA
}

# Add missing columns to WoS
missing_wos <- setdiff(all_fields, names(wos_std))
for (col in missing_wos) {
  wos_std[[col]] <- NA
}

# Ensure same column order
scopus_std <- scopus_std[, all_fields]
wos_std    <- wos_std[, all_fields]

# -------------------------------------------------------------
# 6. Save Standardized Versions
# -------------------------------------------------------------
write_csv(scopus_std, "F:/Research/Affiliatation/30102025/data/scopus_standardized.csv")
write_csv(wos_std,    "F:/Research/Affiliatation/30102025/data/wos_standardized.csv")
