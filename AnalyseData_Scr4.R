#Loads the readr package (for reading CSV files) and ggplot2 (for graphs).
library(readr) #read and write CSV files
library(ggplot2) #creating plots and graphs
library(dplyr) #manipulate and summarize data 
library(tidyr) # each variable is in its own column and each observation 
library(stringr) #manipulate text strings.
library(purrr)
#Reads CSV file from that location and stores it in a variable called data
data <- read_csv("D:/Research/Affliliation/mergedFiltereddata.csv")
#Makes sure the data is stored as a data frame, a common R data format.
data <- data.frame(data)
#Keeps only the first 10 rows of your dataset (useful for quick testing).
data<-data[1:100,]
dim(data)
names(data)
# write.csv(data, "D:/Research/Affliliation/mergedFiltereddata_sample.csv", row.names = FALSE)
#Opens your data in a spreadsheet-like table view (useful in RStudio).
View(data)

#==================================================
# Get Top 10 authors

top_authors <- data %>%
  count(Authors, sort = TRUE) %>%   # Count how many times each author appears
  head(10)                          # Keep only the top 10

print(top_authors)


# --- Create a bar chart ---
ggplot(top_authors, aes(x = reorder(Authors, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip axes to make names easier to read
  labs(
    title = "Top 10 Authors by Number of Publications",
    x = "Author",
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 13)
#=======================================================

top_authors <- data %>%
  count(Authors, sort = TRUE) %>%
  slice_head(n = 10)   # get top 10

# --- 4. Get the most frequent affiliation for each author ---
author_affiliations <- data %>%
  group_by(Authors, Affiliations) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  group_by(Authors) %>%
  slice_head(n = 1) %>%   # keep the most common affiliation
  ungroup()

# --- 5. Merge both tables ---
top_authors_with_aff <- left_join(top_authors, author_affiliations, by = "Authors")

# --- 6. Clean column names ---
top_authors_with_aff <- top_authors_with_aff %>%
  select(Author = Authors, Frequency = n, `Affiliated Institute` = Affiliations)

# --- 7. Handle multiple affiliations separated by ";" ---
top_authors_with_aff <- top_authors_with_aff %>%
  mutate(`Affiliated Institute` = strsplit(`Affiliated Institute`, ";")) %>%
  unnest(cols = c(`Affiliated Institute`)) %>%
  mutate(`Affiliated Institute` = trimws(`Affiliated Institute`))

# --- 8. Print the table ---
print(top_authors_with_aff)

# --- 9. Optional: Save to CSV ---
write.csv(top_authors_with_aff, "Top10_Authors_Affiliations.csv", row.names = FALSE)

# --- 10. Optional: Plot a chart ---
ggplot(top_authors_with_aff, aes(x = reorder(Author, -Frequency), y = Frequency, fill = `Affiliated Institute`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Authors and Their Main Affiliated Institutes",
       x = "Author", y = "Number of Publications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#===================================================================
# Top 10 Affiliations 

data_clean <- data %>%
  mutate(Affiliations = strsplit(Affiliations, ";")) %>%
  unnest(cols = c(Affiliations)) %>%
  mutate(Affiliations = str_trim(Affiliations))

# --- 3. Count frequency of each institute ---
top_institutes <- data_clean %>%
  count(Affiliations, sort = TRUE) %>%
  slice_head(n = 10)

# --- 4. Print the table ---
print(top_institutes)

# --- 5. Optional: Save results to a CSV file ---
write.csv(top_institutes, "Top10_Affiliated_Institutes.csv", row.names = FALSE)

# --- 6. Optional: Visualize the top 10 institutes ---
ggplot(top_institutes, aes(x = reorder(Affiliations, n), y = n, fill = Affiliations)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Affiliated Institutes by Publication Frequency",
       x = "Affiliated Institute", y = "Number of Publications") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 9))

#=========================================================================
#get Author & Affiliantion only
author_aff_data <- author_aff_data %>%
  mutate(
    Authors = strsplit(Authors, ";"),
    Affiliations = strsplit(Affiliations, ";")
  ) %>%
  mutate(
    Authors = map(Authors, str_trim),
    Affiliations = map(Affiliations, str_trim)
  ) %>%
  mutate(
    combo = map2(Authors, Affiliations, ~ expand.grid(Authors = .x, Affiliations = .y, stringsAsFactors = FALSE))
  ) %>%
  select(combo) %>%
  unnest(combo)

# 4. Remove duplicates and blanks
author_aff_data <- author_aff_data %>%
  distinct() %>%
  filter(Authors != "", Affiliations != "")

# 5. View first few rows
head(author_aff_data)
View(author_aff_data)

#=========================================================
# only sri lankan




author_aff_lk <- data %>%
  filter(Affiliation.Country == "Sri Lanka") %>%
  select(Authors, Affiliations, Affiliation.Country)

# View or print the results
View(author_aff_lk)  # if in RStudio
# or
print(author_aff_lk, n = nrow(author_aff_lk))


#-------------------------------------------
# get only first author
names(data)
data_first <- data %>%
  mutate(
    First_Author = str_split(Authors, ";") %>% sapply(function(x) trimws(x[1])),
    First_Affiliation = str_split(Affiliations, ";") %>% sapply(function(x) trimws(x[1]))
  )

# Step 2: Extract country names that contain 'Sri Lanka' (case-insensitive)
data_sri_lanka <- data_first %>%
  filter(str_detect(First_Affiliation, regex("Sri Lanka", ignore_case = TRUE)))

# Step 3: Create a clean table with first author and affiliation
author_aff_srilanka <- data_sri_lanka %>%
  select(First_Author, First_Affiliation)

# Step 4: View the full table
View(author_aff_srilanka)

#=============================================  
#Count how many publications per university (Sri Lanka only)

table_srilanka_unis <- author_aff_srilanka %>%
  count(First_Affiliation, sort = TRUE)

View(table_srilanka_unis)
  


#====================================================================
top10_unis <- table_srilanka_unis %>%
  slice_max(n, n = 10)

# Plot
ggplot(top10_unis, aes(x = reorder(First_Affiliation, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip for readability
  labs(
    title = "Top 10 Sri Lankan Affiliated Institutes (First Author)",
    x = "Affiliated Institute",
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 13)
#=======================================================

# Step 1: Select Authors and Author.Keywords
author_subject <- data %>%
  select(Authors, Author.Keywords) %>%
  filter(!is.na(Authors), !is.na(Author.Keywords))

# Step 2: Separate multiple authors and keywords into individual rows
author_subject_expanded <- author_subject %>%
  separate_rows(Authors, sep = ";") %>%
  separate_rows(Author.Keywords, sep = ";") %>%
  mutate(
    Authors = str_trim(Authors),
    Author.Keywords = str_trim(Author.Keywords)
  )

# Step 3: Count how many times each author appears per subject area
author_subject_count <- author_subject_expanded %>%
  group_by(Authors, Author.Keywords) %>%
  summarise(Frequency = n(), .groups = "drop")

# Step 4: Get top 10 subject areas overall
top_subjects <- author_subject_count %>%
  count(Author.Keywords, wt = Frequency, sort = TRUE) %>%
  slice_max(n, n = 10)

# Step 5: Plot top subject areas
ggplot(top_subjects, aes(x = reorder(Author.Keywords, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Subject Areas (Based on Author Keywords)",
    x = "Subject Area (Keyword)",
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 13)

#======================================================
# Top 10 Authors with keywords

# Step 1: Select only needed columns and clean data
author_subject <- data %>%
  select(Authors, Author.Keywords) %>%
  filter(!is.na(Authors), !is.na(Author.Keywords))

# Step 2: Expand authors and keywords into separate rows
author_subject_expanded <- author_subject %>%
  separate_rows(Authors, sep = ";") %>%
  separate_rows(Author.Keywords, sep = ";") %>%
  mutate(
    Authors = str_trim(Authors),
    Author.Keywords = str_trim(Author.Keywords)
  )

# Step 3: Find the top 10 most frequent authors
top_authors <- author_subject_expanded %>%
  count(Authors, sort = TRUE) %>%
  slice_max(n, n = 10)

# Step 4: Get their top subject areas
top_author_subject_table <- author_subject_expanded %>%
  filter(Authors %in% top_authors$Authors) %>%
  count(Authors, Author.Keywords, sort = TRUE) %>%
  group_by(Authors) %>%
  slice_max(n, n = 3) %>%  # Top 3 subjects per author (you can change this)
  ungroup()

# Step 5: Print neatly
print(top_author_subject_table, n = 10)

#================================================================================
#Popular Journals for University Research Publications


# Step 1: Count publications by Publisher
top_journals <- data %>%
  filter(!is.na(Publisher)) %>%
  group_by(Publisher) %>%
  summarise(Publications = n()) %>%
  arrange(desc(Publications))

# Step 2: Select top 10
top10_journals <- top_journals %>%
  slice_max(Publications, n = 10)

# Step 3: Display table
print(top10_journals)

# Step 4: Plot
ggplot(top10_journals, aes(x = reorder(Publisher, Publications), y = Publications)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Publishers / Journals for University Research Publications",
    x = "Publisher / Journal Name",
    y = "Number of Publications"
  ) +
  theme_minimal(base_size = 13)

#===================================================================
#Last Author

# Step 1: Create a new column with the last author extracted
data_last_author <- data %>%
  mutate(
    Last_Author = sapply(strsplit(Authors, ";"), function(x) {
      x <- str_trim(x)       # remove leading/trailing spaces
      tail(x, 1)             # get the last author in the list
    })
  )

# Step 2: Check the result
data_last_author %>%
  select(Authors, Last_Author) %>%
  head(10)
#==============================================================

# Extract last author and their matching affiliation
data_last_author <- data %>%
  mutate(
    Last_Author = sapply(strsplit(Authors, ";"), function(x) {
      x <- str_trim(x)
      tail(x, 1)
    }),
    Last_Affiliation = sapply(strsplit(Affiliations, ";"), function(x) {
      x <- str_trim(x)
      tail(x, 1)
    })
  )

# Step 2: Create a table of last author + affiliation with counts
last_author_aff_table <- data_last_author %>%
  filter(!is.na(Last_Author), !is.na(Last_Affiliation), Last_Affiliation != "") %>%
  group_by(Last_Author, Last_Affiliation) %>%
  summarise(Publications = n(), .groups = "drop") %>%
  arrange(desc(Publications))

# Step 3: View top 10 authors by publication count
top10_last_authors <- last_author_aff_table %>%
  slice_max(Publications, n = 10)

# Step 4: Display the result
print(top10_last_authors, n = 10)

#======================================================


# Step 1: Extract last author and last affiliation
data_last_author <- data %>%
  mutate(
    Last_Author = sapply(strsplit(Authors, ";"), function(x) {
      x <- str_trim(x)
      tail(x, 1)
    }),
    Last_Affiliation = sapply(strsplit(Affiliations, ";"), function(x) {
      x <- str_trim(x)
      tail(x, 1)
    })
  )

# Step 2: Create summary table
last_author_aff_table <- data_last_author %>%
  filter(!is.na(Last_Author), !is.na(Last_Affiliation), Last_Affiliation != "") %>%
  group_by(Last_Author, Last_Affiliation) %>%
  summarise(Publications = n(), .groups = "drop") %>%
  arrange(desc(Publications))

# Step 3: View top 10 authors with affiliations
top10_last_authors <- last_author_aff_table %>%
  slice_max(Publications, n = 10)

# Step 4: Print result
print(top10_last_authors, n = 10)

#========================================================
#Shows the first 5 unique affiliations.
unique(data$Affiliations)[1:5]

#Runs garbage collection — it frees up unused memory in R.
gc()

#Shows the first 10 unique author names.
unique(data$Authors)[1:10]

#=====================================================

#Shows the first 6 rows 
head(data)
#Shows the column names
names(data)
#Shows the number of rows and columns in your dataset.
dim(data)
#Adds a new column called index and fills it with the number 1 —
#this is often used later for counting how many rows belong to each group
data$index <- 1
#Divides the “Year” column into ranges (for example: 0–1950, 1950–1980, etc.)
#and creates a new column called Year_cat to show which range each record belongs to.
data$Year_cat  <- cut(data$Year, c(0,1950,1980,1990,2000,2010,2020,2025))
#Counts how many records (rows) there are for each year.
tapply(data$index, data$Year , sum)
#Draws a simple line plot:

plot(tapply(data$Year, data$Year , unique), tapply(data$index, data$Year , sum))
plot(tapply(data$Year, data$Year , unique), log(tapply(data$index, data$Year , sum)))
#Counts how many records fall into each year category (the groups you made earlier).
tapply(data$index, data$Year_cat , sum)
barplot(tapply(data$index, data$Year_cat , sum))
#Lists all the unique keywords used by authors (removes duplicates).
unique(data$Author.Keywords)
#Stores the unique affiliations (institutions, universities, etc.) in a variable called temp.
temp <- unique(data$Affiliations)
#Shows the structure of the temp object — how it’s stored and what type of data it contains.
str(temp)

#===================================================
data %>%
  count(Authors, sort = TRUE) %>%   # count how many times each author appears
  head(1)                          # show top 10


data %>%
  count(Authors, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(Authors, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Authors", x = "Author", y = "Count") +
  theme_minimal()
top_authors <- data %>%
  count(Authors, sort = TRUE) %>%   # Count how many times each author appears
  head(10)                          # Take top 10

# Display top 10 authors with counts
print(top_authors)

yearly_counts <- data %>%
  count(Year)
ggplot(top_authors, aes(x = reorder(Authors, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip axes to make names easier to read
  labs(title = "Top 10 Authors by Publication Count", x = "Author", y = "Count") +
  theme_minimal()
labs(title = "Publications per Year", y = "Count", x = "Year")

# --- Count and plot publications by year category ---
year_cat_counts <- data %>%
  count(Year_cat)

barplot(
  year_cat_counts$n,
  names.arg = year_cat_counts$Year_cat,
  col = "skyblue",
  main = "Publications by Year Category",
  ylab = "Count"
)
