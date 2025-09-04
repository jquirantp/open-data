library(data.table)
library(jsonlite)
library(dplyr)
library(purrr)

# 1. Find all event file paths
events_all_paths = list.files(path = 'data/events', full.names = TRUE, recursive = TRUE)

# 2. Select only the first 30 files as a small sample
events_subset_paths <- head(events_all_paths, 30)
print(paste("Loading a subset of", length(events_subset_paths), "files..."))

# 3. Function to read a file without filtering
get_all_events = function(fn){
  ev = read_json(fn, simplifyVector = TRUE)
  ev = as.data.table(ev)
  return(ev)
}

# 4. Load all events and combine into one data.table
subset_data = rbindlist(lapply(events_subset_paths, get_all_events), fill = TRUE)
print("Subset loaded.")

# 5. Keep only the columns you want
cols_to_keep = c(
  "id","index","period","timestamp","minute","second",
  "type.id","type.name","possession_team.name","play_pattern.name",
  "team.id","team.name","pass.type.name","foul_committed.card.name",
  "shot.outcome.name","shot.type.name","foul_committed.penalty","shot.statsbomb_xg"
)

# Use intersect() to avoid errors if some columns are missing
subset_data = subset_data[, intersect(cols_to_keep, names(subset_data)), with = FALSE]

# 6. Explore unique values for a specific column
unique_values = lapply(subset_data, unique)
print("--- Unique Event Types Found in Subset ---")
print(unique_values$pass.type.name)

# 7. Example: filter "Foul Committed" events and remove fully empty columns
subset_data %>%
  filter(type.name == "Foul Committed") %>%
  select(where(~ !all(is.na(.) | map_lgl(., ~ length(.) == 0))))

# 8. Check final columns
colnames(subset_data)
