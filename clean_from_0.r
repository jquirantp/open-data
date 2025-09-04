# -------------------------------
# 0. Load libraries
# -------------------------------
library(data.table)
library(jsonlite)
library(dplyr)

# -------------------------------
# 1. Load and filter match files (No changes needed)
# -------------------------------
matches <- list.files(path = 'data/matches', full.names = TRUE, recursive = TRUE)
match_list <- lapply(matches, function(fn) as.data.table(read_json(fn, simplifyVector = TRUE)))
matches_dt <- rbindlist(match_list, fill = TRUE)
matches_dt <- matches_dt[!duplicated(match_id), ]
top_leagues <- c('1. Bundesliga','Premier League','Serie A','La Liga','Ligue 1')
matches_dt <- matches_dt[competition.competition_name %in% top_leagues]

# -------------------------------
# 2. Load and filter event files (No changes needed)
# -------------------------------
events <- list.files(path = 'data/events', full.names = TRUE, recursive = TRUE)
events <- data.frame(fn = events)
events$match_id <- gsub(".json", "", basename(events$fn), fixed = TRUE)
events <- events[events$match_id %in% matches_dt$match_id, ]

# -------------------------------
# 3. Define REFINED robust function to extract events
# -------------------------------
get_events <- function(fn) {
  # Skip empty files
  if (file.size(fn) < 2) return(NULL) # Check for more than just empty brackets []

  # ✅ IMPROVEMENT: Directly parse to a data.table (much faster)
  dt <- as.data.table(read_json(fn, simplifyVector = TRUE))
  if (nrow(dt) == 0) return(NULL)
  
  # Flatten nested fields
  if ("shot" %in% names(dt)) {
    dt <- cbind(dt, dt$shot); dt[, shot := NULL]
  }
  if ("pass" %in% names(dt)) {
    dt <- cbind(dt, dt$pass); dt[, pass := NULL]
  }
  if ("foul_committed" %in% names(dt)) {
    dt <- cbind(dt, dt$foul_committed); dt[, foul_committed := NULL]
  }
  if ("bad_behaviour" %in% names(dt)) {
    if ("card" %in% names(dt$bad_behaviour)) dt <- cbind(dt, dt$bad_behaviour$card)
    dt[, bad_behaviour := NULL]
  }
  
  # ✅ IMPROVEMENT: Safely unify card columns using fcoalesce
  # This creates the new 'card.name' column
  dt[, card.name := fcoalesce(foul_committed.card.name, bad_behaviour.card.name, name)]
  
  # Filter relevant events
  dt <- dt[
    (type.name == "Shot") |
    (pass.type.name == "Corner") |
    (type.name == "Own Goal For") |
    (!is.na(card.name) & card.name %in% c("Yellow Card", "Red Card")) |
    (type.name == "Foul Committed") |
    (type.name %in% c("Half End", "Half Start"))
  ]
  
  if (nrow(dt) == 0) return(NULL)
  
  # Keep only desired columns
  cols_to_keep <- c(
    "id","index","period","timestamp","minute","second","match_id",
    "type.id","type.name","possession_team.name","play_pattern.name",
    "team.id","team.name","pass.type.name","card.name",
    "outcome.name","type.name","penalty","statsbomb_xg"
  )
  
  # Add match_id and select columns
  dt[, match_id := as.integer(gsub(".json", "", basename(fn), fixed = TRUE))]
  dt <- dt[, intersect(cols_to_keep, names(dt)), with = FALSE]
  
  return(dt)
}
# -------------------------------
# 4. Process all events IN BATCHES to save memory
# -------------------------------
cat("Processing", nrow(events), "event files in batches...\n")

# Define the size of each batch
batch_size <- 50
# Split the file list into a list of batches
file_batches <- split(events$fn, ceiling(seq_along(events$fn) / batch_size))

# Loop through each batch of file paths
for (i in seq_along(file_batches)) {
  
  cat("Processing batch", i, "of", length(file_batches), "...\n")
  
  # Get the file paths for the current batch
  current_batch_files <- file_batches[[i]]
  
  # Use lapply on just this small batch
  batch_list <- lapply(current_batch_files, get_events)
  batch_list <- batch_list[!sapply(batch_list, is.null)]
  
  if (length(batch_list) > 0) {
    batch_dt <- rbindlist(batch_list, fill = TRUE)
    
    # Save the result of this batch to a CSV file
    # `append = TRUE` adds to the file instead of overwriting it
    # `col.names = !file.exists(...)` writes headers only for the first batch
    fwrite(batch_dt, 
           file = "all_events.csv", 
           append = file.exists("all_events.csv"), 
           col.names = !file.exists("all_events.csv"))
  }
  
  # Optional: Clean up memory before the next batch
  rm(batch_list, batch_dt)
  gc() 
}

cat("Batch processing complete.\n")


# -------------------------------
# 5. Load the final combined CSV
# -------------------------------
print("Loading the final combined dataset from CSV...")
all_events <- fread("all_events.csv")

print("CSV file 'all_events.csv' generated successfully!")
print("--- Columns ---")
print(colnames(all_events))
print("--- Event types ---")
print(table(all_events$type.name))