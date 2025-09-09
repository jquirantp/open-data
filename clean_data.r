library(data.table)
library(jsonlite)

# --- Parameter for Testing ---
# Set how many event files to process. Use Inf to process all of them.
#files_to_process <- Inf
files_to_process <- Inf
# ---------------------------

## 1. Load Match Data
match_files <- list.files(path = 'data/matches', full.names = TRUE, recursive = TRUE)
matches_dt <- rbindlist(lapply(match_files, function(fn) {
  as.data.table(read_json(fn, simplifyVector = TRUE))
}), fill = TRUE)
matches_dt <- unique(matches_dt, by = "match_id")

## 2. Prepare and Filter Event File List
# Get all event file paths into a data.table
event_files_dt <- data.table(
  fn = list.files(path = 'data/events', full.names = TRUE, recursive = TRUE)
)
# Extract match_id from filename
event_files_dt[, match_id := gsub(".json", "", basename(fn), fixed = TRUE)]

# Filter for matches in the top 5 leagues
top5_leagues <- c('1. Bundesliga', 'Premier League', 'Serie A', 'La Liga', 'Ligue 1')
matches_to_process <- matches_dt[competition.competition_name %in% top5_leagues]

# Keep only the event files for the matches we care about
event_files_to_process <- event_files_dt[match_id %in% matches_to_process$match_id]

# Limit the number of files based on the parameter
event_files_to_process <- head(event_files_to_process, files_to_process)


## 3. Define the Event Processing Function
get_events_original <- function(fn) {
  ev <- as.data.table(read_json(fn, simplifyVector = TRUE))
  
  # Filter logic remains the same
  ev2 <- ev[
    pass.type.name == 'Corner' |
      type.name %in% c('Shot', 'Foul Committed', 'Half End', 'Half Start', 'Own Goal For', 'Bad Behaviour')
  ]
  
  # Exit if no relevant events are found
  if (nrow(ev2) == 0) {
    return(NULL)
  }
  
  # Updated the list of columns to include the three new ones
  original_cols <- c(
    "id", "index", "period", "timestamp", "minute", "second", "type.id", "type.name",
    "possession_team.name", "play_pattern.name", "team.id", "team.name",
    "pass.type.name", "shot.outcome.name", "shot.statsbomb_xg",
    "bad_behaviour.card.name",
    "foul_committed.card.name", # <-- Added here
    "shot.type.name",           # <-- Added here
    "foul_committed.penalty"    # <-- Added here
  )
  
  # Safely select only the columns that exist in this file
  existing_cols <- intersect(original_cols, names(ev2))
  ev3 <- ev2[, ..existing_cols]
  
  ev3[, fn := fn] # Add filename for joining
  return(ev3)
}

## 4. Execute and Create the Final Table
# Apply the function to the limited list of files
event_data_list <- lapply(event_files_to_process$fn, get_events_original)

# Combine all the results
all_events_v3 <- rbindlist(event_data_list, fill = TRUE)

# Join the match_id back efficiently
all_events_v3[event_files_to_process, on = "fn", match_id := i.match_id]
# FIX: Convert the character match_id to a numeric type
all_events_v3[, match_id := as.numeric(match_id)]



#write.csv(all_events_v3, "all_events_v3.csv", row.names = FALSE)

#all_events <- fread("all_events.csv")




