library(data.table)
library(jsonlite)
library(dplyr)
library(data.table)
library(jsonlite)

matches = list.files(path = 'data/matches',full.names = T,recursive = T)

z = (lapply(matches,function(fn){
  
  x0 = read_json(fn,simplifyVector = T);
  x0 = as.data.table(x0)
  x0;
}))
z = rbindlist(z,fill = T)
z = z[!duplicated(z$match_id),]
table(z$competition.competition_name)
events = list.files(path = 'data/events',full.names = T,recursive = T)
events = data.frame(fn = events)
events$match_id = unlist(lapply(events$fn,function(ww){
  qq=strsplit(ww,split = '/',fixed=T)[[1]]
  rev(qq)[1]
}))
events$match_id = gsub(events$match_id,pattern='.json',replacement='',fixed=T)
m0 = z[z$competition.competition_name %in% c(
  '1. Bundesliga',
  'Premier League',
  'Serie A',
  'La Liga',
  'Ligue 1'
)]
m0$event_present = m0$match_id %in% events$match_id
events = events[events$match_id %in% m0$match_id,]
get_events=function(fn){
# we want xg , shots, corners, bookings
ev = read_json(fn,simplifyVector = T);
ev = as.data.table(ev)
ev2 = ev[(ev$pass.type.name == 'Corner') | (ev$type.name =='Shot') | (ev$type.name =='Own Goal For') | (ev$bad_behaviour.card.name %in% c("Yellow Card", "Red Card")) |(ev$type.name=='Foul Committed') | ev$type.name %in% c('Half End','Half Start'),]
ev3 = ev2[, .(id,index,period,timestamp,minute, second, type.id , type.name,possession_team.name,play_pattern.name,team.id,team.name,pass.type.name,foul_committed.card.name, shot.outcome.name,shot.type.name , foul_committed.penalty, shot.statsbomb_xg)]
ev3$fn = fn
return(ev3)
}


all_events = rbindlist(lapply(events$fn,get_events),fill=T)
all_events$match_id = events$match_id[match(all_events$fn,events$fn)]
fwrite(all_events, file = "all_events.csv")

# --- From now on, just load the saved CSV ---
#all_events <- fread("all_events.csv")








# Find all event file paths
events_all_paths = list.files(path = 'data/events', full.names = T, recursive = T)

# --- KEY CHANGE: Select only the first 3 files to create a small sample ---
events_subset_paths <- head(events_all_paths, 1)

print(paste("Loading a subset of", length(events_subset_paths), "files..."))


# Simple function to read a file without any filtering
get_all_events = function(fn){
  ev = read_json(fn, simplifyVector = T)
  ev = as.data.table(ev)
  return(ev)
}

# Run the function on our small subset of files
subset_data = rbindlist(lapply(events_subset_paths, get_all_events), fill = T)

print("Subset loaded. Now finding unique values for each column...")


# --- Explore the unique values ---

# Use lapply to run the 'unique' function on every column of our subset_data
unique_values = lapply(subset_data, unique)


# Print the list of unique values. It might be long!
# This will show you the column name, followed by all unique values found in it.
print(unique_values)

# To see the unique values for a specific column (like event type),
# you can access it by name:
print("--- Unique Event Types Found in Subset ---")
print(unique_values$type.name)


subset_data %>%
  filter(type.name == "Foul Committed") %>%
  select(where(~ !all(is.na(.) | purrr::map_lgl(., ~ length(.) == 0))))

colnames(subset_data)




