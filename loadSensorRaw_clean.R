# Main data structure
library("data.table")
# Time manipulation
library("lubridate")
# Mapping vector values
library("plyr")

###
# Settings

inDir <- "raw_validation"

outDir <- "hen_timeline"

###
# Raw values mapping

zoneMapping <- c(
  # Pen 17
  # Tier 4
  "16400044" = "A1",  # Left, AKB
  "16400045" = "A2",  # Center, AKB
  "16400042" = "A3",  # Right, AKB
  "16400014" = "A4",  # Left, BG
  "16400015" = "A5",  # Center, BG
  "16400057" = "A6",  # Right, BG
  # Tier 3
  "16400018" = "A7",  # Left, AKB
  "16400019" = "A8",  # Center, AKB
  "16400020" = "A9",  # Right, AKB
  "16400016" = "A10", # Left, BG
  "16400017" = "A11", # Center, BG
  "16400078" = "A12", # Right, BG
  # Tier 2
  "16400013" = "A13", # Left, AKB
  "16400012" = "A14", # Center, AKB
  "16400011" = "A15", # Right, AKB
  "16400080" = "A16", # Left, BG
  "16400079" = "A17", # Center, BG
  "16400009" = "A18", # Right, BG   # Reconstructed from data
  # Tier 1
  "16400050" = "A19", # Left, AKB
  "16400049" = "A20", # Center, AKB
  "16400072" = "A21", # Right, AKB
  "16400010" = "A22", # Left, BG
  "16400008" = "A23", # Center, BG
  "16400007" = "A24", # Right, BG
  "16400075" = "A25", # Exit into garden 
  "16400077" = "A26", # Winter garden
  ### Pen 16
  # Tier 4
  "16400073" = "A1",  # Left, AKB
  "16400006" = "A2",  # Center, AKB
  "16400005" = "A3",  # Right, AKB
  "16400031" = "A4",  # Left, BG
  "16400004" = "A5",  # Center, BG
  "16400003" = "A6",  # Right, BG
  # Tier 3
  "16400001" = "A7",  # Left, AKB
  "16400161" = "A8",  # Center, AKB
  "16400074" = "A9",  # Right, AKB
  "16400032" = "A10", # Left, BG
  "16400034" = "A11", # Center, BG
  "16400033" = "A12", # Right, BG
  # Tier 2
  "16400071" = "A13", # Left, AKB
  "16400076" = "A14", # Center, AKB
  "16400002" = "A15", # Right, AKB
  "16400037" = "A16", # Left, BG
  "16400036" = "A17", # Center, BG
  "16400035" = "A18", # Right, BG
  # Tier 1
  "16400162" = "A19", # Left, AKB
  "16400163" = "A20", # Center, AKB
  "16400164" = "A21", # Right, AKB
  "16400038" = "A22", # Left, BG
  "16400039" = "A23", # Center, BG
  "16400040" = "A24", # Right, BG
  "16400048" = "A25", # Exit into garden 
  "16400043" = "A26" # Winter garden
)

penMapping <- c(
  # Pen 17
  # Tier 4
  "16400044" = "P17",  # Left, AKB
  "16400045" = "P17",  # Center, AKB
  "16400042" = "P17",  # Right, AKB
  "16400014" = "P17",  # Left, BG
  "16400015" = "P17",  # Center, BG
  "16400057" = "P17",  # Right, BG
  # Tier 3
  "16400018" = "P17",  # Left, AKB
  "16400019" = "P17",  # Center, AKB
  "16400020" = "P17",  # Right, AKB
  "16400016" = "P17", # Left, BG
  "16400017" = "P17", # Center, BG
  "16400078" = "P17", # Right, BG
  # Tier 2
  "16400013" = "P17", # Left, AKB
  "16400012" = "P17", # Center, AKB
  "16400011" = "P17", # Right, AKB
  "16400080" = "P17", # Left, BG
  "16400079" = "P17", # Center, BG
  "16400009" = "P17", # Right, BG   # Reconstructed from data
  # Tier 1
  "16400050" = "P17", # Left, AKB
  "16400049" = "P17", # Center, AKB
  "16400072" = "P17", # Right, AKB
  "16400010" = "P17", # Left, BG
  "16400008" = "P17", # Center, BG
  "16400007" = "P17", # Right, BG
  "16400075" = "P17", # Exit into garden 
  "16400077" = "P17", # Winter garden
  ### Pen 16
  # Tier 4
  "16400073" = "P16",  # Left, AKB
  "16400006" = "P16",  # Center, AKB
  "16400005" = "P16",  # Right, AKB
  "16400031" = "P16",  # Left, BG
  "16400004" = "P16",  # Center, BG
  "16400003" = "P16",  # Right, BG
  # Tier 3
  "16400001" = "P16",  # Left, AKB
  "16400161" = "P16",  # Center, AKB
  "16400074" = "P16",  # Right, AKB
  "16400032" = "P16", # Left, BG
  "16400034" = "P16", # Center, BG
  "16400033" = "P16", # Right, BG
  # Tier 2
  "16400071" = "P16", # Left, AKB
  "16400076" = "P16", # Center, AKB
  "16400002" = "P16", # Right, AKB
  "16400037" = "P16", # Left, BG
  "16400036" = "P16", # Center, BG
  "16400035" = "P16", # Right, BG
  # Tier 1
  "16400162" = "P16", # Left, AKB
  "16400163" = "P16", # Center, AKB
  "16400164" = "P16", # Right, AKB
  "16400038" = "P16", # Left, BG
  "16400039" = "P16", # Center, BG
  "16400040" = "P16", # Right, BG
  "16400048" = "P16", # Exit into garden 
  "16400043" = "P16" # Winter garden
)

henMapping <- c(
  "DA09C13C" = "GS",
  "DA09C1C9" = "PS",
  "DA09C10F" = "GG"
)

###
# Helper functions

# Convert a timeline into an list of single-hen timelines
# Parameter: a timeline
# Output: a list of single-hen timelines
splitHenData <- function(data) {
  if (nrow(data)) {
    hens <- unique(data[, Hen])
    splitData <- list()
    for (henID in hens) {
      splitData <- append(splitData, list(data[Hen == henID]))
    }
  } else { # Data is empty - return a list with one empty dataframe
    splitData <- list(data) 
  }
  return(splitData)
}

# Merge a list of timelines into a single timeline
# Parameter: a list of timelines
# Output: a single timeline
mergeHenData <- function(henDataList) {
  combined <- rbindlist(henDataList)
  setkeyv(combined, c("Time", "Hen"))
  setindex(combined, Hen)
  return(combined)
}

# Apply a function to a timeline hen-wise
# Parameters: a function to transform a single-hen timeline, a timeline
# Output: a single transformed timeline
applyPerHen <- function(f, data) {
  mergeHenData(Map(f, splitHenData(data)))
}

# Remove consecutive Zone duplicates but also keep the last record
# Does not change original data
# Parameter: single-hen timeline
# Output: a sparse timeline
compactHenData <- function(henData) {
  # See https://www.rdocumentation.org/packages/data.table/versions/1.14.0/topics/rleid
  #   TRUE for first element in groups of repeated elements, FALSE otherwise
  # henData[, .I == .N] is a "is it the last row" vector
  henData[!duplicated(rleid(henData$Zone)) | henData[, .I == .N]]
}

# Add a Duration column to a timeline
# Calculated for each row as the time difference to next row; 0 for the last row
# Parameter: a single-hen timeline (ONLY single-hen timelines, otherwise values will be invalid!)
# Output: a timeline with durations
addDurations <- function(henData) {
  newData <- copy(henData)
  # Calculate duration of entry as difference to next entry
  newData[, Duration := (shift(Time, type="lead") - Time)]
  # Set duration for last entry as 0
  newData[nrow(newData), Duration := as.difftime(0, units="secs")]
  return(data.table(newData))
}

# Constructs a "HH:MM:DD" string for a given number of seconds since start of day
# totalSeconds - integer, number of seconds since midnight
secondsToHMS <- function(totalSeconds) {
  totalSeconds <- round(totalSeconds)
  hours <- totalSeconds %/% 3600
  minutes <- (totalSeconds %% 3600) %/% 60
  seconds <- totalSeconds %% 60
  
  return(sprintf("%d:%02d:%02d", hours, minutes, seconds))
}

###
# Loading the data

# Get the list of .csv filenames under a given path
filenames <- list.files(path = inDir, pattern = "\\.csv$", ignore.case=TRUE)

# Load all the files into separate data.table tables
filesData <- lapply(filenames, function(filename) {
  # To have any warnings printed out immediately
  options(warn=1)
  # To correlate warnings with the file
  print(filename)
  
  # Read lines from file
  con <- file(file.path(inDir, filename))
  lines <- readLines(con)
  close(con)
  
  # Regular expression pattern
  # Example string: "28.04.2021 16:43:12.66 DA760104 16400088"
  pattern <- "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}(\\.\\d{1,2})? [0-9A-F]{8} \\d{8}$"
  
  # Drop lines that don't match the pattern (as a result of a broken write)
  lines <- lines[grepl(pattern, lines)]
  
  # Glue lines together back into a file + last line break
  text <- paste0(paste0(lines, collapse="\n"), "\n")
  
  # Parse as data.table
  fread(text=text)
})

# Merge data in a single data.table
# It can still be unordered
data <- rbindlist(filesData)
# Free up separate files' data
rm(filesData)

###
# Interpreting and cleaning the data

# Data has the following shape at this point (auto-assigned column names:
# V1: "DD.MM.YYYY" date
# V2: "HH:MM:SS.SS" timestamp
# V3: Hexadecimal ID of the antenna that detected the tag
# V4: Decimal ID of the tag that was detected
# Note that it can be unordered
# Also note that the timestamps are not super-precise


# Map tracker IDs to Hen codes
data[, Hen := revalue(V3, henMapping)]
# Drop unrecognized Hen IDs
data <- data[Hen %in% henMapping]

# Parse datetime with lubridate - understands fractional seconds
data[, Time := dmy_hms(paste(V1, V2))]

# Map antenna IDs to zone names
data[, Zone := revalue(as.character(V4), zoneMapping)]
# It's possible to further use revalue to group together antennas into zone

# Map antenna IDs to pen names
data[, Pen := revalue(as.character(V4), penMapping)]

# Map antenna IDs to perspecitves
data[, Perspective := revalue(as.character(V4), perspectiveMapping)]

# Drop raw columns
data[, `:=`(V1=NULL, V2=NULL, V3=NULL, V4=NULL)]

# Order by timestamp then hen
setkeyv(data, c("Time", "Hen"))

# Output "raw" timeline with all events and unknown antennae
fwrite(data, file=file.path(
  getwd(),
  outDir,
  paste0("full_raw_timeline_unident.csv")
), sep=";")

# Output "raw" timeline with all events
fwrite(data[!startsWith(Zone,"A"), Zone := "Unknown"], file=file.path(
  getwd(),
  outDir,
  paste0("full_raw_timeline.csv")
), sep=";")

# Round timestamps
data[, Time := round_date(Time, unit="secs")]

# Remove non-transitions:
# * Look at each hen's timeline individually (applyPerHen)
# * Remove records that have the same zone as the previous one (except last one)
sparseData <- applyPerHen(compactHenData, data)

# Split data into individual hen timelines
for (hen in henMapping) {
  henData <- sparseData[Hen == hen]
  henData <- addDurations(henData)
  
  # Add various fields to match requested output - optional
  henData[, Onset_Time := sapply(
    as.numeric(
      Time - floor_date(Time, unit="days"),
      units="secs"
    ),
    secondsToHMS
  )]
  henData[, Duration_Time := sapply(as.numeric(Duration, units="secs"), secondsToHMS)]
  henData[, Offset_Time := sapply(
    as.numeric(
      (Time + Duration) - floor_date(Time + Duration, unit="days"),
      units="secs"
    ),
    secondsToHMS
  )]
  
  # Data with some zones potentially not mapped
  fwrite(henData, file=file.path(
    getwd(),
    outDir,
    paste0(hen, "_timeline.csv")
  ), sep=";")
  
  # Mark zones that were not mapped properly as "Unknown"
  henData[!startsWith(Zone,"A"), Zone := "Unknown"]
  
  # Recompact and recalculate durations (because now there are more duplicates)
  henData <- compactHenData(henData)
  henData <- addDurations(henData)
  
  # Add various fields to match requested output - optional
  henData[, Onset_Time := sapply(
    as.numeric(
      Time - floor_date(Time, unit="days"),
      units="secs"
    ),
    secondsToHMS
  )]
  henData[, Duration_Time := sapply(as.numeric(Duration, units="secs"), secondsToHMS)]
  henData[, Offset_Time := sapply(
    as.numeric(
      (Time + Duration) - floor_date(Time + Duration, unit="days"),
      units="secs"
    ),
    secondsToHMS
  )]
  
  # Data with Unknown
  fwrite(compactHenData(henData), file=file.path(
    getwd(),
    outDir,
    paste0(hen, "_Unknown_timeline.csv")
  ), sep=";")
  
  # Data with Unknown transition simply dropped
  fwrite(compactHenData(henData[Zone != "Unknown"]), file=file.path(
    getwd(),
    outDir,
    paste0(hen, "_noUnknown_timeline.csv")
  ), sep=";")
}


