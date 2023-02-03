# Main data structure
library("data.table")
# Time manipulation
library("lubridate")
# Mapping vector values
library("plyr")

# Loading CSV-converted validation data
validationData <- fread("Validation_masha.csv")

## Checking data for anomalies
# Marking line numbers to correlate with the original spreadsheet
validationData <- validationData[, LineNum := .I + 1]
# Detecting anomalous data rows
anomalies <- validationData[
  Anwesend == "" |                                                       # Row without a hen specified 
    ((Anwesend == "gg") & (gg_Position == "")) |                           # Row specified for hen gg but no position marked
    ((Anwesend == "gs") & (gs_Position == "")) |                           # Row specified for hen gs but no position marked
    ((Anwesend == "ps") & (ps_Position == "")) |                           # Row specified for hen ps but no position marked
    ((gg_Position != "") + (gs_Position != "") + (ps_Position != "") > 1)  # More than one position specified in the same row
]
# Print out anomalies
print(anomalies)
# Filtering out the anomalies
cleanValidationData <- validationData[!(LineNum %in% anomalies[, LineNum])]

## Populating columns
# Reconstructing the date: DataGroup is e.g. "pen 17BG 22.04", take last 5 characters, append ".2021" -> "22.04.2021"
cleanValidationData[, Date := paste0(substr(DataGroup, nchar(DataGroup)-4, nchar(DataGroup)), ".2021")] 

# The validation file has 3 fields: Onset_Time (when detection starts), Duration and Offset_Time (when detection ends)
# Parsing Date + Onset_Time as the beginning timestamp
cleanValidationData[, Timestamp := dmy_hms(paste(Date, Onset_Time))]
# Parsing Duration_Time as HH:MM:SS and storing it in seconds
cleanValidationData[, Duration := as.numeric(hms(Duration_Time), unit="secs")]
# Calculating start + duration (should be same as Offset_Time)
cleanValidationData[, DurationTimestamp := Timestamp + seconds(Duration)]
# Parsing Date + Offset_Time as timestamp
cleanValidationData[, OffsetTimestamp := dmy_hms(paste(Date, Offset_Time))]

# Displaying distribution of difference between calculated and included Offset time
# If there are differences of more than +- 1 second, the validation data is suspect
# For this round, this was all OK
print("Counts of mismatch between Onset + Duration vs Offset")
cleanValidationData[, .N, by=(OffsetTimestamp - DurationTimestamp)]

## Remove everything but data marked as certain
cleanValidationData <- cleanValidationData[certainty == "sure"]

## Aux data lists
# List of perspectives (here, perspective is pen# + side)
perspectives <- c("16WG", "16BG", "17WG", "17BG")
# List of hens
hens <- c("gg", "gs", "ps")

## Collapsing the 3 separate position columns into one
for (hen in hens) {
  # Constructing the name of the hen-specific column, e.g. "gg" -> "gg_Position"
  posColumn <- paste0(hen, "_Position")
  # Copy data from that named column into column Position for rows of that hen
  cleanValidationData[Anwesend == hen, Position := .SD, .SDcols = c(posColumn)]
  # Delete the hen-specific column
  cleanValidationData[, (posColumn) := NULL]
}

## Splitting data into perspectives and hens
for (perspective in perspectives) {
  # Filter out data where DataGroup includes perspective as substring
  PerspectiveData <- cleanValidationData[grepl(perspective, DataGroup)]
  for (hen in hens) {
    # Filter out further data for a specific hen
    HenPerspectiveData <- PerspectiveData[Anwesend == hen]
    # Order by (onset) timestamp
    setkeyv(HenPerspectiveData, "Timestamp")
    
    # Mark the timestamp of the next data point
    HenPerspectiveData[, NextTimestamp := shift(Timestamp, type="lead")]
    
    # Construct a fresh data table with two columns (Time, Position)
    output <- data.table(
      Time = POSIXct(),
      Position = character()
    )
    
    # Go over every row of filtered data
    for (n in 1:nrow(HenPerspectiveData)) {
      # Add the row's data to the output table
      output <- rbind(output, data.table(
        Time = HenPerspectiveData[n, Timestamp],
        Position = HenPerspectiveData[n, Position]
      ))
      # If the difference of the next data point is big (e.g. day boundary), insert an additional "No data" row
      if (
        !is.na(HenPerspectiveData[n, NextTimestamp]) &              # If not last row
        HenPerspectiveData[n, NextTimestamp - OffsetTimestamp] > 2  # and next onset is more than 2 seconds later than offset
      ) {
        # Add a "No data" row starting with OffsetTimestamp 
        output <- rbind(output, data.table(
          Time = HenPerspectiveData[n, OffsetTimestamp],
          Position = "No data"
        ))
      }
    }
    
    # Write the output
    fwrite(output, file=file.path(
      getwd(),
      "validation_timeline_pass2",
      paste0(toupper(hen), "_", perspective, "_timeline.csv")
    ), sep=";")
  }
}
