# Main data structure
library("data.table")
# Time manipulation
library("lubridate")
# Mapping vector values
library("plyr")

###
# Settings

inDirS <- "hen_timeline"
inDirV <- "validation_timeline_pass2"

outDir <- "event_validation_corr"

source("mappings.R")

###

# Loads hen data file, converting timestamps to POSIX and setting up a key
# Parameter: path to file
# Output: a timeline
loadTimeline <- function(path) {
  # Load data
  data <- fread(path, sep=";", header=TRUE, select = c("Time", "Hen", "Zone", "Pen", "Perspective"))
  # Sanity check that required columns are present
  stopifnot(all(c("Time", "Hen", "Zone", "Pen", "Perspective") %in% colnames(data)))
  # Fast-convert string timestamps into POSIX
  data[, Time := lubridate::ymd_hms(Time)]
  # Make sure empty strings are converted to NA
  data[Zone == "", Zone := NA]
  # Set indexing key (also sorts)
  setkeyv(data, c("Time", "Hen"))
}

###

fullEventTimeline <- loadTimeline(
  file.path(getwd(), inDirS, "full_raw_timeline_unident.csv")
)

fullEventTimeline[, Tier := revalue(Zone, tierMapping)]

fullEventTimeline[!startsWith(Zone,"A") & !startsWith(Tier,"T"), Tier := "Unknown"]
fullEventTimeline[!startsWith(Zone,"A") & !startsWith(Tier,"T"), Perspective := "Unknown"]


closestValue <- function(data, column, timestamp) {
  prevCandidate <- last(data[Time <= timestamp])
  nextCandidate <- first(data[Time >= timestamp])
  
  if (nrow(prevCandidate) && nrow(nextCandidate)) {
    if(as.numeric(nextCandidate[1, Time] - timestamp, unit="secs") 
       < as.numeric(timestamp - prevCandidate[1, Time], unit="secs"))
    {
      return(nextCandidate[1, get(column)])
    } else {
      return(prevCandidate[1, get(column)])
    }
  } else if (nrow(prevCandidate)) {
    return(prevCandidate[1, get(column)])
  } else if (nrow(nextCandidate)) {
    return(nextCandidate[1, get(column)])
  } else {
    return(NA)
  }
}

closestDistance <- function(data, timestamp) {
  closestTime <- closestValue(data, "Time", timestamp)
  
  if (!is.na(closestTime)) {
    return(as.numeric(closestTime - timestamp, unit="secs"))
  } else {
    return(Inf)
  }
}

closestDistanceByZone <- function(data, zone, pen, timestamp) {
  return(closestDistance(data[(Zone == zone) & (Pen == pen)], timestamp))
}

closestDistanceByTier <- function(data, tier, timestamp) {
  return(closestDistance(data[Tier == tier], timestamp))
}

closestZoneByTier <- function(data, tier, timestamp) {
  return(closestValue(data[Tier == tier], "Zone", timestamp))
}

closestDistanceByTriple <- function(data, tier, perspective, pen, timestamp) {
  return(closestDistance(data[(Tier == tier) & (Perspective == perspective) & (Pen == pen)], timestamp))
}

closestZoneByTriple <- function(data, tier, perspective, pen, timestamp) {
  return(closestValue(data[(Tier == tier) & (Perspective == perspective) & (Pen == pen)], "Zone", timestamp))
}

valPerspectives <- c("16WG", "16BG", "17WG", "17BG")
hens <- c("GG", "GS", "PS")

valPerspectiveToPen <- c(
  "16WG" = "P16",
  "16BG" = "P16",
  "17WG" = "P17",
  "17BG" = "P17"
)

valPerspectiveToPerspective <- c(
  "16WG" = "AKB",
  "16BG" = "BG",
  "17WG" = "AKB",
  "17BG" = "BG"
)

isAntennaEvent <- function(zoneData) {
  return(startsWith(zoneData, "A"))
}

dataList <- list()

for (hen in hens) {
  henData <- fullEventTimeline[Hen == hen]
  
  for (perspective in valPerspectives) {
    valData <- fread(
      file.path(getwd(), inDirV, paste0(hen, "_", perspective, "_timeline.csv"))
    )
    valData[, Hen := hen]
    valData[, Pen := valPerspectiveToPen[perspective]]
    
    valData <- valData[isAntennaEvent(Position)]
    
    valData[, WrongMapping := valPerspectiveToPerspective[perspective] != revalue(Position, zoneToPerspective)]
    # Fix wrong mapping
    valData[WrongMapping == TRUE, Position := revalue(Position, oppositeMapping)]
    
    valData[, Tier := revalue(Position, tierMapping)]
    valData[, Perspective := revalue(Position, zoneToPerspective)]
    
    vfun <- Vectorize(closestZoneByTriple, vectorize.args = c("tier", "perspective", "pen", "timestamp"))
    valData[, ClosestZoneByTriple := vfun(henData, Tier, Perspective, Pen, Time)]
    
    vfun <- Vectorize(closestDistanceByTriple, vectorize.args = c("tier", "perspective", "pen", "timestamp"))
    valData[, DistanceToTriple := vfun(henData, Tier, Perspective, Pen, Time)]
    
    vfun <- Vectorize(closestDistanceByTier, vectorize.args = c("tier", "timestamp"))
    valData[, DistanceToTier := vfun(henData, Tier, Time)]
    
    vfun <- Vectorize(closestZoneByTier, vectorize.args = c("tier", "timestamp"))
    valData[, ClosestZoneByTier := vfun(henData, Tier, Time)]
    
    vfun <- Vectorize(closestDistanceByZone, vectorize.args = c("zone", "pen", "timestamp"))
    valData[, DistanceToZone := vfun(henData, Position, Pen, Time)]
    
    vfun <- Vectorize(closestDistance, vectorize.args = c("timestamp"))
    valData[, ClosestDistance := vfun(henData, Time)]
    
    vfun <- Vectorize(closestValue, vectorize.args = c("timestamp"))
    valData[, ClosestZone := vfun(henData, "Zone", Time)]
    
    vfun <- Vectorize(closestValue, vectorize.args = c("timestamp"))
    valData[, ClosestTier := vfun(henData, "Tier", Time)]
    
    vfun <- Vectorize(closestValue, vectorize.args = c("timestamp"))
    valData[, ClosestPerspective := vfun(henData, "Perspective", Time)]
    
    vfun <- Vectorize(closestValue, vectorize.args = c("timestamp"))
    valData[, ClosestPen := vfun(henData, "Pen", Time)]
    
    valData[, MappingFlipped := WrongMapping]
    valData[, WrongMapping := NULL]
    
    valData[abs(DistanceToTier) <= 60, DetectedInTier := "Detected in tier within 1 minute"]
    valData[abs(DistanceToTriple) <= 60, DetectedInTier := "Detected in tier+side within 1 minute"]
    valData[abs(DistanceToZone) <= 60, DetectedInTier := "Detected in zone within 1 minute"]
    valData[Tier == ClosestTier, SameClosestTier := "Same Tier"]
    valData[Pen == ClosestPen, SameClosestPen := "Same Pen"]
    valData[Perspective == ClosestPerspective, SameClosestPerspective := "Same Perspective"]
    valData[Position == ClosestZone, SameClosestZone := "Same Zone"]
    
    valData[, Time := Time - lubridate::hours(2)]
    
    fwrite(valData, file=file.path(
      getwd(),
      outDir,
      paste0(hen, "_", perspective, "_eventValidation.csv")
    ), sep=";")
    
    dataList[[length(dataList) + 1]] <- copy(valData)
  }
}

fwrite(rbindlist(dataList), file=file.path(
  getwd(),
  outDir,
  paste0("combined_eventValidation.csv")
), sep=";")
