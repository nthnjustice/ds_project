###################################################################################################
# Set Working Environment and Import Dependencies
###################################################################################################

setwd("C:/Users/Nathan/Desktop/Project 1/Code")

library(sp)
library(rgdal)
library(rgeos)
library(dismo)
library(geosphere)

###################################################################################################
# Load Datasets and Initialize Global Variables
###################################################################################################

# load cleaned camp data
CAMP <- read.csv("../Data/Clean/Camp/Camp_Clean.csv", stringsAsFactors=F)
# load cleaned spillover data
IP <- read.csv("../Data/Clean/Spillover/IP_Clean.csv", stringsAsFactors=F)
# load cleaned horse property data
HORSE <- read.csv("../Data/Clean/Horse/Horse_Merged_Clean.csv", stringsAsFactors=F)
# load study area spatial object
studyAreaLayer <- readOGR(dsn="../Data/Clean/Study Area", layer="StudyArea")

# initialize projection definitions for spatial objects
longlat <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
utm56 <- CRS("+proj=utm +zone=56 ellps=WGS84")

###################################################################################################
# Build Subset of Horse Properties that are Within the Study Area
###################################################################################################

# create spatial object of horse properties
HORSE_sp <- SpatialPointsDataFrame(data.frame(HORSE$Long, HORSE$Lat), HORSE, proj4string=longlat)

# create boolean vector of horse properties that intersect the study area
HORSE_sa <- over(HORSE_sp, studyAreaLayer)
# initialize storage for the indices of intersecting properties
inSA <- vector()
# loop through each boolean
for (i in 1:nrow(HORSE_sa)) {
  if (!is.na(HORSE_sa$ID[i])) {
    # append index of horse property if it's within study area
    inSA <- c(inSA, i)
  }
}

# save subset of horse property data within study area
inSA <- HORSE[which(HORSE$Id %in% inSA),]
# create spatial object
HORSE_sp <- SpatialPointsDataFrame(data.frame(inSA$Long, inSA$Lat), inSA, proj4string=longlat)

# append unique numeric identifier
HORSE_sp$Id <- seq(1, nrow(HORSE_sp@data), 1)
# rename rows
row.names(HORSE_sp) <- seq(1, nrow(HORSE_sp@data), 1)
# catch the dataframe behind spatial object
HORSE <- HORSE_sp@data

###################################################################################################
# Determine Which Camps are the Closest to an IP and Occupied in the Same Month
###################################################################################################

# purpose: subset the camp dataframe for rows with flying fox population counts in a given month
  # not equal to NA
# input: string for month of interest, camp dataframe
# output: subset of camp dataframe
makeCampPool <- function(month, CAMP) {
  if (month == "June") {
    return(subset(CAMP, Jun_Pres == 1))
  } else if (month == "July") {
    return(subset(CAMP, Jul_Pres == 1))
  } else if (month == "August") {
    return(subset(CAMP, Aug_Pres == 1))
  } else if (month == "October") {
    return(subset(CAMP, Oct_Pres == 1))
  }
}

# ititialize Boolean column for closest occupied camp to an IP
CAMP$Close_To_Ip <- 0
# loop through each IP
for (i in 1:nrow(IP)) {
  # ititialize storage for distances
  dists <- vector()
  # subset camps occupied in the same month as the IP in focus
  campPool <- makeCampPool(IP$Month[i], CAMP)

  # loop through each camp and compute the distance
  for (j in 1:nrow(campPool)) {
    dists <- c(dists, distm(c(IP$Long[i], IP$Lat[i]), c(campPool$Long[j], campPool$Lat[j])))
  }

  # update Boolean value for the closest occupied camp to the IP in focus
  CAMP$Close_To_Ip[campPool$Id[match(min(dists), dists)]] <- 1
}

###################################################################################################
# Append the Coordinates of the Nearest Horse Property
###################################################################################################

# loop through each camp
for (i in 1:nrow(CAMP)) {
  # create spatial object of horse properties
  horse <- SpatialPointsDataFrame(data.frame(HORSE$Long, HORSE$Lat), HORSE, proj4string=longlat)
  horse <- spTransform(horse, utm56)

  # create spatial object of camp in focus
  camp <- SpatialPoints(data.frame(CAMP$Long[i], CAMP$Lat[i]), proj4string=longlat)
  camp <- spTransform(camp, utm56)

  # find the closest horse property
  dists <- gDistance(camp, horse, byid=T)
  closest <- HORSE[match(min(dists), dists),]

  # append the coordinates
  CAMP$Near_Hp_Coords[i] <- closest$Coords
}

###################################################################################################
# Append Information About Surrounding Horse Properties
###################################################################################################

# merge horse property and IP data
RPIP <- rbind(IP, HORSE)
# append unique numeric identifier
RPIP$Id <- seq(1, nrow(RPIP), 1)
# update row names
row.names(RPIP) <- seq(1, nrow(RPIP), 1)

# loop through each camp
for (i in 1:nrow(CAMP)) {
  # create spatial object for properties
  rpip <- SpatialPointsDataFrame(data.frame(RPIP$Long, RPIP$Lat), RPIP, proj4string=longlat)

  # create spatial object for camp in focus
  camp <- SpatialPoints(data.frame(CAMP$Long[i], CAMP$Lat[i]), proj4string=longlat)
  camp <- spTransform(camp, utm56)

  # draw 20km buffers around each camp
  camp_buff <- gBuffer(camp, width=20000, quadsegs=100)
  camp_buff <- spTransform(camp_buff, longlat)

  # compute the number of properties within 20km of the camp in focus
  within <- over(rpip, camp_buff)
  count <- 0
  for (j in within) {
    if (!is.na(j)) {
      count <- count + 1
    }
  }

  # append Boolean column for if a property is within 20km of the camp in focus
  if (count > 0) {
    CAMP$Prop_In_20km[i] <- 1
  } else {
    CAMP$Prop_In_20km[i] <- 0
  }

  # append new column
  CAMP$Prop_Num_20km[i] <- count

  # append the distance to the nearest property to the camp in focus
  rpip <- spTransform(rpip, utm56)
  dists <- gDistance(camp, rpip, byid=T)

  CAMP$Prop_Near_Dist[i] <- min(dists)
}

###################################################################################################
# Append Columns for Species Count Averages Across all 4 Months
###################################################################################################

###################################################################################################
# Append Event Column for Model Clarity
###################################################################################################

# copy column for model clarity
for (i in 1:nrow(CAMP)) {
  if (CAMP$Close_To_Ip[i]) {
    CAMP$Event[i] <- 1
  } else {
    CAMP$Event[i] <- 0
  }
}

###################################################################################################
# Update Close_To_Ip Column for Validating Horse Properties
###################################################################################################

# ititialize coordinates of camps that were the nearest camp to 2013 spillover events
campDrops <- data.frame(Lat=c(-27.468, -30.726, -31.084), Long=c(152.5886, 152.926, 152.843))
campDrops$Coords <- paste(campDrops$Lat, campDrops$Long, sep=", ")

# change column value to sentinal to signal these camps should not be used when validating horse
  # properties
for (i in 1:nrow(CAMP)) {
  for (j in 1:nrow(campDrops)) {
    if (CAMP$Coords[i] == campDrops$Coords[j]) {
      CAMP$Close_To_Ip[i] <- -1
    }
  }
}

# save results
write.csv(CAMP, "../Data/Final/Camp/Camp_Final.csv", row.names=F)

###################################################################################################
# Validate Horse Properties for Case-Control Design
###################################################################################################

# purpose: create subset of horse properties that are viable control candidates based on whether or
  # not they intersect a 20km buffer around all of the camps occupied in a given month
# input: month of interest, camp dataframe, horse property dataframe
# output: subset of viable horse property controls for a given month
validateHorses <- function(month, CAMP, HORSE) {
  # exclude camps nearest a spillover event, including those nearest 2013 spillover events
  camps <- subset(CAMP, Close_To_Ip == 0)

  # subset camps based on occupancy in the specified month
  if (month == "June") {
    camps <- subset(camps, Jun_Pres == 1)
  } else if (month == "July") {
    camps <- subset(camps, Jul_Pres == 1)
  } else if (month == "August") {
    camps <- subset(camps, Aug_Pres == 1)
  } else if (month == "October") {
    camps <- subset(camps, Oct_Pres == 1)
  }

  # create spatial object of camps
  camps <- SpatialPoints(data.frame(camps$Long, camps$Lat), proj4string=longlat)
  camps <- spTransform(camps, utm56)

  # draw 20km buffers around each camp
  camps_buff <- gBuffer(camps, width=20000, quadsegs=100)
  camps_buff <- spTransform(camps_buff, longlat)

  # create spatial object of horse properties
  horses <- SpatialPointsDataFrame(data.frame(HORSE$Long, HORSE$Lat), HORSE, proj4string=longlat)

  # determine which horse properties intersect the camp buffers
  within <- over(horses, camps_buff)

  # store the indices of intersecting horse properties
  validIds <- vector()
  for (i in 1:length(within)) {
    if (!is.na(within[i])) {
      validIds <- c(validIds, i)
    }
  }

  # return the subset of horse properties that intersect 20km buffers drawn around the camps
    # occupied in the specified month
  return(HORSE[which(HORSE$Id %in% validIds),])
}

# validate horse property control candidates for each month
horse6 <- validateHorses("June", CAMP, HORSE)
horse7 <- validateHorses("July", CAMP, HORSE)
horse8 <- validateHorses("August", CAMP, HORSE)
horse10 <- validateHorses("October", CAMP, HORSE)

# update month column
horse6$Month <- "June"
horse7$Month <- "July"
horse8$Month <- "August"
horse10$Month <- "October"

# merge horse property control candidates
HORSE_val <- rbind(horse6, horse7, horse8, horse10)

###################################################################################################
# Determine the Coordinates of the Nearest Camp to Each RPIP
###################################################################################################

# purpose: append coordinates of the nearest camp to each property
# input: properties dataframe, camp dataframe, Boolean signaling if the first argument represents
  # spillover events - matters because the camps closest camps to a spillover event must be masked
  # from all other properties
# output: updated version of the passed property dataframe
findNearestCamp <- function(df, CAMP, isIP) {
  # loop through each property
  for (i in 1:nrow(df)) {
    # subset camps based on whether or not properties represent spillover events
    if (!isIP) {
      camps <- subset(CAMP, State == df$State[i])
      camps <- subset(camps, Close_To_Ip == 0)
    } else {
      camps <- subset(CAMP, Close_To_Ip == 1)
    }

    # subset camps for those occupied in the month matching the property in focus
    camps <- makeCampPool(df$Month[i], camps)

    # create spatial object for camps
    camps <- SpatialPointsDataFrame(data.frame(camps$Long, camps$Lat), camps, proj4string=longlat)
    camps <- spTransform(camps, utm56)

    # create spatial object for property in focus
    rpip <- SpatialPoints(data.frame(df$Long[i], df$Lat[i]), proj4string=longlat)
    rpip <- spTransform(rpip, utm56)

    # determine the closest camp to the property in focus
    dists <- gDistance(rpip, camps, byid=T)
    closest <- camps@data[match(min(dists), dists),]

    # append coordinates of the closest camp
    df$Near_Camp_Coords[i] <- closest$Coords
  }

  # return updated dataframe
  return(df)
}

# append coordinates of the nearest camp to properties
HORSE_val <- findNearestCamp(HORSE_val, CAMP, F)
IP <- findNearestCamp(IP, CAMP, T)

# save results
write.csv(HORSE, "../Data/Final/Horse/Horse_Final.csv", row.names=F)
write.csv(IP, "../Data/Final/Spillover/IP_Final.csv", row.names=F)

###################################################################################################
# Build RPIP Dataset
###################################################################################################

# merge spillover events with all other horse properties
RPIP <- rbind(IP, HORSE_val)
# update unique numeric identifier
RPIP$Id <- seq(1, nrow(RPIP), 1)
# update row names
row.names(RPIP) <- seq(1, nrow(RPIP), 1)

# loop through each property
for (i in 1:nrow(RPIP)) {
  # subset the camp that is the closest camp to the property in focus
  camp <- subset(CAMP, Coords == RPIP$Near_Camp_Coords[i])

  # loop through selection of camp columns and append the information to the property in focus
  for (j in 1:17) {
    colName <- paste("Camp_", colnames(camp)[j], sep="")
    RPIP[[colName]][i] <- camp[1,j]
  }

  # append information about the species population counts of the nearest camp to the property in
    # focus based on the month of the property
  if (RPIP$Month[i] == "June") {
    RPIP$Bff[i] <- camp$Bff6
    RPIP$GHff[i] <- camp$GHff6
    RPIP$LRff[i] <- camp$LRff6
    RPIP$Tot[i] <- camp$Tot6
  } else if (RPIP$Month[i] == "July") {
    RPIP$Bff[i] <- camp$Bff7
    RPIP$GHff[i] <- camp$GHff7
    RPIP$LRff[i] <- camp$LRff7
    RPIP$Tot[i] <- camp$Tot7
  } else if (RPIP$Month[i] == "August") {
    RPIP$Bff[i] <- camp$Bff8
    RPIP$GHff[i] <- camp$GHff8
    RPIP$LRff[i] <- camp$LRff8
    RPIP$Tot[i] <- camp$Tot8
  } else if (RPIP$Month[i] == "October") {
    RPIP$Bff[i] <- camp$Bff10
    RPIP$GHff[i] <- camp$GHff10
    RPIP$LRff[i] <- camp$LRff10
    RPIP$Tot[i] <- camp$Tot10
  }

  # loop through remaining camp columns and append the information to the property in focus
  for (j in 34:ncol(camp)) {
    colName <- paste("Camp_", colnames(camp)[j], sep="")
    RPIP[[colName]][i] <- camp[1,j]
  }
}

# save results
write.csv(RPIP, "../Data/Final/RPIP/RPIP_Final.csv", row.names=F)
