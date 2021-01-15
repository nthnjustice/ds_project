###################################################################################################
# Set Working Environment and Import Dependencies
###################################################################################################

setwd("C:/Users/Nathan/Desktop/Project 1/Code")

library(sp)
library(rgdal)
library(rgeos)
library(dismo)

###################################################################################################
# Build and Clean Camp Dataset
###################################################################################################

# load source data
CAMP <- read.csv("../Data/Source/Camp/Camp_Download.csv", stringsAsFactors=F)
# trim invalid columns
CAMP <- CAMP[,2:ncol(CAMP)]
# rename columns
colnames(CAMP) <- c("Id", "Roost", "Lat", "Long", "State",  "Cont", "Status", "Post_10",
                    "Post_07", "Hist", "Bff6", "GHff6", "LRff6", "Tot6", "Bff7", "GHff7",
                    "LRff7", "Tot7", "Bff8", "GHff8", "LRff8", "Tot8", "Bff10", "GHff10",
                    "LRff10", "Tot10")

# load supplemental camp rows
campAdd <- read.csv("../Data/Source/Camp/Camp_Download_Additional.csv", stringsAsFactors=F)
# trim invalid rows
campAdd <- subset(campAdd, ROOST != "")
# rename columns
colnames(campAdd) <- c("Id", "Roost", "Lat", "Long", "State",  "Cont", "Status", "Post_10",
                       "Post_07", "Hist", "Bff6", "GHff6", "LRff6", "Tot6", "Bff7", "GHff7",
                       "LRff7", "Tot7", "Bff8", "GHff8", "LRff8", "Tot8", "Bff10", "GHff10",
                       "LRff10", "Tot10")

# merge supplemental rows with main dataframe
CAMP <- rbind(CAMP, campAdd)
# remove duplicated rows
CAMP <- CAMP[which(duplicated(CAMP$Roost) == F),]
# append unique identifier composed of lat-long coordinate pairs
CAMP$Coords <- paste(CAMP$Lat, CAMP$Long, sep=", ")

# load dataset with landuse information
campLA <- read.csv("../Data/Source/Camp/Camp_Download_Landuse.csv", stringsAsFactors=F)
# remove duplicated rows
campLA <- campLA[which(duplicated(campLA$ROOST) == F),]
# append unique identifier comosed of lat-long coordinate pairs
campLA$Coords <- paste(campLA$LAT, campLA$LONG, sep=", ")

# loop through each camp
for (i in 1:nrow(CAMP)) {
  # loop through each landuse entry
  for (j in 1:nrow(campLA)) {
    # append landuse information to the corresponding camp in the main dataframe
    if ((CAMP$Roost[i] == campLA$ROOST[j]) && (CAMP$Coords[i]) == campLA$Coords[j]) {
      CAMP$Fa_Size[i] <- campLA$fa.size[j]
      CAMP$Dist_Urb[i] <- campLA$dist_urb[j]
      CAMP$Prop_Mod[i] <- campLA$prop_mod[j]
      CAMP$Prop_Urb[i] <- campLA$prop_urb[j]
      CAMP$Prop_Veg[i] <- campLA$prop_veg[j]
      CAMP$Cont_Bff[i] <- campLA$BFF_cont[j]
    }
  }
}

# append unique numeric identifier
CAMP$Id <- seq(1, nrow(CAMP), 1)
# rename rows
rownames(CAMP) <- seq(1, nrow(CAMP), 1)

# standardize state values
CAMP$State[CAMP$State == "Qld"] <- "Queensland"
CAMP$State[CAMP$State == "qld"] <- "Queensland"
CAMP$State[CAMP$State == "nsw"] <- "New South Wales"

# hot-fix trailing whitespace in roost name
CAMP$Roost[which(CAMP$Roost == "Woolgoolga ")] <- "Woolgoolga"

###################################################################################################
# Fix Erroneous, Meaningless, and Inconsistent Values in Camp Dataset
###################################################################################################

### Issue 1. ###
# problem: value of the 'Status' column is an empty string and has no meaning
# resolution: reassign the 'Status' column value with NA and reassign the corresponding 'Cont'
  # column value with 0

for (i in 1:nrow(CAMP)) {
  if (CAMP$Status[i] == "") {
    CAMP$Status[i] <- NA
    CAMP$Cont[i] <- 0
  }
}

### Issue 2. ###
# problem: value of the 'Cont' column doesn't reflect the corresponding value of the 'Status' column,
  # the value of the 'Cont' column is based on the value of the 'Status' column and needs to be
  # a consistent reflection
# resolution: reassign the 'Cont' column value with 1 when the corresponding value of the 'Status'
  # column is equal to "c"

for (i in 1:nrow(CAMP)) {
  if (!is.na(CAMP$Status[i]) && CAMP$Status[i] == "c" && CAMP$Cont[i] == 0) {
    CAMP$Cont[i] <- 1
  }
}

### Issue 3. ###
# problem: value of one or multiple population count is equal to the string "PRESENT" and is
  # inconsistent with the rest of the column's data because all values in these columns should have
  # a numeric data type
# resolution: reassign the population count column value with NA because a raw number cannot be
  # determined retrospectively

# store the camps that are going to be modified because they will need to be accounted for when the
  # columns describing flying fox presence/absence in each month are created
campWithPres <- subset(CAMP, Bff6 == "PRESENT")

CAMP[CAMP == "PRESENT"] <- NA

### Issue 4. ###
# problem: some values of the 'Bff10' column that are meant to be NA are instead the string " NA"
# resolution: reassign the 'Bff10' column value with NA where it equals the string " NA"

for (i in 1:nrow(CAMP)) {
  if (!is.na(CAMP$Bff10[i]) && CAMP$Bff10[i] == " NA") {
    CAMP$Bff10[i] <- NA
  }
}

### Issue 5. ###
# problem: the value of the 'Cont' column is equal to 1 (true) but the values of the corresponding
  # population count columns are not valid across all four months and is inconsistent with the rest
  # of the dataset
# details:
  # a) a value of 0 in any population count column for a given month means the camp was surveyed
    # in said month but no flying foxes were observed
  # b) a value of NA in all four population count columns for a given month means the camp was not
    # surveyed in said month
  # c) when the value of the species population count columns for a given month all equal NA and the
    # corresponding total population column in said month has a value not equal to NA implies the
    # camp was surveyed in said month but the total number of flying foxes (if any) were not
    # allocated across species
  # d) the value of the 'Cont' clumn was determined beyond the temporal scope of the dataset and
    # should not be changed
# resolution: instances in which a total population count for a given month was reported, not equal
  # to NA, and not allocated across species, but the corresponding species population count columns
  # in said month have a value of 0 should be reassigned to NA
# example:
  # a scenario like
    # Bff6 == 0
    # GHff6 == 0
    # LRff6 == 0
    # Tot6 == 100
  # should be changed to
    # Bff6 == NA
    # GHff6 == NA
    # LRff6 == NA
    # Tot6 == 100

CAMP$Bff6[which(CAMP$Roost == "Casino")] <- NA
CAMP$GHff6[which(CAMP$Roost == "Casino")] <- NA
CAMP$LRff6[which(CAMP$Roost == "Casino")] <- NA

CAMP$Bff10[which(CAMP$Roost == "Bongaree, Shirley Creek")] <- NA
CAMP$GHff10[which(CAMP$Roost == "Bongaree, Shirley Creek")] <- NA
CAMP$LRff10[which(CAMP$Roost == "Bongaree, Shirley Creek")] <- NA

### Issue 6. ###
# problem: value of the 'Bff' and 'LRff' columns in a given month have a raw numeric count while the
  # value of the corresponding 'GHff' column in said month has a value equal to NA is erroneous
  # because the species allocation of the population survey total is all or nothing
# resolution: reassign the 'GHff' column value under this circumstance with 0

for (i in 1:nrow(CAMP)) {
  if (!is.na(CAMP$Bff6[i]) && is.na(CAMP$GHff6[i]) && !is.na(CAMP$LRff6[i])) {
    CAMP$GHff6[i] <- 0
  }

  if (!is.na(CAMP$Bff7[i]) && is.na(CAMP$GHff7[i]) && !is.na(CAMP$LRff7[i])) {
    CAMP$GHff7[i] <- 0
  }

  if (!is.na(CAMP$Bff8[i]) && is.na(CAMP$GHff8[i]) && !is.na(CAMP$LRff8[i])) {
    CAMP$GHff8[i] <- 0
  }

  if (!is.na(CAMP$Bff10[i]) && is.na(CAMP$GHff10[i]) && !is.na(CAMP$LRff10[i])) {
    CAMP$GHff10[i] <- 0
  }
}

### Issue 7. ###
# problem: value of the 'GHff' and 'LRff' columns in a given month have a raw numeric count while
  # the value of the corresponding 'Bff' column in said month has a value equal to NA is erroneous
  # because the species allocation of the population survey total is all or nothing
# resolution: reassign the 'Bff' column value under this circumstance with 0


for (i in 1:nrow(CAMP)) {
  if (is.na(CAMP$Bff6[i]) && !is.na(CAMP$GHff6[i]) && !is.na(CAMP$LRff6[i])) {
    CAMP$Bff6[i] <- 0
  }

  if (is.na(CAMP$Bff7[i]) && !is.na(CAMP$GHff7[i]) && !is.na(CAMP$LRff7[i])) {
    CAMP$Bff7[i] <- 0
  }

  if (is.na(CAMP$Bff8[i]) && !is.na(CAMP$GHff8[i]) && !is.na(CAMP$LRff8[i])) {
    CAMP$Bff8[i] <- 0
  }

  if (is.na(CAMP$Bff10[i]) && !is.na(CAMP$GHff10[i]) && !is.na(CAMP$LRff10[i])) {
    CAMP$Bff10[i] <- 0
  }
}

### Issue 8. ###
# problem: value of the 'Tot' column in a given month does not equal the sum of the species
  # population count columns in said month and is inconsistent with the rest of the dataset
# resolution: 'Tot' column values in a given month equal to 99 when the value of all three
  # corresponding species population count columns for said month are equal to NA should be
  # reassigned with NA

for (i in 1:nrow(CAMP)) {
  if (!is.na(CAMP$Tot6[i])) {
    if (is.na(CAMP$Bff6[i]) && is.na(CAMP$GHff6[i]) && is.na(CAMP$LRff6[i])) {
      if (CAMP$Tot6[i] == "99") {
        CAMP$Tot6[i] <- NA
      }
    }
  }

  if (!is.na(CAMP$Tot7[i])) {
    if (is.na(CAMP$Bff7[i]) && is.na(CAMP$GHff7[i]) && is.na(CAMP$LRff7[i])) {
      if (CAMP$Tot7[i] == "99") {
        CAMP$Tot7[i] <- NA

      }
    }
  }

  if (!is.na(CAMP$Tot8[i])) {
    if (is.na(CAMP$Bff8[i]) && is.na(CAMP$GHff8[i]) && is.na(CAMP$LRff8[i])) {
      if (CAMP$Tot8[i] == "99") {
        CAMP$Tot8[i] <- NA
      }
    }
  }

  if (!is.na(CAMP$Tot10[i])) {
    if (is.na(CAMP$Bff10[i]) && is.na(CAMP$GHff10[i]) && is.na(CAMP$LRff10[i])) {
      if (CAMP$Tot10[i] == "99") {
        CAMP$Tot10[i] <- NA
      }
    }
  }
}

###################################################################################################
# Assert Numeric Data Type of the Population Counts for Each Species in Each Month
###################################################################################################

# assert conversion of June population counts to numeric
CAMP$Bff6 <- as.numeric(CAMP$Bff6)
CAMP$GHff6 <- as.numeric(CAMP$GHff6)
CAMP$LRff6 <- as.numeric(CAMP$LRff6)
CAMP$Tot6 <- as.numeric(CAMP$Tot6)

# assert conversion of July population counts to numeric
CAMP$Bff7 <- as.numeric(CAMP$Bff7)
CAMP$GHff7 <- as.numeric(CAMP$GHff7)
CAMP$LRff7 <- as.numeric(CAMP$LRff7)
CAMP$Tot7 <- as.numeric(CAMP$Tot7)

# assert conversion of August population counts to numeric
CAMP$Bff8 <- as.numeric(CAMP$Bff8)
CAMP$GHff8 <- as.numeric(CAMP$GHff8)
CAMP$LRff8 <- as.numeric(CAMP$LRff8)
CAMP$Tot8 <- as.numeric(CAMP$Tot8)

# assert conversion of October population counts to numeric
CAMP$Bff10 <- as.numeric(CAMP$Bff10)
CAMP$GHff10 <- as.numeric(CAMP$GHff10)
CAMP$LRff10 <- as.numeric(CAMP$LRff10)
CAMP$Tot10 <- as.numeric(CAMP$Tot10)

###################################################################################################
# Append Columns Describing the Establishment History of the Camp Dataset
###################################################################################################

# storage for years that experienced a food shortage
shortYears <- c(1998, 2003, 2007, 2010)

# loop through each camp
for (i in 1:nrow(CAMP)) {
  # catch the value describing the establishment history of the camp in focus
  estab <- CAMP$Hist[i]

  # for each level of the establishment history, assign/append values appropriately describing
  #   the following variables:
  #
  # Post_98: 1 (true), 0 (false), NA (unknown) - camp was established in 1998 or later
  # Post_03: 1 (true), 0 (false), NA (unknown) - camp was established in 2003 or later
  # Hist_Simp: 'Pre-2007', '2007-2009', '2010', unknown (NA) - classification of the year the camp
  #   was established
  # Short_07_10: 1 (true), 0 (false), NA (unknown) - camp was established in 2007 or 2010
  # Short_Any: 1 (true), 0 (false), NA (unknown) - camp was established in any of the years
    # which experienced a food shortage (1998, 2003, 2007, 2010)

  if (estab == "1998") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 0
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 1
  }  else if (estab == "2001") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 0
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 0
  } else if (estab == "2002") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 0
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 0
  } else if (estab == "2003") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 1
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 1
  } else if (estab == "2007") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 1
    CAMP$Hist_Simp[i] <- "2007-2009"
    CAMP$Short_07_10[i] <- 1
    CAMP$Short_Any[i] <- 1
  } else if (estab == "2008") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 1
    CAMP$Hist_Simp[i] <- "2007-2009"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 0
  } else if (estab == "2009") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 1
    CAMP$Hist_Simp[i] <- "2007-2009"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- 0
  } else if (estab == "2010") {
    CAMP$Post_98[i] <- 1
    CAMP$Post_03[i] <- 1
    CAMP$Hist_Simp[i] <- "2010"
    CAMP$Short_07_10[i] <- 1
    CAMP$Short_Any[i] <- 1
  } else if (estab == "uk") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- NA
    CAMP$Short_07_10[i] <- NA
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2002") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- 0
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2003") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- 0
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2004") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2005") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2006") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- "Pre-2007"
    CAMP$Short_07_10[i] <- 0
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2008") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- NA
    CAMP$Short_07_10[i] <- NA
    CAMP$Short_Any[i] <- NA
  } else if (estab == "uk pre 2009") {
    CAMP$Post_98[i] <- NA
    CAMP$Post_03[i] <- NA
    CAMP$Hist_Simp[i] <- NA
    CAMP$Short_07_10[i] <- NA
    CAMP$Short_Any[i] <- NA
  }
}

###################################################################################################
# Appends Columns Describing the Presence/Absence of Flying Foxes in Each Month to the Camp Dataset
###################################################################################################

# purpose: determine if flying foxes of any species were observed at each camp in a given month
# input: vector of total flying fox population counts across all camps for a given month
# output: vector of boolean values 1 (true), 0 (false), NA (unknown)
monthPresAbs <- function(tot) {
  output <- vector()

  for (i in 1:length(tot)) {
    if (!is.na(tot[i])) {
      if (tot[i] > 0) {
        output <- c(output, 1)
      } else {
        output <- c(output, 0)
      }
    } else {
      output <- c(output, NA)
    }
  }

  return(output)
}

# append flying fox presence/absence column for each month
CAMP$Jun_Pres <- monthPresAbs(CAMP$Tot6)
CAMP$Jul_Pres <- monthPresAbs(CAMP$Tot7)
CAMP$Aug_Pres <- monthPresAbs(CAMP$Tot8)
CAMP$Oct_Pres <- monthPresAbs(CAMP$Tot10)

# update flying fox presence/absence columns to reflect the rows with population counts equal to
  # the string "PRESENT" (since changed in main dataframe to NA values)
CAMP$Jun_Pres[which(CAMP$Id == campWithPres$Id[1])] <- 1
CAMP$Jul_Pres[which(CAMP$Id == campWithPres$Id[1])] <- 1
CAMP$Aug_Pres[which(CAMP$Id == campWithPres$Id[1])] <- 1
CAMP$Oct_Pres[which(CAMP$Id == campWithPres$Id[1])] <- 1

CAMP$Jun_Pres[which(CAMP$Id == campWithPres$Id[2])] <- 1
CAMP$Oct_Pres[which(CAMP$Id == campWithPres$Id[2])] <- 1

###################################################################################################
# Save Cleaned Camp Dataset
###################################################################################################

# reorganize camp dataframe
CAMP <- data.frame(
  Id=CAMP$Id,
  Roost=CAMP$Roost,
  Coords=CAMP$Coords,
  Lat=CAMP$Lat,
  Long=CAMP$Long,
  State=CAMP$State,
  Cont=CAMP$Cont,
  Cont_Bff=CAMP$Cont_Bff,
  Status=CAMP$Status,
  Hist=CAMP$Hist,
  Hist_Simp=CAMP$Hist_Simp,
  Post_98=CAMP$Post_98,
  Post_03=CAMP$Post_03,
  Post_07=CAMP$Post_07,
  Post_10=CAMP$Post_10,
  Short_07_10=CAMP$Short_07_10,
  Short_Any=CAMP$Short_Any,
  Bff6=CAMP$Bff6,
  GHff6=CAMP$GHff6,
  LRff6=CAMP$LRff6,
  Tot6=CAMP$Tot6,
  Bff7=CAMP$Bff7,
  GHff7=CAMP$GHff7,
  LRff7=CAMP$LRff7,
  Tot7=CAMP$Tot7,
  Bff8=CAMP$Bff8,
  GHff8=CAMP$GHff8,
  LRff8=CAMP$LRff8,
  Tot8=CAMP$Tot8,
  Bff10=CAMP$Bff10,
  GHff10=CAMP$GHff10,
  LRff10=CAMP$LRff10,
  Tot10=CAMP$Tot10,
  Fa_Size=CAMP$Fa_Size,
  Dist_Urb=CAMP$Dist_Urb,
  Prop_Mod=CAMP$Prop_Mod,
  Prop_Urb=CAMP$Prop_Urb,
  Prop_Veg=CAMP$Prop_Veg,
  Jun_Pres=CAMP$Jun_Pres,
  Jul_Pres=CAMP$Jul_Pres,
  Aug_Pres=CAMP$Aug_Pres,
  Oct_Pres=CAMP$Oct_Pres,
  stringsAsFactors=F
)

# save results
write.csv(CAMP, "../Data/Clean/Camp/Camp_Clean.csv", row.names=F)

###################################################################################################
# Initialize Spatial Objects
###################################################################################################

# load Australia shapefile
ausLayer <- readOGR(dsn="../Data/Source/Australia", layer="STE11aAust")

# create spatial subsets for each state
qldLayer <- ausLayer[ausLayer$STATE_NAME == "Queensland",]
nswLayer <- ausLayer[ausLayer$STATE_NAME == "New South Wales",]

# store a merge of the spatial state objects for plotting purposes
eastLayer <- rbind(qldLayer, nswLayer)

# initialize projection definitions for spatial objects
longlat <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
utm56 <- CRS("+proj=utm +zone=56 ellps=WGS84")

###################################################################################################
# Clean the Spillover Events Dataset
###################################################################################################

# load source data
IP <- read.csv("../Data/Source/Spillover/IP_Download.csv", stringsAsFactors=F)

# trim invalid columns
IP <- IP[,1:5]

# rename columns
colnames(IP) <- c("Name", "Event", "Long", "Lat", "Month")

# loop through each spillover event
for (i in 1:nrow(IP)) {
  # create spatial point object of spillover event in focus
  ip <- SpatialPoints(data.frame(Long=IP$Long[i], Lat=IP$Lat[i]), proj4string=longlat)
  # append the name of the state the spillover event in focus was located
  IP$State[i] <- as.character(over(ip, eastLayer)$STATE_NAME)
}

# append dummy column used later for merging with horse property data
IP$Id <- 0
# append unique identifier composed of latitude and longitude coordinate pairs
IP$Coords <- paste(IP$Lat, IP$Long, sep=", ")

# reorganize dataframe
IP <- data.frame(
  Id=IP$Id,
  Name=IP$Name,
  Coords=IP$Coords,
  Lat=IP$Lat,
  Long=IP$Long,
  State=IP$State,
  Month=IP$Month,
  Event=IP$Event,
  stringsAsFactors=T
)

# save results
write.csv(IP, "../Data/Clean/Spillover/IP_Clean.csv", row.names=F)

###################################################################################################
# Create Spatial Object/Layer for the Study Area
###################################################################################################

# create spatial object of spillover events
IP_sp <- SpatialPointsDataFrame(data.frame(Long=IP$Long, Lat=IP$Lat), data=IP, proj4string=longlat)
IP_sp <- spTransform(IP_sp, utm56)

# store 100km buffers drawn around each spillover event
IP_buffer_sp <- gBuffer(IP_sp, width=100000, quadsegs=100)

# change projections to longlat
IP_sp <- spTransform(IP_sp, longlat)
IP_buffer_sp <- spTransform(IP_buffer_sp, longlat)

# ititialize storage for the coordinates that define each of the 100km buffers
IP_buffer_coords <- data.frame()
# loop through each 100km buffer polygon
for (i in 1:length(IP_buffer_sp@polygons[[1]]@Polygons)) {
  # catch polygon in focus
  poly <- IP_buffer_sp@polygons[[1]]@Polygons[[i]]@coords

  # loop through the coordinate matrix that defines the polygon in focus
  for (j in 1:dim(poly)[1]) {
    # catch latitude and longitude coodinates
    x <- poly[j,1]
    y <- poly[j,2]

    # append coordate pairs to the storage object
    IP_buffer_coords <- rbind(IP_buffer_coords, data.frame(x, y))
  }
}

# rename columns
colnames(IP_buffer_coords) <- c("Long", "Lat")

# draw a minimum bounding box around the 100km buffers
studyArea <- convHull(IP_buffer_coords)

# create spatial object of the study area
studyAreaLayer <- SpatialPolygons(studyArea@polygons@polygons, proj4string=longlat)
studyAreaLayer <- SpatialPolygonsDataFrame(studyAreaLayer, data.frame(ID=1))

# check the accuracy of the study area spatial object
plot(eastLayer)
plot(studyAreaLayer, col="gray", add=T)

# save results - NOTE: failure + error message will be thrown if the shapefile already exists in the
  # target directory
#writeOGR(obj=studyAreaLayer, dsn="../Data/Clean/Study Area", layer="StudyArea",
#         driver="ESRI Shapefile")

###################################################################################################
# Clean Horse Property Datasets for both Queensland and New South Wales
###################################################################################################

# load source data for both states
HORSE_qld <- readOGR(dsn="../Data/Source/Horse", layer="locsite_horsecount")
HORSE_nsw <- readOGR(dsn="../Data/Source/Horse", layer="NSW_horse_general")

# change NSW projection to longlat
HORSE_nsw <- spTransform(HORSE_nsw, longlat)

# convert QLD spatial object from polygons to points by using the geometric center of each polygon
HORSE_qld_p <- gCentroid(HORSE_qld, byid=T)
HORSE_qld <- SpatialPointsDataFrame(HORSE_qld_p@coords, data=HORSE_qld@data, proj4string=longlat)

# store the dataframes behind the spatial objects of both states
HORSE_qldDF <- HORSE_qld@data
HORSE_nswDF <- HORSE_nsw@data

# loop through each of the coordinate pairs defining the QLD spatial object
for (i in 1:dim(HORSE_qld@coords)[1]) {
  # append geographic coordinates of the property in focus to the QLD dataframe
  HORSE_qldDF$Lat[i] <- HORSE_qld@coords[i,2]
  HORSE_qldDF$Long[i] <- HORSE_qld@coords[i,1]
  # append unique identifier composed of latitude and longitude coordinate pairs
  HORSE_qldDF$Coords[i] <- paste(HORSE_qld@coords[i,2], HORSE_qld@coords[i,1], sep=", ")
}

# loop through each of the coordinate pairs defining the NSW spatial object
for (i in 1:dim(HORSE_nsw@coords)[1]) {
  # append geographic coordinates of the property in focus to the NSW dataframe
  HORSE_nswDF$Lat[i] <- HORSE_nsw@coords[i,2]
  HORSE_nswDF$Long[i] <- HORSE_nsw@coords[i,1]
  # append unique identifier composed of latitude and longitude coordinate pairs
  HORSE_nswDF$Coords[i] <- paste(HORSE_nsw@coords[i,2], HORSE_nsw@coords[i,1], sep=", ")
}

###################################################################################################
# Remove Duplicates from Horse Property Datasets for both Queensland and New South Wales
###################################################################################################

# purpose: remove duplicated horse property entries based on geographic coordinate pairs
# input: a dataframe of horse property data
# output: a copy of the passed dataframe with all duplicated entries removed
removeDupHp <- function(df) {
  df$Id <- seq(1, nrow(df), 1)
  dups <- duplicated(df$Coords)
  remove <- vector()

  for (i in 1:length(dups)) {
    if (dups[i]) {
      remove <- c(remove, i)
    }
  }

  return(df[!(df$Id %in% remove),])
}

# remove duplicated entries based on geographic coordinate pairs (value of the 'Coords' column)
HORSE_qld <- removeDupHp(HORSE_qldDF)
HORSE_nsw <- removeDupHp(HORSE_nswDF)

###################################################################################################
# Save Cleaned Horse Property Datasets
###################################################################################################

# append unique numeric identifiers
HORSE_qld$Id <- seq(1, nrow(HORSE_qld), 1)
HORSE_nsw$Id <- seq(1, nrow(HORSE_nsw), 1)

# reorganize QLD dataframe
HORSE_qld <- data.frame(
  Id=HORSE_qld$Id,
  Name="RP",
  Coords=HORSE_qld$Coords,
  Lat=HORSE_qld$Lat,
  Long=HORSE_qld$Long,
  State="Queensland",
  Month=NA,
  Event=0,
  stringsAsFactors=F
)

# reorganize NSW dataframe
HORSE_nsw <- data.frame(
  Id=HORSE_nsw$Id,
  Name="RP",
  Coords=HORSE_nsw$Coords,
  Lat=HORSE_nsw$Lat,
  Long=HORSE_nsw$Long,
  State="New South Wales",
  Month=NA,
  Event=0,
  stringsAsFactors=F
)

# save results
write.csv(HORSE_qld, "../Data/Clean/Horse/Horse_QLD_Clean.csv", row.names=F)
write.csv(HORSE_nsw, "../Data/Clean/Horse/Horse_NSW_Clean.csv", row.names=F)

###################################################################################################
# Build and Save the Merge of both Queensland and New South Wales Horse Property Datasets
###################################################################################################

# merge QLD and NSW datasets
HORSE <- rbind(HORSE_qld, HORSE_nsw)
# append unique numeric identifier
HORSE$Id <- seq(1, nrow(HORSE), 1)

# save results
write.csv(HORSE, "../Data/Clean/Horse/Horse_Merged_Clean.csv", row.names=F)
