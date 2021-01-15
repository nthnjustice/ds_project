###################################################################################################
# Set Working Environment, Import Dependencies, and Load Data
###################################################################################################

setwd("C:/Users/Nathan/Desktop/Project 1/Code")

library(stargazer)
library(MASS)

# load camp dataset
df <- read.csv("../Data/Final/Camp/Camp_Final.csv")

# store the covariates to be modelled
covars <- c("Cont", "Cont_Bff", "Hist_Simp", "Post_07", "Post_10", "Short_07_10", "Short_Any",
            "Fa_Size", "Dist_Urb", "Prop_Mod", "Prop_Urb", "Prop_Veg", "Prop_In_20km",
            "Prop_Near_Dist")

# ititialize the HTML string for the model results
starTable <- ""

# purpose: runs univariate model of passed covariate and builds HTML table of results
# input: the current HTML string of model results, covariate to be modelled, title of table results,
  # label for dependant variable, label for covariate
# output: HTML string of model results
buildStarTable <- function(starTable, covar, title, dep.var.labels, covariate.labels) {
  # ititialize return object
  output <- ""

  # run model
  model <- glm(as.formula(paste("Event ~", covar, sep=" ")), family="binomial", df)

  # build HTML string of results
  gaze <- stargazer(model, type="html", ci=T, title=title, dep.var.labels=dep.var.labels,
                    covariate.labels=covariate.labels, apply.coef=exp)
  starTable <- paste(starTable, gaze, sep="")

  for (i in 1:length(gaze)) {
    output <- paste(output, gaze[i], sep="")
  }

  # append spacing for readability
  output <- paste(output, '<div style="height: 50px;"></div>')

  # return HTML table of model results
  return(output)
}

###################################################################################################
# Model and Table for Cont
###################################################################################################

covar <- "Cont"
title <- "Continuously Occupied"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Continuously Occupied")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Cont_Bff
###################################################################################################

covar <- "Cont_Bff"
title <- "Continuously Occupied by BFF"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Continuously Occupied by BFF")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Hist_Simp
###################################################################################################

covar <- "Hist_Simp"
title <- "History of Establishment"
dep.var.labels = c("Pre-2007, 2007-2009, 2010")
covariate.labels <- c("History")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Post_07
###################################################################################################

covar <- "Post_07"
title <- "Established 2007 or Later"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Formed 2007 or Later")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Post_10
###################################################################################################

covar <- "Post_10"
title <- "Established 2010 or Later"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Formed 2010 or Later")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Short_07_10
###################################################################################################

covar <- "Short_07_10"
title <- "Established During a Food Shortage (2007 or 2010)"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Formed in Food Shortage")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Short_Any
###################################################################################################

covar <- "Short_Any"
title <- "Established During a Food Shortage (1998, 2003, 2007, 2010)"
dep.var.labels = c("0 = false, 1 = true")
covariate.labels <- c("Formed in Food Shortage")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Fa_Size
###################################################################################################

covar <- "Fa_Size"
title <- "Feeding Area Size"
dep.var.labels = c("In Meters (?)")
covariate.labels <- c("Feeding Area Size")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Dist_Urb
###################################################################################################

covar <- "Dist_Urb"
title <- "Distance to Urban Area"
dep.var.labels = c("In Kilometers (?)")
covariate.labels <- c("Distance to Urban Area")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Prop_Mod
###################################################################################################

covar <- "Prop_Mod"
title <- "Proportion Modified"
dep.var.labels = c("In Percentage (?)")
covariate.labels <- c("Proportion Modified")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Prop_Urb
###################################################################################################

covar <- "Prop_Urb"
title <- "Proportion Urban"
dep.var.labels = c("In Percentage (?)")
covariate.labels <- c("Proportion Urban")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Prop_Veg
###################################################################################################

covar <- "Prop_Veg"
title <- "Proportion Vegitation"
dep.var.labels = c("In Percentage (?)")
covariate.labels <- c("Proportion Vegitation")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Prop_In_20km
###################################################################################################

covar <- "Prop_In_20km"
title <- "Horse Properties within 20 Kilometers"
dep.var.labels = c("Count")
covariate.labels <- c("Horse Properties in 20km")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Model and Table for Prop_Near_Dist
###################################################################################################

covar <- "Prop_Near_Dist"
title <- "Distance to the Nearest Horse Property"
dep.var.labels = c("In Meters")
covariate.labels <- c("Distance to Horse Property")
starTable <- paste(starTable,
                   buildStarTable(starTable, covar, title, dep.var.labels, covariate.labels),
                   sep="")

###################################################################################################
# Finish HTML File
###################################################################################################

# build full HTML file
starTable <- paste("<html><head></head><body>", starTable, "</body></html>", sep="")

# save results as HTML file
write(starTable, "../Results/GLM_results.html")
