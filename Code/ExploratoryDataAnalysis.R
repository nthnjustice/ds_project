###################################################################################################
# Set Working Environment, Import Dependencies, and Load Data
###################################################################################################

setwd("C:/Users/Nathan/Desktop/Project 1/Code")

library(ggplot2)

source("helpers.R")

# load camp dataframe
CAMP <- read.csv("../Data/Final/Camp/Camp_Final.csv")

###################################################################################################
# Plot for Camp Distribution Between States
###################################################################################################

p1 <- ggplot(CAMP, aes(x=State, fill=State)) +
  theme_classic() +
  geom_bar(stat="count") +
  ggtitle("Camp Distribution Between States") +
  xlab("State") +
  ylab("Number of Camps") +
  scale_fill_discrete(guide=F)
p1


dev.copy(png, "../Figures/Camp/Camp Distribution Between States.png")
dev.off()

###################################################################################################
# Plot for Continuously Occupied
###################################################################################################

p1 <- ggplot(CAMP, aes(x=as.factor(Cont), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Continuously Occupied") +
  xlab("") +
  ylab("Number of Camps") +
  scale_fill_discrete(guide=F)
p1

p2 <- ggplot(CAMP, aes(x=as.factor(Cont), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  ylab("Percentage of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(guide=F)
p2

p3 <- ggplot(CAMP, aes(x=as.factor(Cont_Bff), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Continuously Occupied by BFF") +
  xlab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p3

p4 <- ggplot(CAMP, aes(x=as.factor(Cont_Bff), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p4

multiplot(p1, p2, p3, p4, cols=2)

dev.copy(png, "../Figures/Camp/Continuously Occupied.png")
dev.off()

###################################################################################################
# Plot for Establishment History (simplified)
###################################################################################################

p1 <- ggplot(CAMP, aes(x=Hist_Simp, fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Establishment History (simplified)") +
  xlab("") +
  ylab("Number of Camps") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p1

p2 <- ggplot(CAMP, aes(x=Hist_Simp, fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  ylab("Percentage of Camps") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p2

multiplot(p1, p2, cols=1)

dev.copy(png, "../Figures/Camp/Establishment History (simplified).png")
dev.off()

###################################################################################################
# Plot for Established 2007-or_later and 2010-_or_later
###################################################################################################

p1 <- ggplot(CAMP, aes(x=as.factor(Post_07), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Established 2007 or later") +
  xlab("") +
  ylab("Number of Camps") +
  scale_fill_discrete(guide=F)
p1

p2 <- ggplot(CAMP, aes(x=as.factor(Post_07), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  ylab("Percentage of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(guide=F)
p2

p3 <- ggplot(CAMP, aes(x=as.factor(Post_10), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Established 2010 or later") +
  xlab("") +
  ylab("Number of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p3

p4 <- ggplot(CAMP, aes(x=as.factor(Post_10), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  ylab("Percentage of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p4

multiplot(p1, p2, p3, p4, cols=2)

dev.copy(png, "../Figures/Camp/Established 2007-or_later and 2010-_or_later.png")
dev.off()

###################################################################################################
# Plot for Established 2007_2010
###################################################################################################

p1 <- ggplot(CAMP, aes(x=as.factor(Short_07_10), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_bar(stat="count") +
  ggtitle("Established 2007 or 2010") +
  xlab("") +
  ylab("Number of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p1

p2 <- ggplot(CAMP, aes(x=as.factor(Short_07_10), fill=as.factor(Event))) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_bar(stat="count", position="fill") +
  ggtitle("(in percentage)") +
  xlab("") +
  ylab("Percentage of Camps") +
  scale_x_discrete(labels=c("False", "True")) +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p2

multiplot(p1, p2, cols=1)

dev.copy(png, "../Figures/Camp/Established 2007_2010.png")
dev.off()

###################################################################################################
# Plot for Monthly Population Distribution
###################################################################################################

# purpose: create a transformed dataframe containing species population counts from wide-form to
  # long-form for plotting
# input: BFF population count, GHFF population count, LRFF population count, total population count,
  # value for the max value subset, Boolean for whether or not zero-counts should be included
# output: transformed dataframe of species population counts in long-form
fitCensusData <- function(bff, ghff, lrff, tot, tail=F, zero=T) {
  output <- data.frame(species=rep("BFF", length(bff)), count=bff)
  output <- rbind(output, data.frame(species=rep("GHFF", length(ghff)), count=ghff))
  output <- rbind(output, data.frame(species=rep("LRFF", length(lrff)), count=lrff))
  output <- rbind(output, data.frame(species=rep("Total", length(tot)), count=tot))

  if (!tail && !zero) {
    output <- subset(output, count != 0)
  } else if (tail && zero) {
    output <- subset(output, count <= tail)
  } else if (tail && !zero) {
    output <- subset(output, count <= tail)
    output <- subset(output, count != 0)
  }

  return(output)
}

df <- fitCensusData(CAMP$Bff6, CAMP$GHff6, CAMP$LRff6, CAMP$Tot6)

p1 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_histogram(show.legend=F, bins=20) +
  ggtitle("June Population Distribution") +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p1

df <- fitCensusData(CAMP$Bff7, CAMP$GHff7, CAMP$LRff7, CAMP$Tot7)

p2 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_histogram(show.legend=F, bins=20) +
  ggtitle("July Population Distribution") +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p2

df <- fitCensusData(CAMP$Bff8, CAMP$GHff8, CAMP$LRff8, CAMP$Tot8)

p3 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
  geom_histogram(show.legend=F, bins=20) +
  ggtitle("August Population Distribution") +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p3

df <- fitCensusData(CAMP$Bff10, CAMP$GHff10, CAMP$LRff10, CAMP$Tot10)

p4 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  geom_histogram(show.legend=F, bins=20) +
  ggtitle("October Population Distribution") +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p4

df <- fitCensusData(CAMP$Bff6, CAMP$GHff6, CAMP$LRff6, CAMP$Tot6, tail=1000, zero=F)

p5 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
  geom_histogram(bins=20) +
  ggtitle("(1 <= count => 1000)") +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p5

df <- fitCensusData(CAMP$Bff7, CAMP$GHff7, CAMP$LRff7, CAMP$Tot7, tail=1000, zero=F)

p6 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
  geom_histogram(bins=20) +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p6

df <- fitCensusData(CAMP$Bff8, CAMP$GHff8, CAMP$LRff8, CAMP$Tot8, tail=1000, zero=F)

p7 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
  geom_histogram(bins=20) +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p7

df <- fitCensusData(CAMP$Bff10, CAMP$GHff10, CAMP$LRff10, CAMP$Tot10, tail=1000, zero=F)

p8 <- ggplot(df, aes(count, fill=species)) +
  theme_classic() +
  theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
  geom_histogram(bins=20) +
  xlab("Population Count") +
  ylab("Frequency") +
  scale_fill_manual("Species", limits=c("BFF", "GHFF", "LRFF", "Total"),
                    values=c("deepskyblue4", "green4", "khaki4", "maroon"))
p8

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=2)

dev.copy(png, "../Figures/Camp/Monthly Population Distribution.png")
dev.off()

###################################################################################################
# Plot for Landuse
###################################################################################################

p1 <- ggplot(CAMP, aes(Fa_Size, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Feeding Area Size") +
  xlab("Unit of Measurement Unknown (Meters)") +
  ylab("Frequency") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p1

p2 <- ggplot(CAMP, aes(Dist_Urb, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Distance to Urban Area") +
  xlab("Unit of Measurement Unknown (Kilometers?)") +
  ylab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p2

p3 <- ggplot(CAMP, aes(Prop_Mod, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Proportion of Feeding Area is Modified") +
  xlab("Percentage") +
  ylab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p3

p4 <- ggplot(CAMP, aes(Prop_Urb, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Proportion of Feeding Area is Urban") +
  xlab("Percentage") +
  ylab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p4

p5 <- ggplot(CAMP, aes(Prop_Veg, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Proportion of Feeding Area is Vegitative") +
  xlab("Percentage") +
  ylab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p5

multiplot(p1, p2, p3, p4, p5, cols=1)

dev.copy(png, "../Figures/Camp/Landuse.png")
dev.off()

###################################################################################################
# Plot for Horse Properties
###################################################################################################

p1 <- ggplot(CAMP, aes(Prop_Num_20km, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Number of Horse Properties Within 20 Kilometers") +
  xlab("Count") +
  ylab("Frequency") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p1

p2 <- ggplot(CAMP, aes(Prop_Near_Dist, fill=as.factor(Event))) +
  theme_classic() +
  geom_histogram(bins=30) +
  ggtitle("Distance to Nearest Horse Property") +
  xlab("Meters") +
  ylab("") +
  scale_fill_discrete(aes(title="Event Camp"), labels=c("No", "Yes"))
p2

multiplot(p1, p2, cols=1)

dev.copy(png, "../Figures/Camp/Horse Properties.png")
dev.off()
