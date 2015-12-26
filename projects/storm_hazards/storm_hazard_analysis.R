#### Storm hazard data analysis


### Libraries
library(lattice)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringdist)


### Retrieve data and documentation file

setwd("~/git/coursera_reproducible_research/projects/storm_hazards")

if(!file.exists("stormdata.csv.bz2")){
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2", method="curl")
}
df.stormdata <- read.csv("stormdata.csv.bz2")

df.eventtypes <- read.csv("event_types.txt")



### Data cleaning & selection strategy

# Convert BGN_DATE to POSIXct type
df.stormdata$BGN_DATE <- mdy_hms(df.stormdata$BGN_DATE)

# Select relevant fields: BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP
df.stormdata.subset.alltime <- subset(df.stormdata, select=c(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))


## Check data quality

summary(df.stormdata.subset.alltime)

# Check BGN_DATE
hist(year(df.stormdata.subset.alltime$BGN_DATE))

# Check EVTYPE categories matching official categories
sort(unique(df.stormdata.subset.alltime$EVTYPE))
length(unique(df.stormdata.subset.alltime$EVTYPE))
# => Many more categories (985) than expected because of non-consistent spelling

# Check NAs
sum(is.na(df.stormdata.subset.alltime))
# => No NA values

# Check PROPDMGEXP and CROPDMGEXP consistent values

sort(unique(df.stormdata.subset.alltime$PROPDMGEXP))
df.stormdata.subset.alltime[df.stormdata.subset.alltime$PROPDMGEXP %in% c("-", "?", "+", "h", "H", "0", "1", "2"),]
# => Strange values for PROPDMGEXP, all between 1993 and 1995
sort(unique(df.stormdata.subset.alltime$CROPDMGEXP))
df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("?", "0", "2"),]
dim(df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("?", "0", "2"),])
# => 27 strange values for CROPDMGEXP, all between 1993 and 1995

# Check for potential outliers
df.stormdata.subset.alltime[df.stormdata.subset.alltime$PROPDMGEXP %in% c("B"),]
# => Is this entry correct?
# 605953   1/1/2006 0:00:00                      FLOOD          0        0  115.00          B   32.50          M
df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("B"),]


## Select data subset

# Select subset in specific date range (after 1995 because of unreliable data, before 2011 because 2011 data is incomplete)
#df.stormdata.subset <- subset(df.stormdata, BGN_DATE > ymd("1995-12-31") & BGN_DATE < ymd("2006-01-01"), select=c(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
df.stormdata.subset <- subset(df.stormdata, BGN_DATE > ymd("1995-12-31") & BGN_DATE < ymd("2011-01-01"), select=c(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

# summary(df.stormdata.subset)
# 
# # Check BGN_DATE
# hist(year(df.stormdata.subset$BGN_DATE))
# 
# # Check EVTYPE categories matching official categories
# sort(unique(df.stormdata.subset$EVTYPE))
# length(unique(df.stormdata.subset$EVTYPE))
# 
# # Check NAs
# sum(is.na(df.stormdata.subset))
# # => No NA values
# 
# # Check PROPDMGEXP and CROPDMGEXP consistent values and potential outliers
# sort(unique(df.stormdata.subset$PROPDMGEXP))
# df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("-", "?", "+", "h", "H", "0", "1", "2"),]
# summary(df.stormdata.subset[df.stormdata.subset$PROPDMGEXP=="", "PROPDMG"])
# df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("B"),]
# sort(unique(df.stormdata.subset$CROPDMGEXP))
# df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),]
# dim(df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),])
# summary(df.stormdata.subset[df.stormdata.subset$CROPDMGEXP=="", "CROPDMG"])
# df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("B"),]

# Replace PROPDMG * 10^PROPDMGEXP by PROPERTY_DAMAGE and CROPDMG* 10^CROPDMGEXP by CROP_DAMAGE variables
multiplier_mapping <- c("0" = 1, "1" = 10, "2" = 100, "3" = 1000, "4" = 10000, "5" = 100000, "6" = 1000000, "7" = 10000000, "8" = 100000000, "H" = 100, "h" = 100, "K" = 1000, "k" = 1000, "M" = 1000000, "m" = 1000000, "B" =1000000000, "b" = 1000000000)
df.stormdata.subset$PROPERTY_DAMAGE <- df.stormdata.subset$PROPDMG * multiplier_mapping[droplevels(df.stormdata.subset$PROPDMGEXP)]
df.stormdata.subset$CROP_DAMAGE <- df.stormdata.subset$CROPDMG * multiplier_mapping[droplevels(df.stormdata.subset$CROPDMGEXP)]
df.stormdata.subset <- subset(df.stormdata.subset, select=-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))


## Clean up EVTYPE levels

# Convert to lowercase to prevent upper/lowercase mismatch
df.stormdata.subset$EVTYPE <- tolower(df.stormdata.subset$EVTYPE)
# Manual rules for EVTYPEs with many injuries
df.stormdata.subset$EVTYPE <- gsub("tstm", "thunderstorm", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^wild/forest fire$", "wildfire", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^thunderstorm wind/hail$", "hail", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^winter weather/mix$", "winter weather", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^extreme cold$", "extreme cold/wind chill", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^snow$", "heavy snow", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^wind$", "strong wind", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^storm surge$", "storm surge/tide", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^landslide$", "debris flow", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^heavy surf/high surf$", "high surf", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^extreme windchill$", "extreme cold/wind chill", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^hurricane$", "hurricane (typhoon)", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("^record warmth$|^recored heat$", "excessive heat", df.stormdata.subset$EVTYPE)
df.stormdata.subset$EVTYPE <- gsub("wintry", "winter", df.stormdata.subset$EVTYPE)

# Try to match raw EVTYPE to correct EVENT_TYPE and add as new column to df.stormdata.subset
df.stormdata.subset$EVENT_TYPE <- df.eventtypes$EVENT_TYPE[amatch(tolower(df.stormdata.subset$EVTYPE), tolower(df.eventtypes$EVENT_TYPE), maxDist=3, nomatch=NA)]
# Use "OTHER" factor level for unmatched EVENT_TYPE
levels(df.stormdata.subset$EVENT_TYPE) <- c(levels(df.stormdata.subset$EVENT_TYPE), "OTHER")
df.stormdata.subset$EVENT_TYPE[is.na(df.stormdata.subset$EVENT_TYPE)] <- "OTHER"

# EVENT_TYPE "OTHER" is less than 2% of all entries
sum(df.stormdata.subset$EVENT_TYPE=="OTHER")/length(df.stormdata.subset$EVENT_TYPE)

# For unmatched EVENT_TYPE, list by raw EVTYPE and take top sorted by number of INJURIES
aggregate(INJURIES ~ EVTYPE, data=df.stormdata.subset[df.stormdata.subset$EVENT_TYPE=="OTHER",], FUN=length) %>% arrange(desc(INJURIES)) %>% head(10)


# Drop EVTYPE column
df.stormdata.subset <- subset(df.stormdata.subset, select=-c(EVTYPE))



### Exploratory Analysis

## 1. Across the United States, which types of events are most harmful with respect to population health ("FATALITIES", "INJURIES")?

df.fatalities <- aggregate(FATALITIES ~ EVENT_TYPE, data = df.stormdata.subset, sum)
df.injuries <- aggregate(INJURIES ~ EVENT_TYPE, data = df.stormdata.subset, sum)
df.fatalities.injuries <- merge(df.fatalities, df.injuries) %>% mutate(SUM.FATALATIES.INJURIES = FATALITIES + INJURIES)

# Top 10 event types by fatalities and injuries respectively
df.fatalities.injuries %>% arrange(desc(FATALITIES)) %>% head(10)
df.fatalities.injuries %>% arrange(desc(INJURIES)) %>% head(10)

# Plot fatalities and injuries in barplot for top categories
df.fatalities.injuries.top.sum <- df.fatalities.injuries %>% arrange(desc(SUM.FATALATIES.INJURIES)) %>% head(10)
barchart(FATALITIES + INJURIES ~ EVENT_TYPE, data=df.fatalities.injuries.top.sum, ylab="Number of people", main="Population health consequences per type")

# TODO: handle OTHER better in this graph!
# TODO: legend (ggplot?)
# TODO_optional: sort on highest damage

#ggplot(df.fatalities.injuries.top.sum, aes(x = EVENT_TYPE, y = FATALITIES)) + geom_bar(stat = "identity", position = "stack")


## 2. Across the United States, which types of events have the greatest economic consequences?

df.property.damage <- aggregate(PROPERTY_DAMAGE ~ EVENT_TYPE, data = df.stormdata.subset, sum)
df.crop.damage <- aggregate(CROP_DAMAGE ~ EVENT_TYPE, data = df.stormdata.subset, sum)
df.economic.damage <- merge(df.property.damage, df.crop.damage) %>% mutate(TOTAL.DAMAGE = PROPERTY_DAMAGE + CROP_DAMAGE)

# Plot damage in barplot for top categories
df.economic.damage.top.sum <- df.economic.damage %>% arrange(desc(TOTAL.DAMAGE)) %>% head(10)
barchart(PROPERTY_DAMAGE + CROP_DAMAGE ~ EVENT_TYPE, data=df.economic.damage.top.sum, ylab="Damage", main="Economic consequences per type")


