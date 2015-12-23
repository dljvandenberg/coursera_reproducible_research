#### Storm hazard data analysis


### Libraries
library(lattice)
library(ggplot2)
library(dplyr)
library(lubridate)


### Retrieve and read data

if(!file.exists("stormdata.csv.bz2")){
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2", method="curl")
}
df.stormdata <- read.csv("stormdata.csv.bz2")


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

# Check PROPDMGEXP and CROPDMGEXP consistent values and potential outliers
sort(unique(df.stormdata.subset.alltime$PROPDMGEXP))
df.stormdata.subset.alltime[df.stormdata.subset.alltime$PROPDMGEXP %in% c("-", "?", "+", "h", "H", "0", "1", "2"),]
# => Strange values for PROPDMGEXP only between 1993 and 1995
df.stormdata.subset.alltime[df.stormdata.subset.alltime$PROPDMGEXP %in% c("B"),]
# => Is this entry correct?
# 605953   1/1/2006 0:00:00                      FLOOD          0        0  115.00          B   32.50          M
sort(unique(df.stormdata.subset.alltime$CROPDMGEXP))
df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("?", "0", "2"),]
dim(df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("?", "0", "2"),])
# => 27 strange values for CROPDMGEXP, only between 1993 and 1995 => Remove these from dataset?
df.stormdata.subset.alltime[df.stormdata.subset.alltime$CROPDMGEXP %in% c("B"),]

# Check empty strings in PROPDMGEXP and CROPDMGEXP


# Select subset in 10-year date range
df.stormdata.subset <- subset(df.stormdata, BGN_DATE > ymd("1995-12-31") & BGN_DATE < ymd("2006-01-01"), select=c(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

summary(df.stormdata.subset)

# Check BGN_DATE
hist(year(df.stormdata.subset$BGN_DATE))

# Check EVTYPE categories matching official categories
sort(unique(df.stormdata.subset$EVTYPE))
length(unique(df.stormdata.subset$EVTYPE))

# Check NAs
sum(is.na(df.stormdata.subset))
# => No NA values

# Check PROPDMGEXP and CROPDMGEXP consistent values and potential outliers
sort(unique(df.stormdata.subset$PROPDMGEXP))
df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("-", "?", "+", "h", "H", "0", "1", "2"),]
summary(df.stormdata.subset[df.stormdata.subset$PROPDMGEXP=="", "PROPDMG"])
df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("B"),]
sort(unique(df.stormdata.subset$CROPDMGEXP))
df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),]
dim(df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),])
summary(df.stormdata.subset[df.stormdata.subset$CROPDMGEXP=="", "CROPDMG"])
df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("B"),]

# Replace PROPDMG * 10^PROPDMGEXP by PROPERTY_DAMAGE and CROPDMG* 10^CROPDMGEXP by CROP_DAMAGE variables
multiplier_table <- c("0" = 1, "1" = 10, "2" = 100, "3" = 1000, "4" = 10000, "5" = 100000, "6" = 1000000, "7" = 10000000, "8" = 100000000, "H" = 100, "h" = 100, "K" = 1000, "k" = 1000, "M" = 1000000, "m" = 1000000, "B" =1000000000, "b" = 1000000000)
df.stormdata.subset$PROPERTY_DAMAGE <- df.stormdata.subset$PROPDMG * multiplier_table[droplevels(df.stormdata.subset$PROPDMGEXP)]
df.stormdata.subset$CROP_DAMAGE <- df.stormdata.subset$CROPDMG * multiplier_table[droplevels(df.stormdata.subset$CROPDMGEXP)]
df.stormdata.subset <- subset(df.stormdata.subset, select=-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))



## Exploratory Analysis

# 1. Across the United States, which types of events ("EVTYPE") are most harmful with respect to population health ("FATALITIES", "INJURIES")?

df.fatalities <- aggregate(FATALITIES ~ EVTYPE, data = df.stormdata, sum)
df.injuries <- aggregate(INJURIES ~ EVTYPE, data = df.stormdata, sum)
df.fatalities.injuries <- merge(df.fatalities, df.injuries) %>% mutate(SUM.FATALATIES.INJURIES = FATALITIES + INJURIES)

# Top 10 event types by fatalities and injuries respectively
df.fatalities.injuries %>% arrange(desc(FATALITIES)) %>% head(10)
df.fatalities.injuries %>% arrange(desc(INJURIES)) %>% head(10)

# Plot fatalities and injuries in barplot for top categories
df.fatalities.injuries.top.sum <- df.fatalities.injuries %>% arrange(desc(SUM.FATALATIES.INJURIES)) %>% head(10)
barchart(FATALITIES + INJURIES ~ EVTYPE, data=df.fatalities.injuries.top.sum, ylab="Number of people", main="Population health consequences per type")

#ggplot(df.fatalities.injuries.top.sum, aes(x = EVTYPE, y = FATALITIES)) + geom_bar(stat = "identity", position = "stack")


# 2. Across the United States, which types of events have the greatest economic consequences?

df.propdmg <- aggregate(PROPDMG ~ EVTYPE, data = df.stormdata, sum)
df.cropdmg <- aggregate(CROPDMG ~ EVTYPE, data = df.stormdata, sum)
df.propdmg.cropdmg <- merge(df.propdmg, df.cropdmg) %>% mutate(SUM.PROPDMG.CROPDMG = PROPDMG + CROPDMG)

# Plot damage in barplot for top categories
df.propdmg.cropdmg.top.sum <- df.propdmg.cropdmg %>% arrange(desc(SUM.PROPDMG.CROPDMG)) %>% head(10)
barchart(PROPDMG + CROPDMG ~ EVTYPE, data=df.propdmg.cropdmg.top.sum, ylab="Damage", main="Economic consequences per type")

# OPTIONAL: adjust for inflation?
