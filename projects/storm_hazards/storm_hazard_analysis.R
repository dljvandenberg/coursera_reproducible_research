#### Storm hazard data analysis


### Libraries
library(lattice)
library(ggplot2)
library(dplyr)


### Retrieve and read data

if(!file.exists("stormdata.csv.bz2")){
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2", method="curl")
}
df.stormdata <- read.csv("stormdata.csv.bz2")


### Data cleaning & selection strategy

# Select relevant fields: BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP
df.stormdata.subset <- subset(df.stormdata, select=c(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

## Check data quality

summary(df.stormdata.subset)

# Check EVTYPE categories matching official categories
sort(unique(df.stormdata.subset$EVTYPE))
length(unique(df.stormdata.subset$EVTYPE))

# Check NAs
sum(is.na(df.stormdata.subset))

# Check PROPDMGEXP and CROPDMGEXP consistent values
sort(unique(df.stormdata.subset$PROPDMGEXP))
df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("-", "?", "+", "h", "H", "0", "1", "2"),]
# => Strange values for PROPDMGEXP only between 1993 and 1995

df.stormdata.subset[df.stormdata.subset$PROPDMGEXP %in% c("B"),]

sort(unique(df.stormdata.subset$CROPDMGEXP))
df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),]
dim(df.stormdata.subset[df.stormdata.subset$CROPDMGEXP %in% c("?", "0", "2"),])
# => 27 strange values for CROPDMGEXP, only between 1993 and 1995 => Remove these from dataset?


# 605953   1/1/2006 0:00:00                      FLOOD          0        0  115.00          B   32.50          M

# Convert BGN_DATE to POSIXct type




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
