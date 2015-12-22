### Storm hazard data analysis


## Libraries
library(lattice)
library(ggplot2)
library(dplyr)


## Retrieve and read data
if(!file.exists("stormdata.csv.bz2")){
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2", method="curl")
}
df.stormdata <- read.csv("stormdata.csv.bz2")


## Data cleaning & selection strategy

# TODO: use amatch() from stringdist package to match EVTYPE strings to table 2.1.1 entries
# in https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf


# TODO: convert 'PROPDMG' and 'PROPDMGEXP' to damage_property  Ex: 10.00K, 0.00K, 10.00M. Similarly, for CROPDMG
# See http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx

# field names: https://ire.org/media/uploads/files/datalibrary/samplefiles/Storm%20Events/layout08.doc, https://class.coursera.org/repdata-035/forum/thread?thread_id=51

# multiplier_table <- c("0" = 1, "K" = 1000, "M" = 1000000, "B" = 1000000000)
# data$damage * multiplier_table[data$multiplier]

# Only choose last decade?

# https://class.coursera.org/repdata-035/forum/thread?thread_id=35: In the interest of reproducible research you might want to programmatically extract the 48 officially recognised weather event types as defined in National Weather Service Instruction (NWS-I10-1605).



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
