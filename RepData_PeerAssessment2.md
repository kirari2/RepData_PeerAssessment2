---
title: "Peer-graded Assignment: Course Project 2"
output: 
    html_document:
        keep_md: true
date: '2023-01-27'
---



## Title
Assessing the Most Severe Weather Events in Terms of Population Health and Economic Damage: Studying the US NOAA Storm Data, 1950–2011

## Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities.

This assignment explored the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, which tracks characteristics of major storms and weather events in the United States from 1950 to Nov 2011, including estimates of any fatalities, injuries, and property damage.

The aim is to find out which types of weather events caused are most harmful with respect to population health, and which types of events have the greatest economic consequences.

## Data Processing
The data for this assignment come from the course web site: [Storm data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

1. Download raw data

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "./df.bz2", method = "curl")
```

2. Read raw data

```r
library(data.table)
df <- fread("./df.bz2")
```

3. Explore the raw data set

```r
# Features of the whole data set
str(df)
```

```
## Classes 'data.table' and 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
# Number of weather events included
length(unique(df$EVTYPE))
```

```
## [1] 985
```

```r
# Statistics of weather-related fatalities
summary(df$FATALITIES)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##   0.0000   0.0000   0.0000   0.0168   0.0000 583.0000
```

```r
# Statistics of weather-related injuries
summary(df$INJURIES)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##    0.0000    0.0000    0.0000    0.1557    0.0000 1700.0000
```

4. Address question 1: Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(patchwork)

# Data extraction and rank the top 5 most fatal weather events
p1 <- df %>% 
        group_by(EVTYPE) %>% 
        summarise(total_f = sum(FATALITIES)) %>% 
        arrange(desc(total_f)) %>% 
        top_n(5) %>% 
# Plot and save bar plot
        ggplot() +
                geom_bar(aes(reorder(EVTYPE, total_f), total_f), 
                         stat = "identity") +
                ylim(0, 6000) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x = "",
                     y = "Count",
                     title = "Top 5 Causes of Weather\nFatalities in US, 1950–Nov 2011")
```

```
## Selecting by total_f
```

```r
# Data extraction and rank the top 5 weather events causing most injuries
p2 <- df %>% 
        group_by(EVTYPE) %>% 
        summarise(total_i = sum(INJURIES)) %>% 
        arrange(desc(total_i)) %>% 
        top_n(5) %>% 
# Plot and save bar plot
        ggplot() +
            geom_bar(aes(reorder(EVTYPE, total_i), total_i), 
                     stat = "identity") +
            ylim(0, 100000) +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(x = "",
                 y = "",
                 title = "Top 5 Causes of Weather\nInjuries in US, 1950–Nov 2011")
```

```
## Selecting by total_i
```

4. Address question 2: Across the United States, which types of events have the greatest economic consequences?

```r
# Features of property damage data
summary(df$PROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   12.06    0.50 5000.00
```

```r
unique(df$PROPDMGEXP)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-" "1" "8"
```

```r
# Clone an identical data set
df2 <- df
# Replace empty values
df2$PROPDMGEXP[df2$PROPDMGEXP == ""] <- "+"
# Transform property damage costs to the same unit
df2 <- df2 %>% 
    mutate(prop_dmg_exp = recode(PROPDMGEXP,
                                 K = 1e+03,
                                 M = 1e+06,
                                 B = 1e+09,
                                 m = 1e+06,
                                 `+` = 0,
                                 `0` = 1,
                                 `5` = 1e+05,
                                 `6` = 1e+06,
                                 `?` = 0, 
                                 `4` = 1e+04,
                                 `2` = 1e+02,
                                 `3` = 1e+03, 
                                 h = 1e+02,
                                 `7` = 1e+07,
                                 H = 1e+02,
                                 `-` = 0,
                                 `1` = 1e+01,
                                 `8` = 1e+08))
# Calculate total property damage
df2$prop_dmg_norm <- df2$PROPDMG * df2$prop_dmg_exp

# Data extraction and rank top 5 causes of property damage
p3 <- df2 %>% 
    group_by(EVTYPE) %>% 
    summarise(total_p = sum(prop_dmg_norm)) %>% 
    arrange(desc(total_p)) %>% 
    top_n(5) %>% 
# Plot and save the bar plot
    ggplot() +
        geom_bar(aes(reorder(EVTYPE, total_p), total_p), 
                 stat = "identity") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "",
             y = "Damage (US dollars)",
             title = "Weather-related\nProperty Damage\nin US, 1950\n–Nov 2011")
```

```
## Selecting by total_p
```

```r
# Features of crop damage data
summary(df2$CROPDMG)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.000   0.000   1.527   0.000 990.000
```

```r
unique(df2$CROPDMGEXP)
```

```
## [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
```

```r
# Replace empty values
df2$CROPDMGEXP[df2$CROPDMGEXP == ""] <- "?"
# Transform crop damage to the same unit
df2 <- df2 %>% 
    mutate(crop_dmg_exp = recode(CROPDMGEXP,
                                 M = 1e+06,
                                 K = 1e+03,
                                 m = 1e+06,
                                 B = 1e+09,
                                 `?` = 0,
                                 `0` = 1,
                                 k = 1e+03,
                                 `2` = 1e+02))
# Calculate total crop damage
df2$crop_dmg_norm <- df2$CROPDMG * df2$crop_dmg_exp 

# Data extraction for top 5 causes of crop damage
p4 <- df2 %>% 
    group_by(EVTYPE) %>% 
    summarise(total_c = sum(crop_dmg_norm)) %>% 
    arrange(desc(total_c)) %>% 
    top_n(5) %>% 
# Plot and save bar plot
    ggplot() +
        geom_bar(aes(reorder(EVTYPE, total_c), total_c), 
                 stat = "identity") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "",
             y = "Damage (US dollars)",
             title = "Weather-related\nCrop Damage\nin US, 1950\n–Nov 2011")
```

```
## Selecting by total_c
```

```r
df2$all_dmg <- df2$prop_dmg_norm + df2$crop_dmg_norm
p5 <- df2 %>% 
    group_by(EVTYPE) %>% 
    summarise(total = sum(all_dmg)) %>% 
    arrange(desc(total)) %>% 
    top_n(5) %>% 
    ggplot() +
    geom_bar(aes(reorder(EVTYPE, total), total), 
             stat = "identity") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "",
         y = "Damage (US dollars)",
         title = "Total Weather\n-related Damage\nin US, 1950\n–Nov 2011")
```

```
## Selecting by total
```

## Results

```r
# Combine two bar plots into one
p1 + p2
```

![](RepData_PeerAssessment2_files/figure-html/Result for question 1-1.png)<!-- -->

During the period of 1950 to Nov 2011, tornado was the most harmful weather events causing largest number of fatalities and injuries in the US. It is followed by excessive heat (in terms of fatalities) and TSTM (thunderstorm) wind (in terms of injuries).


```r
# Combine three bar plots into one
p3 + p4 + p5
```

![](RepData_PeerAssessment2_files/figure-html/Results for question 2-1.png)<!-- -->

During the period of 1950 to Nov 2011, flood has caused the greatest economic consequence (in terms of total property and crop damage) in the US. It is also the single weather event that caused highest property damage. Considering crop damage, drought had the biggest share.

