# Download raw data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "./df.bz2", method = "curl")

# Read raw data
library(data.table)
df <- fread("./df.bz2")

# Assess weather-related population health damage
library(dplyr)
library(ggplot2)
library(patchwork)

# Generate and save plot 1
p1 <- df %>% 
        group_by(EVTYPE) %>% 
        summarise(total_f = sum(FATALITIES)) %>% 
        arrange(desc(total_f)) %>% 
        top_n(5) %>% 
        ggplot() +
                geom_bar(aes(reorder(EVTYPE, total_f), total_f), 
                         stat = "identity") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x = "",
                     y = "Count",
                     title = "Top 5 Causes of Weather Fatalities in US,\n1950–Nov 2011")

# Generate and save plot 2
p2 <- df %>% 
        group_by(EVTYPE) %>% 
        summarise(total_i = sum(INJURIES)) %>% 
        arrange(desc(total_i)) %>% 
        top_n(5) %>% 
        ggplot() +
                geom_bar(aes(reorder(EVTYPE, total_i), total_i), 
                         stat = "identity") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x = "",
                     y = "",
                     title = "Top 5 Causes of Weather Injuries in US,\n1950–Nov 2011")

# Combine plot 1 and plot 2 in to one
p1 + p2

# Assess weather-related econmic damage
summary(df$PROPDMG)
unique(df$PROPDMGEXP)

df2 <- df
# Replace empty values ("") with "+" to prevent dplyr::recode generating warning message of "Problem while computing `crop_dmg_exp = recode(...)`.
# Unreplaced values treated as NA as `.x` is not compatible.""
df2$PROPDMGEXP[df2$PROPDMGEXP == ""] <- "+"
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

# Generate and save plot 3
p3 <- df2 %>% 
        group_by(EVTYPE) %>% 
        summarise(total_p = sum(prop_dmg_norm)) %>% 
        arrange(desc(total_p)) %>% 
        top_n(5) %>% 
        ggplot() +
                geom_bar(aes(reorder(EVTYPE, total_p), total_p), 
                         stat = "identity") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x = "",
                     y = "Damage (US dollars)",
                     title = "Top 5 Causes of Weather-related\n Property Damage in US, 1950–Nov 2011")

summary(df2$CROPDMG)
unique(df2$CROPDMGEXP)
# Replace empty values ("") with "?"
df2$CROPDMGEXP[df2$CROPDMGEXP == ""] <- "?"
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

# Generate and save plot 4
p4 <- df2 %>% 
        group_by(EVTYPE) %>% 
        summarise(total_c = sum(crop_dmg_norm)) %>% 
        arrange(desc(total_c)) %>% 
        top_n(5) %>% 
        ggplot() +
                geom_bar(aes(reorder(EVTYPE, total_c), total_c), 
                         stat = "identity") +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(x = "",
                     y = "Damage (US dollars)",
                     title = "Top 5 Causes of Weather-related Crop Damage in US, 1950–Nov 2011")

# Calculate total econmic damage
df2$all_dmg <- df2$prop_dmg_norm + df2$crop_dmg_norm

# Generate and save the plot 5
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
                     title = "Top 5 Causes of Property & Crop Damage in US, 1950–Nov 2011")

# Combine three figures into one
p3 + p4 + p5