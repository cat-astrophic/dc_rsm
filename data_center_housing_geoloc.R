# This script geolocates houses for the data centers project

# Loading libraries

library(tidygeocoder)
library(dplyr)

# Project directory info

direc <- 'D:/data_centers/'

# Reading in the data

data <- read.csv(paste0(direc, 'data/housing_data.csv'))

# Geolocation

data$Address <- sub(' $', '', data$Address)
data$loc <- paste(data$Address, data$City, data$State, sep = ', ')

df <- as.data.frame(unique(data$loc))
colnames(df)[1] <- 'loc'

df1 <- as.data.frame(df[1:10000,1])
df2 <- as.data.frame(df[10001:20000,1])
df3 <- as.data.frame(df[20001:30000,1])
df4 <- as.data.frame(df[30001:40000,1])
df5 <- as.data.frame(df[40001:50000,1])
df6 <- as.data.frame(df[50001:60000,1])
df7 <- as.data.frame(df[60001:70000,1])
df8 <- as.data.frame(df[70001:80000,1])
df9 <- as.data.frame(df[80001:90000,1])
df10 <- as.data.frame(df[90001:100000,1])
df11 <- as.data.frame(df[100001:110000,1])
df12 <- as.data.frame(df[110001:120000,1])
df13 <- as.data.frame(df[120001:130000,1])
df14 <- as.data.frame(df[130001:140000,1])
df15 <- as.data.frame(df[140001:150000,1])
df16 <- as.data.frame(df[150001:160000,1])
df17 <- as.data.frame(df[160001:170000,1])
df18 <- as.data.frame(df[170001:180000,1])
df19 <- as.data.frame(df[180001:190000,1])
df20 <- as.data.frame(df[190001:200000,1])
df21 <- as.data.frame(df[200001:210000,1])
df22 <- as.data.frame(df[210001:220000,1])
df23 <- as.data.frame(df[220001:nrow(df),1])

colnames(df1)[1] <- 'loc'
colnames(df2)[1] <- 'loc'
colnames(df3)[1] <- 'loc'
colnames(df4)[1] <- 'loc'
colnames(df5)[1] <- 'loc'
colnames(df6)[1] <- 'loc'
colnames(df7)[1] <- 'loc'
colnames(df8)[1] <- 'loc'
colnames(df9)[1] <- 'loc'
colnames(df10)[1] <- 'loc'
colnames(df11)[1] <- 'loc'
colnames(df12)[1] <- 'loc'
colnames(df13)[1] <- 'loc'
colnames(df14)[1] <- 'loc'
colnames(df15)[1] <- 'loc'
colnames(df16)[1] <- 'loc'
colnames(df17)[1] <- 'loc'
colnames(df18)[1] <- 'loc'
colnames(df19)[1] <- 'loc'
colnames(df20)[1] <- 'loc'
colnames(df21)[1] <- 'loc'
colnames(df22)[1] <- 'loc'
colnames(df23)[1] <- 'loc'

df1 <- df1 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df2 <- df2 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df3 <- df3 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df4 <- df4 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df5 <- df5 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df6 <- df6 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df7 <- df7 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df8 <- df8 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df9 <- df9 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df10 <- df10 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df11 <- df11 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df12 <- df12 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df13 <- df13 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df14 <- df14 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df15 <- df15 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df16 <- df16 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df17 <- df17 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df18 <- df18 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df19 <- df19 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df20 <- df20 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df21 <- df21 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df22 <- df22 %>% geocode(loc, method = 'census', lat = lat , long = lon)
df23 <- df23 %>% geocode(loc, method = 'census', lat = lat , long = lon)

dfx <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12,
             df13, df14, df15, df16, df17, df18, df19, df20, df21, df22, df23)

data <- left_join(data, dfx, by = 'loc')

write.csv(data, paste0(direc, 'data/h_data.csv'), row.names = FALSE)

