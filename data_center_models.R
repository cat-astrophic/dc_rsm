# This script runs regressions for the data centers project

# Loading libraries

library(polars)
library(DIDmultiplegtDYN)
library(modelsummary)
library(stargazer)
library(sandwich)
library(splines2)
library(splines)
library(contdid)
library(ggplot2)
library(leaflet)
library(lmtest)
library(tigris)
library(fixest)
library(dplyr)
library(MASS)
library(lfe)
library(sf)

# Project directory info

direc <- 'D:/data_centers/'

# Reading in the data

data <- read.csv(paste0(direc, 'data/h_data.csv'))
cpi <- read.csv(paste0(direc, 'data/cpi.csv'))

# Imputing October 2025 CPI

cpi$Oct[113] <- (cpi$Sep[113] + cpi$Nov[113]) / 2

# Adjusting prices via CPI

idx <- match(data$Year, cpi$Year)
data$CPI <- cpi[cbind(idx, data$Month+1)]
data$Real_Price <- data$Price * (cpi$Dec[113] / data$CPI)

# Identify the data centers in our sample

dc <- read.csv(paste0(direc, 'data/datacentermap.csv'))
dc <- dc %>% filter(state %in% c('Connecticut', 'Massachusetts', 'Maine', 'New Hampshire', 'Virginia'))
dc <- dc %>% filter(!is.na(latitude))
dc$year_operational <- ifelse(is.na(dc$year_operational), dc$year_operational_2, dc$year_operational)
dc <- dc %>% filter(!is.na(year_operational))
dc <- st_as_sf(dc, coords = c('longitude', 'latitude'))

data <- data %>% filter(!is.na(lat))
data <- st_as_sf(data, coords = c('lon', 'lat'))

dc <- st_set_crs(dc, 4326)
data <- st_set_crs(data, 4326)

nearest.feature <- st_nearest_feature(data, dc)
which_dc <- dc[nearest.feature,]
dc_distances <- st_distance(data, which_dc, by_element = TRUE)

data$Distance <- as.numeric(dc_distances)
data$Open <- which_dc$year_operational
data$Center <- which_dc$id

dc_data <- data %>% filter(!is.na(Open))

dc_data$Post <- ifelse(dc_data$Year > dc_data$Open, 1, 0)
dc_data$Drop <- ifelse(dc_data$Year == dc_data$Open, 1, 0)

dc_data$T1 <- ifelse(dc_data$Distance <= 1000, 1, 0)
dc_data$T2 <- ifelse(dc_data$Distance <= 2000, 1, 0)
dc_data$T3 <- ifelse(dc_data$Distance <= 3000, 1, 0)
dc_data$T4 <- ifelse(dc_data$Distance <= 4000, 1, 0)
dc_data$T5 <- ifelse(dc_data$Distance <= 5000, 1, 0)

dc_data$T2X <- dc_data$T2 - dc_data$T1
dc_data$T3X <- dc_data$T3 - dc_data$T2X
dc_data$T4X <- dc_data$T4 - dc_data$T3X
dc_data$T5X <- dc_data$T5 - dc_data$T4X

dc_data$TM <- ifelse(dc_data$Distance <= 1600, 1, 0)
dc_data$TM2 <- ifelse(dc_data$Distance <= 3200, 1, 0)
dc_data$TM2X <- dc_data$TM2 - dc_data$TM

dc_data2 <- dc_data[which(dc_data$Real_Price > 10000 & dc_data$Real_Price < 1000000),]
dc_data2 <- dc_data2[which(dc_data2$Drop != 1),]

# Remove observations around old (1971) data center in Chelmsford, MA

dc_data2 <- dc_data2[which(dc_data2$Open != 1971),]

# Models at 5000 meters

mod1 <- felm(log(Real_Price) ~ log(Distance)*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod2 <- felm(log(Real_Price) ~ T1*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod3 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod4 <- felm(log(Real_Price) ~ T3*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod5 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod6 <- felm(log(Real_Price) ~ T1*Post + T2X*Post + T3X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod7 <- felm(log(Real_Price) ~ TM*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod8 <- felm(log(Real_Price) ~ TM2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

mod9 <- felm(log(Real_Price) ~ TM*Post + TM2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
summary(mod7)
summary(mod8)
summary(mod9)

# Models at 8000 meters

mmod1 <- felm(log(Real_Price) ~ log(Distance)*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod2 <- felm(log(Real_Price) ~ T1*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod3 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod4 <- felm(log(Real_Price) ~ T3*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod5 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod6 <- felm(log(Real_Price) ~ T1*Post + T2X*Post + T3X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod7 <- felm(log(Real_Price) ~ TM*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod8 <- felm(log(Real_Price) ~ TM2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

mmod9 <- felm(log(Real_Price) ~ TM*Post + TM2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

summary(mmod1)
summary(mmod2)
summary(mmod3)
summary(mmod4)
summary(mmod5)
summary(mmod6)
summary(mmod7)
summary(mmod8)
summary(mmod9)

# Models at 10000 meters

mmmod1 <- felm(log(Real_Price) ~ log(Distance)*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod2 <- felm(log(Real_Price) ~ T1*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod3 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod4 <- felm(log(Real_Price) ~ T3*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod5 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod6 <- felm(log(Real_Price) ~ T1*Post + T2X*Post + T3X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod7 <- felm(log(Real_Price) ~ TM*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod8 <- felm(log(Real_Price) ~ TM2*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

mmmod9 <- felm(log(Real_Price) ~ TM*Post + TM2X*Post | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

summary(mmmod1)
summary(mmmod2)
summary(mmmod3)
summary(mmmod4)
summary(mmmod5)
summary(mmmod6)
summary(mmmod7)
summary(mmmod8)
summary(mmmod9)

# Collecting and saving all terms

model1.5 <- c(mod2$coefficients[3], mod2$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model2.5 <- c(NA, NA, mod3$coefficients[3], mod3$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model3.5 <- c(NA, NA, NA, NA, mod4$coefficients[3], mod4$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model4.5 <- c(mod5$coefficients[5], mod5$cse[5], NA, NA, NA, NA, mod5$coefficients[6], mod5$cse[6], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model5.5 <- c(mod6$coefficients[5], mod6$cse[5], NA, NA, NA, NA, mod6$coefficients[6], mod6$cse[6], mod6$coefficients[7], mod6$cse[7], NA, NA, NA, NA, NA, NA, NA, NA)
model6.5 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mod7$coefficients[3], mod7$cse[3], NA, NA, NA, NA, NA, NA)
model7.5 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mod8$coefficients[3], mod8$cse[3], NA, NA, NA, NA)
model8.5 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mod9$coefficients[4], mod9$cse[4], NA, NA, mod9$coefficients[5], mod9$cse[5], NA, NA)
model9.5 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mod1$coefficients[3], mod1$cse[3])

model.5000 <- as.data.frame(cbind(model1.5, model2.5, model3.5, model4.5, model5.5, model6.5, model7.5, model8.5, model9.5))

model1.8 <- c(mmod2$coefficients[3], mmod2$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model2.8 <- c(NA, NA, mmod3$coefficients[3], mmod3$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model3.8 <- c(NA, NA, NA, NA, mmod4$coefficients[3], mmod4$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model4.8 <- c(mmod5$coefficients[5], mmod5$cse[5], NA, NA, NA, NA, mmod5$coefficients[6], mmod5$cse[6], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model5.8 <- c(mmod6$coefficients[5], mmod6$cse[5], NA, NA, NA, NA, mmod6$coefficients[6], mmod6$cse[6], mmod6$coefficients[7], mmod6$cse[7], NA, NA, NA, NA, NA, NA, NA, NA)
model6.8 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmod7$coefficients[3], mmod7$cse[3], NA, NA, NA, NA, NA, NA)
model7.8 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmod8$coefficients[3], mmod8$cse[3], NA, NA, NA, NA)
model8.8 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmod9$coefficients[4], mmod9$cse[4], NA, NA, mmod9$coefficients[5], mmod9$cse[5], NA, NA)
model9.8 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmod1$coefficients[3], mmod1$cse[3])

model.8000 <- as.data.frame(cbind(model1.8, model2.8, model3.8, model4.8, model5.8, model6.8, model7.8, model8.8, model9.8))

model1.1 <- c(mmmod2$coefficients[3], mmmod2$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model2.1 <- c(NA, NA, mmmod3$coefficients[3], mmmod3$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model3.1 <- c(NA, NA, NA, NA, mmmod4$coefficients[3], mmmod4$cse[3], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model4.1 <- c(mmmod5$coefficients[5], mmmod5$cse[5], NA, NA, NA, NA, mmmod5$coefficients[6], mmmod5$cse[6], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
model5.1 <- c(mmmod6$coefficients[5], mmmod6$cse[5], NA, NA, NA, NA, mmmod6$coefficients[6], mmmod6$cse[6], mmmod6$coefficients[7], mmmod6$cse[7], NA, NA, NA, NA, NA, NA, NA, NA)
model6.1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmmod7$coefficients[3], mmmod7$cse[3], NA, NA, NA, NA, NA, NA)
model7.1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmmod8$coefficients[3], mmmod8$cse[3], NA, NA, NA, NA)
model8.1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmmod9$coefficients[4], mmmod9$cse[4], NA, NA, mmmod9$coefficients[5], mmmod9$cse[5], NA, NA)
model9.1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mmmod1$coefficients[3], mmmod1$cse[3])

model.10000 <- as.data.frame(cbind(model1.1, model2.1, model3.1, model4.1, model5.1, model6.1, model7.1, model8.1, model9.1))

write.csv(model.5000, paste0(direc, 'results/model.5000.txt'), row.names = FALSE)
write.csv(model.8000, paste0(direc, 'results/model.8000.txt'), row.names = FALSE)
write.csv(model.10000, paste0(direc, 'results/model.10000.txt'), row.names = FALSE)

# More granular models to demonstrate the end of the effect radius

dc_data2$T0.5 <- ifelse(dc_data2$Distance <= 500, 1, 0)
dc_data2$T1X <- dc_data2$T1 - dc_data2$T0.5
dc_data2$T1.5X <- ifelse(dc_data2$Distance <= 1500 & dc_data2$Distance > 1000, 1, 0)
dc_data2$T2.0X <- ifelse(dc_data2$Distance <= 2000 & dc_data2$Distance > 1500, 1, 0)
dc_data2$T2.5X <- ifelse(dc_data2$Distance <= 2500 & dc_data2$Distance > 2000, 1, 0)

gmod <- felm(log(Real_Price) ~ Post*T0.5 + Post*T1X + Post*T1.5X + Post*T2.0X + Post*T2.5X + Post*T3X | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

summary(gmod)

g1 <- c(gmod$coefficients[8], gmod$cse[8], gmod$coefficients[9], gmod$cse[9], gmod$coefficients[10], gmod$cse[10],
        gmod$coefficients[11], gmod$cse[11], gmod$coefficients[12], gmod$cse[12], gmod$coefficients[13], gmod$cse[13])

gres <- as.data.frame(g1)

write.csv(gres, paste0(direc, 'results/granular.txt'), row.names = FALSE)

# Even more granular models to demonstrate the end of the effect radius

dc_data2$T0.2 <- ifelse(dc_data2$Distance <= 200, 1, 0)
dc_data2$T0.4XX <- ifelse(dc_data2$Distance <= 400 & dc_data2$Distance > 200, 1, 0)
dc_data2$T0.6XX <- ifelse(dc_data2$Distance <= 600 & dc_data2$Distance > 400, 1, 0)
dc_data2$T0.8XX <- ifelse(dc_data2$Distance <= 800 & dc_data2$Distance > 600, 1, 0)
dc_data2$T1.0XX <- ifelse(dc_data2$Distance <= 1000 & dc_data2$Distance > 800, 1, 0)
dc_data2$T1.2XX <- ifelse(dc_data2$Distance <= 1200 & dc_data2$Distance > 1000, 1, 0)
dc_data2$T1.4XX <- ifelse(dc_data2$Distance <= 1400 & dc_data2$Distance > 1200, 1, 0)
dc_data2$T1.6XX <- ifelse(dc_data2$Distance <= 1600 & dc_data2$Distance > 1400, 1, 0)
dc_data2$T1.8XX <- ifelse(dc_data2$Distance <= 1800 & dc_data2$Distance > 1600, 1, 0)
dc_data2$T2.0XX <- ifelse(dc_data2$Distance <= 2000 & dc_data2$Distance > 1800, 1, 0)
dc_data2$T2.2XX <- ifelse(dc_data2$Distance <= 2200 & dc_data2$Distance > 2000, 1, 0)
dc_data2$T2.4XX <- ifelse(dc_data2$Distance <= 2400 & dc_data2$Distance > 2200, 1, 0)
dc_data2$T2.6XX <- ifelse(dc_data2$Distance <= 2600 & dc_data2$Distance > 2400, 1, 0)
dc_data2$T2.8XX <- ifelse(dc_data2$Distance <= 2800 & dc_data2$Distance > 2600, 1, 0)
dc_data2$T3.0XX <- ifelse(dc_data2$Distance <= 3000 & dc_data2$Distance > 2800, 1, 0)

gmod2 <- felm(log(Real_Price) ~ Post*T0.2 + Post*T0.4XX + Post*T0.6XX + Post*T0.8XX
              + Post*T1.0XX + Post*T1.2XX + Post*T1.4XX + Post*T1.6XX + Post*T1.8XX
              + Post*T2.0XX + Post*T2.2XX + Post*T2.4XX + Post*T2.6XX + Post*T2.8XX
              + Post*T3.0XX | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

summary(gmod2)

g2 <- c(gmod2$coefficients[17], gmod2$cse[17], gmod2$coefficients[18], gmod2$cse[18], gmod2$coefficients[19], gmod2$cse[19],
        gmod2$coefficients[20], gmod2$cse[20], gmod2$coefficients[21], gmod2$cse[21], gmod2$coefficients[22], gmod2$cse[22],
        gmod2$coefficients[23], gmod2$cse[23], gmod2$coefficients[24], gmod2$cse[24], gmod2$coefficients[25], gmod2$cse[25],
        gmod2$coefficients[26], gmod2$cse[26], gmod2$coefficients[27], gmod2$cse[27], gmod2$coefficients[28], gmod2$cse[28],
        gmod2$coefficients[29], gmod2$cse[29], gmod2$coefficients[30], gmod2$cse[30], gmod2$coefficients[31], gmod2$cse[31])

gres2 <- as.data.frame(g2)

write.csv(gres2, paste0(direc, 'results/more_granularer.txt'), row.names = FALSE)

# Even more granular models to demonstrate the end of the effect radius

dc_data2$T0.333XXX <- ifelse(dc_data2$Distance <= 333, 1, 0)
dc_data2$T0.666XXX <- ifelse(dc_data2$Distance <= 666 & dc_data2$Distance > 333, 1, 0)
dc_data2$T0.999XXX <- ifelse(dc_data2$Distance <= 1000 & dc_data2$Distance > 666, 1, 0)
dc_data2$T1.333XXX <- ifelse(dc_data2$Distance <= 1333 & dc_data2$Distance > 1000, 1, 0)
dc_data2$T1.666XXX <- ifelse(dc_data2$Distance <= 1666 & dc_data2$Distance > 1333, 1, 0)
dc_data2$T1.999XXX <- ifelse(dc_data2$Distance <= 2000 & dc_data2$Distance > 1666, 1, 0)

dc_datax <- dc_data2 %>% filter(Distance <= 5000)

gmodx <- felm(log(Real_Price) ~ Post*T0.333XXX + Post*T0.666XXX + Post*T0.999XXX + Post*T1.333XXX + Post*T1.666XXX
              + Post*T1.999XXX | QY + Address | 0 | Address, data = dc_datax[which(dc_datax$Distance <= 2000 | dc_datax$Distance >= 3000),])

summary(gmodx)

gx <- c(gmodx$coefficients[8], gmodx$cse[8], gmodx$coefficients[9], gmodx$cse[9], gmodx$coefficients[10], gmodx$cse[10],
        gmodx$coefficients[11], gmodx$cse[11], gmodx$coefficients[12], gmodx$cse[12], gmodx$coefficients[13], gmodx$cse[13])

gresx <- as.data.frame(gx)

write.csv(gresx, paste0(direc, 'results/more_granularerx.txt'), row.names = FALSE)

# Effect sizes over distance

dc_data9 <- dc_data2[which(dc_data2$Distance <= 10000),]

models.betas <- c()
models.serrs <- c()

for (i in 2:50) {
  
  print(i)
  
  dc_data9$Treated <- as.numeric(dc_data9$Distance <= i*100)
  
  models <- felm(log(Real_Price) ~ Treated*Post | QY + Address | 0 | Address, data = dc_data9)
  
  models.betas <- c(models.betas, models$coefficients[3])
  models.serrs <- c(models.serrs, models$cse[3])
  
}

increments <- as.data.frame(cbind(models.betas, models.serrs, seq(200, 5000, by = 100)))
colnames(increments) <- c('Beta', 'SE', 'Treatment')

ggplot(data = increments, aes(x = Treatment, y = Beta)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = Beta - 2*SE, ymax = Beta + 2*SE), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Effect Size Over Distance') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for Treatment Distance') +
  geom_hline(yintercept = 0, size = 1)

# Effect sizes over distance from granular model

gdf <- as.data.frame(cbind(gres$g1[c(1,3,5,7,9,11)], gres$g1[c(2,4,6,8,10,12)]))
gdf$Treatment <- c(500,1000,1500,2000,2500,3000)

ggplot(data = gdf, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = V1 - 2*V2, ymax = V1 + 2*V2), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 500m Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 3000, by = 500)) +
  ylim(-0.3,0.2)

ggplot(data = gdf, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = V1 - 2*V2, ymax = V1 + 2*V2)) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 500m Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 3000, by = 500)) +
  ylim(-0.25,0.1)

# Effect sizes over distance from more granularer model

gdf2 <- as.data.frame(cbind(gres2$g2[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)],
                            gres2$g2[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)]))
gdf2$Treatment <- seq(200, 3000, by = 200)

ggplot(data = gdf2, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = V1 - 2*V2, ymax = V1 + 2*V2), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 200m Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 3000, by = 500))

ggplot(data = gdf2, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = V1 - 2*V2, ymax = V1 + 2*V2)) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 200m Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 3000, by = 500))

# Effect sizes over distance from more granularerx model

gdfx <- as.data.frame(cbind(gresx$gx[c(1,3,5,7,9,11)],
                            gresx$gx[c(2,4,6,8,10,12)]))
gdfx$Treatment <- c(333, 666, 1000, 1333, 1666, 2000)

ggplot(data = gdfx, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(aes(ymin = V1 - 1.96*V2, ymax = V1 + 1.96*V2), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 1/3 km Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = gdfx$Treatment) +
  ylim(c(-0.5,0.1))

ggplot(data = gdfx, aes(x = Treatment, y = V1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = V1 - 1.96*V2, ymax = V1 + 1.96*V2)) +
  theme_bw() +
  ggtitle('Effect Size Across Bins') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Upper Bound for 1/3 km Treatment Bin') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = gdfx$Treatment) +
  ylim(c(-0.5,0.1))

# Three ring circus

dc_data_xxx0 <- dc_data2[which(dc_data2$Distance >= 3000),]
dc_data_xxx1 <- dc_data2[which(dc_data2$Distance <= 2000),]
dc_data_xxx <- rbind(dc_data_xxx0, dc_data_xxx1)

tmod1 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 5000),])

tmod2 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 8000),])

tmod3 <- felm(log(Real_Price) ~ T2*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 10000),])

tmod4 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 5000),])

tmod5 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 8000),])

tmod6 <- felm(log(Real_Price) ~ T1*Post + T2X*Post | QY + Address | 0 | Address, data = dc_data_xxx[which(dc_data_xxx$Distance <= 10000),])

summary(tmod1)
summary(tmod2)
summary(tmod3)
summary(tmod4)
summary(tmod5)
summary(tmod6)

modelt1 <- c(tmod1$coefficients[3], tmod1$cse[3], NA, NA, NA, NA)
modelt2 <- c(tmod2$coefficients[3], tmod2$cse[3], NA, NA, NA, NA)
modelt3 <- c(tmod3$coefficients[3], tmod3$cse[3], NA, NA, NA, NA)
modelt4 <- c(NA, NA, tmod4$coefficients[4], tmod4$cse[4], tmod4$coefficients[5], tmod4$cse[5])
modelt5 <- c(NA, NA, tmod5$coefficients[4], tmod5$cse[4], tmod5$coefficients[5], tmod5$cse[5])
modelt6 <- c(NA, NA, tmod6$coefficients[4], tmod6$cse[4], tmod6$coefficients[5], tmod6$cse[5])

model.t <- as.data.frame(cbind(modelt1, modelt2, modelt3, modelt4, modelt5, modelt6))

write.csv(model.t, paste0(direc, 'results/model.three.ring.txt'), row.names = FALSE)

# Percentile based models

green <- dc_data2[which(dc_data2$Distance <= 10000),]
green$Percentile <- percent_rank(green$Distance)

betas <- c()
stderrs <- c()
pvals <- c()

for (i in 1:80) {
  
  print(paste0('Running model for percentile ', i, '.......'))
  
  green$TT <- ifelse(green$Percentile <= i/100, 1, 0)
  
  next.mod <- felm(log(Real_Price) ~ TT*Post | QY + Address | 0 | Address, data = green)
  
  betas <- c(betas, next.mod$coefficients[3])
  stderrs <- c(stderrs, next.mod$cse[3])
  pvals <- c(pvals, next.mod$cpval[3])
  
}

green2 <- dc_data2[which(dc_data2$Distance <= 5000),]
green2$Percentile <- percent_rank(green2$Distance)

betas5 <- c()
stderrs5 <- c()
pvals5 <- c()

for (i in 1:80) {
  
  print(paste0('Running model for percentile ', i, '.......'))
  
  green2$TT <- ifelse(green2$Percentile <= i/100, 1, 0)
  
  next.mod <- felm(log(Real_Price) ~ TT*Post | QY + Address | 0 | Address, data = green2)
  
  betas5 <- c(betas5, next.mod$coefficients[3])
  stderrs5 <- c(stderrs5, next.mod$cse[3])
  pvals5 <- c(pvals5, next.mod$cpval[3])
  
}

green3 <- dc_data2[which(dc_data2$Distance <= 8000),]
green3$Percentile <- percent_rank(green3$Distance)

betas8 <- c()
stderrs8 <- c()
pvals8 <- c()

for (i in 1:80) {
  
  print(paste0('Running model for percentile ', i, '.......'))
  
  green3$TT <- ifelse(green3$Percentile <= i/100, 1, 0)
  
  next.mod <- felm(log(Real_Price) ~ TT*Post | QY + Address | 0 | Address, data = green3)
  
  betas8 <- c(betas8, next.mod$coefficients[3])
  stderrs8 <- c(stderrs8, next.mod$cse[3])
  pvals8 <- c(pvals8, next.mod$cpval[3])
  
}

pct.df <- as.data.frame(cbind(1:80, betas5, betas8, betas, stderrs5, stderrs8, stderrs, pvals5, pvals8, pvals))

colnames(pct.df) <- c('Percentile', 'Beta_5k', 'Beta_8k', 'Beta_10k', 'Std_Err_5k',
                      'Std_Err_8k', 'Std_Err_10k', 'p_val_5k', 'p_val_8k', 'p_val_10k')

write.csv(pct.df, paste0(direc, 'results/percentiles.txt'), row.names = FALSE)

# Which percentile is the 2,000 meter group for each control radius

five.k.2k <- ecdf(dc_data2[which(dc_data2$Distance <= 5000),]$Distance)(2000)
eight.k.2k <- ecdf(dc_data2[which(dc_data2$Distance <= 8000),]$Distance)(2000)
ten.k.2k <- ecdf(dc_data2[which(dc_data2$Distance <= 10000),]$Distance)(2000)

# > five.k.2k
# [1] 0.2112243
# > eight.k.2k
# [1] 0.159486
# > ten.k.2k
# [1] 0.1538601

# Plots

ggplot(data = pct.df, aes(x = Percentile, y = Beta_5k)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = Beta_5k - 2*Std_Err_5k, ymax = Beta_5k + 2*Std_Err_5k), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('ATT Over Distance Percentiles - 5km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Distance Percentile') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  geom_vline(xintercept = 21.12243, linetype = 'dashed') +
  ylim(c(-0.25, 0.05))

ggplot(data = pct.df, aes(x = Percentile, y = Beta_8k)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = Beta_8k - 2*Std_Err_8k, ymax = Beta_8k + 2*Std_Err_8k), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('ATT Over Distance Percentiles - 8km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Distance Percentile') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  geom_vline(xintercept = 15.9486, linetype = 'dashed') +
  ylim(c(-0.25, 0.05))

ggplot(data = pct.df, aes(x = Percentile, y = Beta_10k)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = Beta_10k - 2*Std_Err_10k, ymax = Beta_10k + 2*Std_Err_10k), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('ATT Over Distance Percentiles - 10km') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Effect Size') +
  xlab('Distance Percentile') +
  geom_hline(yintercept = 0, size = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  geom_vline(xintercept = 15.38601, linetype = 'dashed') +
  ylim(c(-0.25, 0.05))

# Estimating a dose response function with i-splines and 7 breaks - 5 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 7) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

grid <- data.frame(Distance = seq(0, 5000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:501])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Estimating a dose response function with i-splines and 5 breaks - 5 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 5) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

grid <- data.frame(Distance = seq(0, 5000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:501])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Estimating a dose response function with i-splines and 7 breaks - 8 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 7) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

grid <- data.frame(Distance = seq(0, 8000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:801])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Estimating a dose response function with i-splines and 5 breaks - 8 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 5) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 8000),])

grid <- data.frame(Distance = seq(0, 8000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:801])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Estimating a dose response function with i-splines and 7 breaks - 10 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 7) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

grid <- data.frame(Distance = seq(0, 10000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 7), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:1001])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Estimating a dose response function with i-splines and 5 breaks - 10 km sample

dose.mod <- feols(log(Real_Price) ~ Post * isp(Distance, df = 5) |
                    Address + QY, cluster = ~Address, data = dc_data2[which(dc_data2$Distance <= 10000),])

grid <- data.frame(Distance = seq(0, 10000, by = 10))

X1 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 1))
X0 <- model.matrix(~ Post * isp(Distance, df = 5), data = transform(grid, Post = 0))

b <- coef(dose.mod)

X1 <- X1[, names(b), drop = FALSE]
X0 <- X0[, names(b), drop = FALSE]

Xd <- X1 - X0

att <- as.vector(Xd %*% b)

bar <- mean(att[301:1001])
attbar <- att - bar 

V <- vcov(dose.mod)

se <- sqrt(diag(Xd %*% V %*% t(Xd)))

plotdat <- data.frame(
  Distance = grid$Distance,
  ATT = attbar,
  LB = attbar - 1.96 * se,
  UB = attbar + 1.96 * se
)

ggplot(plotdat, aes(x = Distance, y = ATT)) +
  geom_ribbon(aes(ymin = LB, ymax = UB), alpha = .2, fill = 'orange2') +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(
    x = 'Distance to nearest data center (meters)',
    y = '',
    title = 'Dose–Response Curve'
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-0.7,0.3))

# Double hist of real prices

pdf0 <- dc_data2[which(dc_data2$Distance <= 10000 & dc_data2$Post == 0),]
pdf1 <- dc_data2[which(dc_data2$Distance <= 10000 & dc_data2$Post == 1),]

ggplot() + 
  theme_bw() +
  geom_density(data = pdf0, aes(x = Real_Price, fill = 'Before Data Center'), alpha = 0.2) +
  geom_density(data = pdf1, aes(x = Real_Price, fill = 'After Data Center'), alpha = 0.2) +
  scale_colour_manual(name = 'Legend', values = c('Before Data Center' = 'orange2', 'After Data Center' = 'red4')) +
  scale_fill_manual(name = 'Legend', values = c('Before Data Center' = 'orange2', 'After Data Center' = 'red4')) +
  ggtitle('Distributions of Real House Transaction Prices') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Density') +
  xlab('Price (2025 USD)')

pdf0 <- dc_data2[which(dc_data2$Distance <= 3000),]
pdf1 <- dc_data2[which(dc_data2$Distance > 3000 & dc_data2$Distance <= 5000),]

ggplot() + 
  theme_bw() +
  geom_density(data = pdf0, aes(x = Real_Price, fill = 'Nearby Data Center'), alpha = 0.2) +
  geom_density(data = pdf1, aes(x = Real_Price, fill = 'Remote Data Center'), alpha = 0.2) +
  scale_colour_manual(name = 'Legend', values = c('Nearby Data Center' = 'orange2', 'Remote Data Center' = 'red4')) +
  scale_fill_manual(name = 'Legend', values = c('Nearby Data Center' = 'orange2', 'Remote Data Center' = 'red4')) +
  ggtitle('Distributions of Real House Transaction Prices') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Density') +
  xlab('Price (2025 USD)')

# Making a leaflet of study area

cons <- counties()
cons <- st_transform(cons, 4326)

insides <- st_within(dc_data9[which(dc_data9$Center != 9275),], cons)
organs <- unique(insides)
organelles <- c()
for (i in 1:length(organs)) {organelles <- c(organelles, organs[[i]][1])}

cons <- cons[organelles,]

marvel <- read.csv(paste0(direc, 'data/datacentermap.csv'))
marvel <- marvel %>% filter(state %in% c('Connecticut', 'Massachusetts', 'Maine', 'New Hampshire', 'Virginia'))
marvel <- marvel %>% filter(!is.na(latitude))

marvelous <- st_as_sf(marvel, coords = c('longitude', 'latitude'))
marvelous <- st_set_crs(marvelous, 4326)

nearest.features <- st_nearest_feature(dc_data9, marvelous)

leaflet(dc_data9) %>% addPolygons(data = cons$geometry, color = 'black') %>% addTiles() %>%
  addCircleMarkers(data = marvel[unique(nearest.features),], lng = ~longitude, lat = ~latitude, radius = 1, color = 'red')

# Event study models for pre-trends annually

dc_data3 <- dc_data[which(dc_data$Real_Price > 10000 & dc_data$Real_Price < 1000000),]
dc_data3 <- dc_data3[-which(dc_data3$Open %in% c(1971)),]
dc_data3$Period <- dc_data3$Year - dc_data3$Open
dc_data3$ES <- as.factor(dc_data3$Period)
dc_data3$ES <- relevel(dc_data3$ES, ref = 36)

esmod <- felm(log(Real_Price) ~ T2*ES | QY + Address | 0 | Address, data = dc_data3[which(dc_data3$Distance <= 5000 & dc_data3$Period %in% -15:10),])

summary(esmod)

# Make event study plot

coefs <- c(esmod$coefficients[32:39], 0, esmod$coefficients[40:48])
serrs <- c(esmod$cse[32:39], 0, esmod$cse[40:48])

esdf <- as.data.frame(cbind(coefs, serrs))

ggplot(data = esdf, aes(x = -10:7, y = coefs)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = coefs - 1.96*serrs, ymax = coefs + 1.96*serrs)) +
  theme_bw() +
  ggtitle('Event Study Model Results') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Coefficient') +
  xlab('Year Relative to Data Center Opening') +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = -0.5, linetype = 'dotted', size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  ylim(c(-0.4,0.4))

ggplot(data = esdf, aes(x = -10:7, y = coefs)) + 
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = coefs - 1.96*serrs, ymax = coefs + 1.96*serrs), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Event Study Model Results') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Coefficient') +
  xlab('Year Relative to Data Center Opening') +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = -0.5, linetype = 'dotted', size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Event study models for pre-trends annually

dc_data3 <- dc_data[which(dc_data$Real_Price > 10000 & dc_data$Real_Price < 1000000),]
dc_data3 <- dc_data3[-which(dc_data3$Open %in% c(1971)),]
dc_data3$Period <- dc_data3$Year - dc_data3$Open
dc_data3$ES <- as.factor(dc_data3$Period)
dc_data3$ES <- relevel(dc_data3$ES, ref = 37)

esmod <- felm(log(Real_Price) ~ T2*ES | Address | 0 | Address, data = dc_data3[which(dc_data3$Distance <= 5000 & dc_data3$Period %in% -15:10),])

summary(esmod)

# Make event study plot

coefs <- c(esmod$coefficients[32:39], 0, esmod$coefficients[40:48])
serrs <- c(esmod$cse[32:39], 0, esmod$cse[40:48])

esdf <- as.data.frame(cbind(coefs, serrs))

ggplot(data = esdf, aes(x = -10:7, y = coefs)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = coefs - 1.96*serrs, ymax = coefs + 1.96*serrs)) +
  theme_bw() +
  ggtitle('Event Study Model Results') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Coefficient') +
  xlab('Year Relative to Data Center Opening') +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = -0.5, linetype = 'dotted', size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggplot(data = esdf, aes(x = -10:7, y = coefs)) + 
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = coefs - 1.96*serrs, ymax = coefs + 1.96*serrs), color = 'orange2', fill = 'orange2', alpha = 0.3) +
  theme_bw() +
  ggtitle('Event Study Model Results') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Coefficient') +
  xlab('Year Relative to Data Center Opening') +
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = -0.5, linetype = 'dotted', size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Summary statistics

# 5.000  :: 104.363 :: 22,044 ::  82,319 :: 21.4%
# 8.000  :: 138.219 :: 22,044 :: 116,175 :: 15.9%
# 10.000 :: 143.273 :: 22,044 :: 121,229 :: 15.4%

# mean(dc_data2[which(dc_data2$Distance <= 5000),]$Real_Price)                :: 307,694
# mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 1),]$Real_Price) :: 282,229
# mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 0),]$Real_Price) :: 314,514

# sd(dc_data2[which(dc_data2$Distance <= 5000),]$Real_Price)                  :: 165,620
# sd(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 1),]$Real_Price)   :: 156,352
# sd(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 0),]$Real_Price)   :: 167,359

# mod3$coefficients[3]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 1),]$Real_Price) :: -20681.31
# mod3$coefficients[3]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2 == 1),]$Real_Price) :: -16919.93

# mod5$coefficients[4]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T1 == 1),]$Real_Price)  :: -31286.3
# mod5$coefficients[5]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2X == 1),]$Real_Price) :: -15481.09

# mmmod5$coefficients[4]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T1 == 1),]$Real_Price)  :: -27003.68
# mmmod5$coefficients[5]*mean(dc_data2[which(dc_data2$Distance <= 5000 & dc_data2$T2X == 1),]$Real_Price) :: -11981.9

# A plot comparing prices before and after

dc_data2$T3.2XX <- ifelse(dc_data2$Distance <= 3200 & dc_data2$Distance > 3000, 1, 0)
dc_data2$T3.4XX <- ifelse(dc_data2$Distance <= 3400 & dc_data2$Distance > 3200, 1, 0)
dc_data2$T3.6XX <- ifelse(dc_data2$Distance <= 3600 & dc_data2$Distance > 3400, 1, 0)
dc_data2$T3.8XX <- ifelse(dc_data2$Distance <= 3800 & dc_data2$Distance > 3600, 1, 0)
dc_data2$T4.0XX <- ifelse(dc_data2$Distance <= 4000 & dc_data2$Distance > 3800, 1, 0)
dc_data2$T4.2XX <- ifelse(dc_data2$Distance <= 4200 & dc_data2$Distance > 4000, 1, 0)
dc_data2$T4.4XX <- ifelse(dc_data2$Distance <= 4400 & dc_data2$Distance > 4200, 1, 0)
dc_data2$T4.6XX <- ifelse(dc_data2$Distance <= 4600 & dc_data2$Distance > 4400, 1, 0)
dc_data2$T4.8XX <- ifelse(dc_data2$Distance <= 4800 & dc_data2$Distance > 4600, 1, 0)

gmodx <- felm(log(Real_Price) ~ Post*T0.2 + Post*T0.4XX + Post*T0.6XX + Post*T0.8XX
              + Post*T1.0XX + Post*T1.2XX + Post*T1.4XX + Post*T1.6XX + Post*T1.8XX
              + Post*T2.0XX + Post*T2.2XX + Post*T2.4XX + Post*T2.6XX + Post*T2.8XX
              + Post*T3.0XX + Post*T3.2XX + Post*T3.4XX + Post*T3.6XX + Post*T3.8XX 
              + Post*T4.0XX + Post*T4.2XX + Post*T4.4XX + Post*T4.6XX + Post*T4.8XX 
              | QY + Address | 0 | Address, data = dc_data2[which(dc_data2$Distance <= 5000),])

summary(gmodx)

gplot <- as.data.frame(gmodx$coefficients[26:49])
colnames(gplot) <- 'V1'

pre.x <- c()

its <- seq(200, 4800, by = 200)

for (i in its) {
  
  print(i)
  tmp <- dc_data2[which(dc_data2$Distance > (i-200) & dc_data2$Distance <= i),]
  pre.x <- c(pre.x, mean(tmp$Real_Price, na.rm = TRUE))
  
}

gplot$Pre <- pre.x
gplot$Post <- gplot$Pre * (1+gplot$V1)
gplot$Distance <- its

ggplot(gplot) + 
  theme_bw() +
  geom_smooth(data = gplot, aes(x = Distance, y = Pre, color = 'Before Data Center'), method = 'loess', span = 1, level = 0.9) +
  geom_smooth(data = gplot, aes(x = Distance, y = Post, color = 'After Data Center'), method = 'loess', span = 1, level = 0.9) +
  scale_colour_manual(name = 'Legend', values = c('Before Data Center' = 'orange2', 'After Data Center' = 'red4')) +
  scale_fill_manual(name = 'Legend', values = c('Before Data Center' = 'orange2', 'After Data Center' = 'red4')) +
  ggtitle('Real House Transaction Prices Before and After Data Center Openings') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Price (2025 USD)') +
  xlab('Distance from Data Center')

