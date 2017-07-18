#####################################################################################################
# R script : Explore and map the catch data
#
# Author : Clara Peron
#
# Date : 17-07-2017
#
# R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#
#####################################################################################################

rm(list=ls())

# Load libraries
  library(plyr)
  library(dplyr)
  library(raster)
  library(fields)
  library(rworldmap)

# Load data
  load('C:/Users/Clara PERON/Documents/PELAGIC2/3-Marine Mammals/Risk_analysis/data/threat_distribution/fishing/Watson_2017/data/Catch_final.Rdata')

# 1) Sum catches all gears and all types (Large-scale, small-scale and IUU) per year
  catchAll <- ddply(catch, .(Year, Seq, Lon, Lat), summarise, 
                  CR = sum(Sum_LSF_CR, na.rm=T) + sum(Sum_SSF_CR, na.rm=T) + sum(Sum_IUU_CR, na.rm=T))
  str(catchAll)
  summary(catchAll)
  
  # Remove Seq
  catchAll$Seq <- NULL

  # Create a geographic grid  
  lon <- seq(-179.75, 179.75, 0.5)
  lat <- seq(-89.75, 90, 0.5)
  grid <- expand.grid(lon, lat)
  names(grid) <- c('Lon', 'Lat')
  grid <- as.data.frame(grid)
  grid$cell <- 1:nrow(grid)

  # Match catch dataset and geographic grid
  catchAll1 <- merge(catchAll, grid, by=c('Lon', 'Lat'), all.y=T)

  # Create a rasterstack
  # Initialize the rasterstack with the first year
    i=2010
    sub <- catchAll[catchAll$Year==i,]
    sub <- merge(sub, grid, by=c('Lon', 'Lat'), all.y=T)
    sub$Year <- ifelse(is.na(sub$Year)==T, i, sub$Year)
    print(dim(sub))
    r <- rasterFromXYZ(sub[, c('Lon', 'Lat', 'CR')])
    plot(r, main=i)
    rs <- stack(r)
  
  # Increment the rasterstack for the following years  
  for(i in seq(2011, 2014,1)){
    print(i)
    sub <- catchAll[catchAll$Year==i,]
    sub <- merge(sub, grid, by=c('Lon', 'Lat'), all.y=T)
    sub$Year <- ifelse(is.na(sub$Year)==T, i, sub$Year)
    print(dim(sub))
    r <- rasterFromXYZ(sub[, c('Lon', 'Lat', 'CR')])
    plot(r, main=i, zlim=c(0,7))
    rs <- stack(rs, r)
    
  }

# Save the object as a rasterstack with years in the 3 dimension    
  save(rs, file='C:/Users/Clara PERON/Documents/PELAGIC2/3-Marine Mammals/Risk_analysis/data/threat_distribution/fishing/Watson_2017/raster_files/stack_all_by_year.Rdata')

# Map  
  plot(log(rs+1))

## 2) Average catch_rate over the 5 year period
  catch_mu <- mean(rs)
  plot(log(catch_mu+1), col=tim.colors(100))
  
  save(catch_mu, file='C:/Users/Clara PERON/Documents/PELAGIC2/3-Marine Mammals/Risk_analysis/data/threat_distribution/fishing/Watson_2017/raster_files/stack_all_avg5years.Rdata')

  
