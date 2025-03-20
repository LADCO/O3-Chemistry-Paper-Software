############################################################################################
#                                                                                          #
#                     TROPOMI NO2 COMPOSITES FROM DAN GOLDBERG                             #
#                       FOR WEEKDAY/WEEKEND AND TRENDS PAPER                               #
#                                                                                          #
############################################################################################


require("tidyverse")
require("ggplot2")
require("openair")
library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)
library(sf)
library(sp)
library(viridis)
library(colorRamps)
require("geosphere")
require("cowplot")



## TROPOMI NO2 MAPS FROM DAN GOLDBERG #########################

ncfile = ncdf4::nc_open('C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/Goldberg TROPOMI maps/Tropomi_NO2_V2.4.griddedon0.01grid_May2018-Apr2023_QA75.ncf')
names(ncfile$var)

#extract needed data from netcdf file
lon <- ncvar_get(ncfile, varid = "lon")
lat <- ncvar_get(ncfile, varid = "lat")

NO2.array <- ncvar_get(ncfile, "NO2") #convert to an array
dim(NO2.array) #check the dimensions
# ncatt_get(ncfile, "NO2", "_FillValue") #check fill value for missing data - don't think there is any (since it's a multi-year average)
nc_close(ncfile)

#Convert back to a raster with lat/long as y/x
NO2.raster <- raster(t(NO2.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat)) #t() transposes the raster
NO2.raster <- flip(NO2.raster, direction = "y") #flips the y-axis. Apparently transposing and flipping commonly need to be done.


# input_nc = 'C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/Goldberg TROPOMI maps/Tropomi_NO2_V2.4.griddedon0.01grid_May2018-Apr2023_QA75.ncf'
# varname = 'NO2'
# NO2.raster = raster(input_nc, varname = varname)
# # NO2.raster = raster(input_nc, varname = varname, nrows=2500, ncols=5800, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
# # NO2.raster = raster(input_nc, varname = varname, band = 1, nrows=2500, ncols=5800, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))


#Crop to just LADCO region
LADCO.extent <- extent(-98,-80.25,36.5,49.75)
NO2.raster.LADCO <- crop(NO2.raster, LADCO.extent)

#Crop to just Chicago or St. Louis
Chicago.extent <- extent(-88.6,-86.4,41.0,42.8)
St.Louis.extent <- extent(-91.3,-89.45,37.7,39.5)
Chicago.raster <- crop(NO2.raster, Chicago.extent)
St.Louis.raster <- crop(NO2.raster, St.Louis.extent)

#Define base map
state.borders <- getData('GADM', country='United States', level=1)
county.borders <- getData("GADM", country = "United States", level=2)
LADCO.states <- state.borders[state.borders$NAME_1 %in% c("Wisconsin","Illinois","Indiana","Michigan","Ohio","Minnesota","Iowa","Missouri","Kentucky"),]
LADCO.counties <- county.borders[county.borders$NAME_1 %in% c("Wisconsin","Illinois","Indiana","Michigan","Ohio","Minnesota","Iowa","Missouri","Kentucky"),]


# #define cuts in color scale
# ncuts = 24
my.cutpts <- c(seq(0,8e15,by=0.1e15))
# my.brks=seq(c(seq(0,12,by=0.5))) #gives number of breakpoints
# myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.cutpts), vjust=2,hjust=0.1)


# #LADCO region
    # plot.new()
    # plot <- levelplot(NO2.raster.LADCO, margin=FALSE, col.regions=matlab.like2(80), at=my.cutpts, #cuts=ncuts, colorkey=myColorkey, pretty=T,
    #                   main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
    #   latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1))
    # png(file="Goldberg TROPOMI NO2 2018-23 - LADCO region.png", width=1400, height=1200, res=300)
    # print(plot)
    # dev.off()
#     
# #Chicago
    # plot.new()
    # plot <- levelplot(Chicago.raster, margin=FALSE, col.regions=matlab.like2(80), at=my.cutpts, #cuts=ncuts, colorkey=myColorkey, pretty=T,
    #                   main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
    #   latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
    #   latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1))
    # png(file="Goldberg TROPOMI NO2 2018-23 - Chicago.png", width=1400, height=1200, res=300)
    # print(plot)
    # dev.off()
# 
# #St. Louis
    # plot.new()
    # plot <- levelplot(St.Louis.raster, margin=FALSE, col.regions=matlab.like2(80), at=my.cutpts, #cuts=ncuts, colorkey=myColorkey, pretty=T,
    #                   main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
    #   latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
    #   latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1))
    # png(file="Goldberg TROPOMI NO2 2018-23 - St Louis.png", width=1400, height=1200, res=300)
    # print(plot)
    # dev.off()
# 
# #St. Louis - unique color scale vs Chicago
# 
# my.cutpts <- c(seq(0,5e15,by=0.1e15))
# 
#     plot.new()
#     plot <- levelplot(St.Louis.raster, margin=FALSE, col.regions=matlab.like2(50), at=my.cutpts, #cuts=ncuts, colorkey=myColorkey, pretty=T,
#                       main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
#       latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
#       latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1))
#     png(file="Goldberg TROPOMI NO2 2018-23 - St Louis-zoom color scale.png", width=1400, height=1200, res=300)
#     print(plot)
#     dev.off()


#Add monitors to maps

monitors <- read.csv("./trends over space and time/Cluster monitors lat-long-extended.csv", header = TRUE)

#Eliminate two monitors (no longer used in analysis)
monitors <- dplyr::filter(monitors, !(Site %in% c(170310037, 295100062)))

#Chicago
Chicago.mons <- dplyr::filter(monitors, Area == "Chicago")
coordinates(Chicago.mons) <- ~Longitude + Latitude

peak.NO2 <- data.frame(Longitude = -87.6714, Latitude = 41.86805) #add in location of peak NO2
coordinates(peak.NO2) <- ~Longitude + Latitude

    plot.new()
    plot.Chi <- levelplot(Chicago.raster, margin=FALSE, col.regions=matlab.like2(80), at=my.cutpts, xlab=NULL, ylab=NULL, scales = list(draw=FALSE)) + 
                      #cuts=ncuts, colorkey=myColorkey, pretty=T,
                      # main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
      latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
      latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1)) +
      latticeExtra::layer(sp.points(Chicago.mons, col = "black",
                                    pch = ifelse(Chicago.mons$Cluster == "1", 2, ifelse(Chicago.mons$Cluster == "2a", 4, ifelse(Chicago.mons$Cluster == "2b", 6,
                                          ifelse(Chicago.mons$Cluster == "3", 3, ifelse(Chicago.mons$Cluster == "4", 5, ifelse(Chicago.mons$Cluster == "5", 7,
                                          ifelse(Chicago.mons$Cluster == "6", 1, ifelse(Chicago.mons$Cluster == "7", 0,
                                          ifelse(Chicago.mons$Cluster == "8", 9, 8))))))))),
                                    cex = ifelse(Chicago.mons$cluster.analysis == "Y", 1.5, 1))) +
      latticeExtra::layer(sp.points(peak.NO2, col = "black", pch = 16, cex = 1.25))
    
    # png(file="Goldberg TROPOMI NO2 2018-23 with monitors - Chicago.png", width=1400, height=1200, res=300)
    # png(file="Goldberg TROPOMI NO2 2018-23 with monitors - Chicago-w peak NO2.png", width=1400, height=1200, res=300)
    png(file="Goldberg TROPOMI NO2 2018-23 with monitors - Chicago-w peak NO2-large point.png", width=1400, height=1000, res=300)
    print(plot.Chi)
    dev.off()

#St. Louis
St.Louis.mons <- dplyr::filter(monitors, Area == "St. Louis")
coordinates(St.Louis.mons) <- ~Longitude + Latitude

peak.NO2.Stl <- data.frame(Longitude = -90.21096, Latitude = 38.63934) #add in location of peak NO2
coordinates(peak.NO2.Stl) <- ~Longitude + Latitude
    
    plot.new()
    plot.Stl <- levelplot(St.Louis.raster, margin=FALSE, col.regions=matlab.like2(80), at=my.cutpts, xlab=NULL, ylab=NULL, scales = list(draw=FALSE),
                          colorkey=FALSE) +  
                      #cuts=ncuts, colorkey=myColorkey, pretty=T,
                      # main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
      latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
      latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1)) +
      latticeExtra::layer(sp.points(St.Louis.mons, col = "black",
                                    pch = ifelse(St.Louis.mons$Cluster == "1", 6, ifelse(St.Louis.mons$Cluster == "2", 2, ifelse(St.Louis.mons$Cluster == "3", 3,
                                          ifelse(St.Louis.mons$Cluster == "4", 4, ifelse(St.Louis.mons$Cluster == "5", 5, ifelse(St.Louis.mons$Cluster == "6", 7,
                                          ifelse(St.Louis.mons$Cluster == "7", 0, ifelse(St.Louis.mons$Cluster == "8", 8, 1)))))))),
                                    cex = ifelse(St.Louis.mons$cluster.analysis == "Y", 1.5, 1))) +
      latticeExtra::layer(sp.points(peak.NO2.Stl, col = "black", pch = 16, cex = 1.5))
    # png(file="Goldberg TROPOMI NO2 2018-23 with monitors - St Louis.png", width=1120, height=1200, res=300)
    # png(file="Goldberg TROPOMI NO2 2018-23 with monitors - St Louis-w peak NO2.png", width=1120, height=1200, res=300)
    png(file="Goldberg TROPOMI NO2 2018-23 with monitors - St Louis-w peak NO2-large point.png", width=1120, height=1200, res=300)
    print(plot.Stl)
    dev.off()
    
# # Combine and save plots - didn't work well so assemble manually from above plots
#     
#     Chi.Stl.maps <- plot_grid(plot.Stl, plot.Chi, nrow=1, rel_widths = c(1,1.4), labels = c("(a)","(b)"), label_x = 0.08, label_y = 0.9, align = "h")
#     # ggsave(filename = "St Louis-Chicago maps.png", plot=Chi.Stl.maps, width = 8, height = 3.25)
#     ggsave(filename = "St Louis-Chicago maps- w peak NO2.png", plot=Chi.Stl.maps, width = 8, height = 3.25)
    
    
 #St. Louis - unique color scale vs Chicago

# my.cutpts <- c(seq(0,5e15,by=0.1e15))
# 
#     plot.new()
#     plot <- levelplot(St.Louis.raster, margin=FALSE, col.regions=matlab.like2(50), at=my.cutpts, #cuts=ncuts, colorkey=myColorkey, pretty=T,
#                       main=list("TROPOMI NO2 May 2018-April 2023 \nGoldberg data")) +
#       latticeExtra::layer(sp.polygons(LADCO.counties, col="gray40", lwd=0.5, cex=1)) +
#       latticeExtra::layer(sp.polygons(LADCO.states, col="black", lwd=0.5, cex=1)) +
#       latticeExtra::layer(sp.points(St.Louis.mons, cex = 1, col = "black",
#                                     pch = ifelse(St.Louis.mons$Cluster == "1", 0, ifelse(St.Louis.mons$Cluster == "2", 1, ifelse(St.Louis.mons$Cluster == "3", 2,
#                                           ifelse(St.Louis.mons$Cluster == "4", 3, ifelse(St.Louis.mons$Cluster == "5", 4, ifelse(St.Louis.mons$Cluster == "6", 5,
#                                           ifelse(St.Louis.mons$Cluster == "7", 6, ifelse(St.Louis.mons$Cluster == "8", 7, 8))))))))))
#     png(file="Goldberg TROPOMI NO2 2018-23 with monitors - St Louis-zoom color scale.png", width=1400, height=1200, res=300)
#     print(plot)
#     dev.off()

    


##Convert to a data frame

LADCO.NO2.df <- as.data.frame(NO2.raster.LADCO, xy=TRUE)

Chicago.NO2.df <- as.data.frame(Chicago.raster, xy=TRUE)
Chicago.NO2.lat <- unique(as.vector(Chicago.NO2.df$y))
Chicago.NO2.lon <- unique(as.vector(Chicago.NO2.df$x))

St.Louis.NO2.df <- as.data.frame(St.Louis.raster, xy=TRUE)  
St.Louis.NO2.lat <- unique(as.vector(St.Louis.NO2.df$y))
St.Louis.NO2.lon <- unique(as.vector(St.Louis.NO2.df$x))


#CHICAGO

#select grid cell with the maximum NO2 value - use below
Chicago.max.NO2 <- Chicago.NO2.df %>%
  dplyr::filter(layer == max(layer)) %>%
  dplyr::select(-layer)

#Add TROPOMI NO2 on to monitoring data frame

Chicago.mons.df <- monitors %>%
  dplyr::filter(Area == "Chicago") %>%
  rowwise() %>%
  dplyr::mutate(TROPOMI.lat = Chicago.NO2.lat[which.min(abs(Chicago.NO2.lat - Latitude))], #adds on closest TROPOMI lat/long
                TROPOMI.lon = Chicago.NO2.lon[which.min(abs(Chicago.NO2.lon - Longitude))]) %>%
  ungroup()

Chicago.mons.df <- left_join(Chicago.mons.df, Chicago.NO2.df, by=c("TROPOMI.lat"="y", "TROPOMI.lon"="x"))

Chicago.mons.df <- Chicago.mons.df %>%
  dplyr::rename(TROPOMI.NO2 = layer)

#Calculate distance of each monitor from the maximum NO2 location
Chicago.mons.dist <- dplyr::select(Chicago.mons.df, 1,4,5)
Chicago.mons.dist <- Chicago.mons.dist %>% 
  dplyr::mutate(km.peak.NO2 = (pmap_dbl(., ~ (distm(x = c(..3, ..2), y = c(-87.6714,41.86805), fun = distHaversine))))/1000)

Chicago.mons.df <- left_join(Chicago.mons.df, Chicago.mons.dist, by=c("Site","Latitude","Longitude"))

#Average NO2 & distance from peak NO2 by cluster
Chicago.cluster.NO2 <- Chicago.mons.df %>%
  dplyr::group_by(Area,Cluster) %>%
  dplyr::summarise(mean.TROPOMI.NO2 = mean(TROPOMI.NO2)/1e15, stdev.TROPOMI.NO2 = sd(TROPOMI.NO2)/1e15,
                   mean.km.peak = mean(km.peak.NO2), stdev.km.peak = sd(km.peak.NO2)) %>%
  dplyr::ungroup()

#Add meaningful names for clusters - based on distance from the area of peak NO2 and a direction indicator (if absent, it means monitors are distributed ~ evenly)
Chicago.mon.names <- data.frame(Cluster = c("1","2a","2b","3","4","5","6","7","8","9"), 
                                Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE"))
Chicago.cluster.NO2 <- left_join(Chicago.cluster.NO2, Chicago.mon.names, by="Cluster")

# write.csv(Chicago.cluster.NO2, "Cluster NO2 and distance - Chicago.csv", row.names = FALSE)


#ST. LOUIS

#select grid cell with the maximum NO2 value - use below
St.Louis.max.NO2 <- St.Louis.NO2.df %>%
  dplyr::filter(layer == max(layer)) %>%
  dplyr::select(-layer)

#Add TROPOMI NO2 on to monitoring data frame

St.Louis.mons.df <- monitors %>%
  dplyr::filter(Area == "St. Louis") %>%
  rowwise() %>%
  dplyr::mutate(TROPOMI.lat = St.Louis.NO2.lat[which.min(abs(St.Louis.NO2.lat - Latitude))], #adds on closest TROPOMI lat/long
                TROPOMI.lon = St.Louis.NO2.lon[which.min(abs(St.Louis.NO2.lon - Longitude))]) %>%
  ungroup()

St.Louis.mons.df <- left_join(St.Louis.mons.df, St.Louis.NO2.df, by=c("TROPOMI.lat"="y", "TROPOMI.lon"="x"))

St.Louis.mons.df <- St.Louis.mons.df %>%
  dplyr::rename(TROPOMI.NO2 = layer)

#Calculate distance of each monitor from the maximum NO2 location
St.Louis.mons.dist <- dplyr::select(St.Louis.mons.df, 1,4,5)
St.Louis.mons.dist <- St.Louis.mons.dist %>% 
  dplyr::mutate(km.peak.NO2 = (pmap_dbl(., ~ (distm(x = c(..3, ..2), y = c(-90.21096,38.63934), fun = distHaversine))))/1000)

St.Louis.mons.df <- left_join(St.Louis.mons.df, St.Louis.mons.dist, by=c("Site","Latitude","Longitude"))

#Average NO2 & distance from peak NO2 by cluster
St.Louis.cluster.NO2 <- St.Louis.mons.df %>%
  dplyr::group_by(Area,Cluster) %>%
  dplyr::summarise(mean.TROPOMI.NO2 = mean(TROPOMI.NO2)/1e15, stdev.TROPOMI.NO2 = sd(TROPOMI.NO2)/1e15,
                   mean.km.peak = mean(km.peak.NO2), stdev.km.peak = sd(km.peak.NO2)) %>%
  dplyr::ungroup()

#Add meaningful names for clusters - based on distance from the area of peak NO2 and a direction indicator (if absent, it means monitors are distributed ~ evenly)
St.Louis.mon.names <- data.frame(Cluster = c("1","2","3","4","5","6","7","8","A"), 
                                Cluster.name = c("64N","22","26N","44NW","55S*","84S","3","91N","10W"))
St.Louis.cluster.NO2 <- left_join(St.Louis.cluster.NO2, St.Louis.mon.names, by="Cluster")

# write.csv(St.Louis.cluster.NO2, "Cluster NO2 and distance - St Louis.csv", row.names = FALSE)


#PLOT NO2 VERSUS DISTANCE/CLUSTER

#Chicago
Chicago.shapes <- c(7,6,0,3,1,4,2,5,9,8) #use for manual shapes

    # a <- ggplot(data = Chicago.cluster.NO2, aes(x=mean.km.peak, y=mean.TROPOMI.NO2, pch = Cluster.name)) + geom_point(size=4, stroke = 1) +
    #   geom_errorbar(aes(ymin=mean.TROPOMI.NO2-stdev.TROPOMI.NO2, ymax=mean.TROPOMI.NO2+stdev.TROPOMI.NO2)) +
    #   geom_errorbar(aes(xmin=mean.km.peak-stdev.km.peak, xmax=mean.km.peak+stdev.km.peak)) +
    #   scale_shape_manual(values = Chicago.shapes) +
    #   xlab("Mean distance from NO2 peak (km)") + ylab("Mean TROPOMI NO2 (10^15 molec/cm2)") + ggtitle("TROPOMI NO2 vs cluster location - Chicago") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "TROPOMI NO2 vs distance from peak - Chicago.png", plot=a, width = 9, height = 6.5)

#St. Louis
St.Louis.cluster.NO2$Cluster.name <- factor(St.Louis.cluster.NO2$Cluster.name, c("3","10W","22","26N","44NW","55S*","64N","84S","91N"))
St.Louis.shapes <- c(6,8,1,2,3,4,0,5,7)
    
    # a <- ggplot(data = St.Louis.cluster.NO2, aes(x=mean.km.peak, y=mean.TROPOMI.NO2, pch = Cluster.name)) + geom_point(size=4, stroke = 1) +
    #   geom_errorbar(aes(ymin=mean.TROPOMI.NO2-stdev.TROPOMI.NO2, ymax=mean.TROPOMI.NO2+stdev.TROPOMI.NO2)) +
    #   geom_errorbar(aes(xmin=mean.km.peak-stdev.km.peak, xmax=mean.km.peak+stdev.km.peak)) +
    #   scale_shape_manual(values = St.Louis.shapes) +
    #   xlab("Mean distance from NO2 peak (km)") + ylab("Mean TROPOMI NO2 (10^15 molec/cm2)") + ggtitle("TROPOMI NO2 vs cluster location - St. Louis") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "TROPOMI NO2 vs distance from peak - St Louis.png", plot=a, width = 9, height = 6.5)
    




