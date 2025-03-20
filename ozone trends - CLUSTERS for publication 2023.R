############################################################################################
#                                                                                          #
#                         OZONE TRENDS 2001-2021 - FOR PUBLICATION                         #
#                     WITH OZONE CLUSTERS FROM WEEKDAY-WEEKEND ANALYSIS                    #
#                                       SPRING 2024                                        #
#                                                                                          #
############################################################################################

require("RColorBrewer")
require("tidyverse")
require("cowplot")
require("viridis")
require("colorRamps")


#IMPORT FOURTH HIGH VALUES AND LAT/LONG INFO, 1991-2022

DV.1991.2022 <- read.csv("./datafiles/AMP480_2088392_DV_Ozone 1991-2022.csv", header=TRUE)
DV.1989.1990 <- read.csv("./datafiles/AMP480_2088444_DV_Ozone 1989-90.csv", header=TRUE)
DV.1987.1988 <- read.csv("./datafiles/AMP480_2088633_DV_Ozone 1987-88.csv", header=TRUE)
DV.1987.22 <- rbind(DV.1987.1988, DV.1989.1990, DV.1991.2022)

rm("DV.1991.2022","DV.1989.1990","DV.1987.1988")

# #remove Missouri & Kentucky monitors away from shared areas with R5 (St. Louis, Louisville, and Cincinnati) - added in outlying Louisville counties with monitors
# R5.87.22 <- filter(DV.1987.22, STATE_CODE %in% c("17","18","26","27","39","55") |
#                      (STATE_CODE=="29" & COUNTY_CODE %in% c(183,189,510)) |
#                      (STATE_CODE=="21" & COUNTY_CODE %in% c(29,111,185,15,37,117,93,229,163,27,123,...)))

R5.87.22 <- DV.1987.22

R5.87.22$COUNTY_CODE <- str_pad(R5.87.22$COUNTY_CODE, 3, pad = "0")
R5.87.22$SITE_ID <- str_pad(R5.87.22$SITE_ID, 4, pad = "0")
R5.87.22$Site <- paste(R5.87.22$STATE_CODE, R5.87.22$COUNTY_CODE, R5.87.22$SITE_ID, sep="")
R5.87.22 <- R5.87.22[,c(59,1,2,4,7,13,14,51,30,31,52,53)]
R5.87.22 <- dplyr::rename(R5.87.22, State=STATE_CODE, County=COUNTY_CODE, Monitor=SITE_ID, Year=DV_YEAR,
                           Latitude=LATITUDE, Longitude=LONGITUDE, 
                           DV.complete=DV_PERCENT_COMPLETE, Fourth.high=DV_YEAR_4TH_MAX, Fourth.valid=DV_YEAR_CRITERIA_IND,
                           DV=DESIGN_VALUE, DV.valid=DV_VALIDITY_IND)
#select just valid fourth high values
R5.4th.87.22 <- R5.87.22 %>%
  dplyr::mutate(Fourth.high = Fourth.high * 1000, Fourth.high = ifelse(Fourth.valid=="N", NA, Fourth.high)) %>%
  dplyr::select(-DV.complete,-Fourth.valid,-DV,-DV.valid) %>%
  dplyr::mutate(County = as.numeric(County)) %>%
  dplyr::filter(!is.na(Fourth.high))


## CALCULATE 4TH HIGHS FOR MONITORS WITH DATA MISSING FROM DESIGN VALUE REPORTS IN AQS
#CASTNET monitors (ending in -9991) don't have 4th highs calculated for early years (prior to 2011 or so). This includes 212299991 (Louisville Mackville), 261619991 (Detroit Dexter), 390179991 (Cincinnati Oxford), and 171199991 (St. Louis Alhambra)
#Jardine (170310072) doesn't have any 4th highs in the report. Greg Boutelle (IEPA) says it's a "source monitor", and they don't calculate design values for source monitors

MDA8.missing.mons <- read.csv("AMP350MX_2138026-missing monitors MDA8s 1987-2022.csv", header=TRUE)

MDA8.missing.mons <- MDA8.missing.mons %>%
  dplyr::filter(RAW.DATA.TYPE...2 == 2) %>%
  dplyr::select(2:4,6,17,18) %>%
  dplyr::mutate(COUNTY_CODE = str_pad(COUNTY_CODE, 3, pad = "0"), SITE_ID = str_pad(SITE_ID, 4, pad = "0"), Site = paste0(STATE_CODE, COUNTY_CODE, SITE_ID),
                SAMPLE_DATE = as.Date(as.character(SAMPLE_DATE), "%Y%m%d"), year = format(SAMPLE_DATE, "%Y"), month = format(SAMPLE_DATE, "%m"),
                VALUE_0 = as.numeric(VALUE_0)*1000) %>%
  dplyr::rename(State=STATE_CODE, County=COUNTY_CODE, Monitor=SITE_ID, Date = SAMPLE_DATE, MDA8 = VALUE_0) %>%
  # dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  dplyr::mutate(Site=as.numeric(Site)) %>%
  dplyr::group_by(Site,State,County,Monitor,Date,year,month) %>%
  dplyr::filter(row_number(POC)==1) %>% #selects lowest-POC monitor 
  dplyr::ungroup() %>%
  dplyr::group_by(Site,State,County,Monitor,year) %>%
  dplyr::mutate(n.days = length(MDA8)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n.days >= 204) #select only years with at least 204 days of data (204 = 75% of the days from May to September)

#Calculate fourth highest values
fourth.missing.mons <- MDA8.missing.mons %>%
  dplyr::group_by(Site,State,County,Monitor,year) %>%
  dplyr::filter(row_number(desc(MDA8))==4) %>%
  dplyr::ungroup() %>%
  dplyr::select(Site,State,County,Monitor,year,MDA8) %>%
  dplyr::rename(Fourth.high = MDA8, Year = year)

#add in lat/long
mon.dist.center <- read.csv("Ozone monitor sites 1987-2022-city centers-lake dist.csv", header=TRUE)
Mackville <- data.frame(Site=212299991, Latitude=37.704600, Longitude=-85.048500) #Mackville's lat/long missing from above file

mon.dist.center <- mon.dist.center %>%
  dplyr::select(2:4) %>%
  bind_rows(Mackville)

fourth.missing.mons <- left_join(fourth.missing.mons, mon.dist.center, by="Site")
fourth.missing.mons <- fourth.missing.mons %>%
  dplyr::select(1:5,7,8,6) %>%
  dplyr::mutate(Site = as.character(Site), County = as.numeric(County), Year = as.numeric(Year)) %>%
  dplyr::filter(!(Monitor == "9991" & Year >= 2012) & !(Monitor == "9991" & State %in% c(21,39,17) & Year == 2011))

R5.4th.87.22 <- bind_rows(R5.4th.87.22, fourth.missing.mons)


# #Export just lat/long and site number to use in ArcGIS to determine distance from city centers & from lakeshores
# #(Have to redo this with additional years - adding 1987-1990, and 2022)
# site.lat.long <- R5.4th.87.22 %>%
#   dplyr::select(1,6,7) %>%
#   distinct()
# 
# # write.csv(site.lat.long, "Ozone monitor sites 1987-2022.csv", row.names = FALSE)


#read in county/NAA connections
NAA.county.cross <- read.csv("NAA area counties-LADCO-updated.csv", header=TRUE)
NAA.county.cross <- NAA.county.cross %>%
  dplyr::rename(State.Name = st_abbr, State = fips_state, County = fips_cnty, County.Name = countyname) %>%
  dplyr::select(-NAA.2008) #%>%
  #dplyr::filter(NAA.2015 != "")

MDA8.annual <- full_join(R5.4th.87.22, NAA.county.cross, by=c("State", "County"))
MDA8.annual <- mutate(MDA8.annual, Site = as.numeric(Site))

# MDA8.mon.list <- MDA8.annual %>%
#   select(1,4,10:12) %>%
#   distinct()


#use new groupings of years (5-year means starting 1987 & ending in 2021)
MDA8.annual <- MDA8.annual %>%
  dplyr::filter(Year != 2022) %>%
  dplyr::mutate(yr.bin = ifelse(Year<=1991, "1987-91", ifelse(Year<=1996, "1992-96", ifelse(Year<=2001, "1997-01",
                                ifelse(Year<=2006, "2002-06", ifelse(Year<=2011, "2007-11", ifelse(Year<=2016, "2012-16", "2017-21")))))))


#Only look at monitors with at least five years of data
long.mons <- MDA8.annual %>%
  dplyr::group_by(Site) %>%
  dplyr::summarize(n.years = length(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n.years >= 5) %>%
  dplyr::select(-n.years)
  
MDA8.annual.long.mons <- inner_join(long.mons, MDA8.annual, by=c("Site"))

# #Determine first and last year of record - for table in paper
# yrs.operating <- MDA8.annual.long.mons %>%
#   dplyr::filter(State == 17 | State == 29 | (State == 55 & County == 59) | State == 18 & County %in% c(89,91,127)) %>%
#   dplyr::group_by(Site,State,County) %>%
#   dplyr::summarise(start = min(Year), end = max(Year)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(yrs.operating = paste0(start,"-",end)) %>%
#   dplyr::select(1,6)
# write.table(yrs.operating, "clipboard", sep="\t", row.names=FALSE)


#Calculate 5-year averages

MDA8.5yr.avgs.by.mon <- MDA8.annual.long.mons %>%
  dplyr::group_by(Site, State.Name, County.Name, NAA.2015, yr.bin, Latitude, Longitude) %>%
  dplyr::summarise(mean.4th = mean(Fourth.high, na.rm = TRUE), stdev = sd(Fourth.high, na.rm = TRUE), n.years = n_distinct(Year, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n.years >1) %>%
  dplyr::mutate(mean.4th = ifelse(is.nan(mean.4th), NA, mean.4th))

#Export list of monitors to link to clusters
monitor.list <- MDA8.5yr.avgs.by.mon %>%
  dplyr::select(1:4) %>%
  dplyr::distinct() %>%
  dplyr::arrange(NAA.2015,State.Name,Site)

# write.table(monitor.list, "clipboard", sep="\t", row.names = FALSE)


#Import updated monitor-cluster links and add on
#Determined by comparing geography, W-W analysis differences (where available), and mean 4th high ozone trends.

mon.clusters <- read.csv("Trends monitors by clusters.csv", header=TRUE)

MDA8.5yr.avgs.by.mon <- left_join(MDA8.5yr.avgs.by.mon, mon.clusters, by=c("Site","State.Name","County.Name","NAA.2015"))

## DROP TWO MONITORS AND ALL 1987-91 DATA - THESE MONITORS HAVE <5 YEARS OF DATA WHEN YOU DROP THOSE YEARS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MDA8.5yr.avgs.by.mon <- MDA8.5yr.avgs.by.mon %>%
  dplyr::filter(!(Site %in% c(170310037, 295100062)) & yr.bin != "1987-91")

MDA8.5yr.clusters <- MDA8.5yr.avgs.by.mon %>%
  dplyr::filter(!is.na(Area) & Cluster != "x") %>% #remove monitors I didn't put in a cluster
  dplyr::group_by(Area,Cluster,yr.bin) %>%
  dplyr::summarise(mean.4th = mean(mean.4th)) %>%
  dplyr::ungroup()

#Export monitor/cluster/lat/long for mapping & satellite NO2 determination
cluster.mon.lat.long <- MDA8.5yr.avgs.by.mon %>%
  dplyr::filter(!is.na(Area) & Cluster != "x") %>% #remove monitors I didn't put in a cluster
  dplyr::select(1,2,3,6,7,11,12,13) %>%
  dplyr::distinct()
# write.csv(cluster.mon.lat.long, "Cluster monitors lat-long.csv", row.names = FALSE)




## LOOK AT ALL MONITORS WITHIN 100 KM OF CITY CENTERS 

#Plot by cluster vs year

city.list <- unique(MDA8.5yr.clusters$Area)

    # for(i in city.list)
    # {
    #   NAA.subset <- dplyr::filter(MDA8.5yr.clusters, Area == i)
    # 
    #        a <- ggplot(data = NAA.subset, aes(x = yr.bin, y = mean.4th, group=Cluster, color=Cluster)) +
    #         geom_point(size = 1, alpha=0.2) + geom_line(size=1) +
    #         xlab(NULL) + ylab("Mean Fourth High (ppb)") + ggtitle(paste("Ozone Trends by Cluster (Fourth high) - ", i)) +
    #         scale_color_brewer(palette = "Spectral") +
    #         # ylim(56,97) +
    #         theme(axis.text = element_text(size = 14, color = "black"),
    #               axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #               axis.title = element_text(size = 16),
    #               legend.text = element_text(size = 12),
    #               # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #               # legend.title = element_text(size = 14),
    #               plot.title = element_text(size = 20, hjust = 0))
    # 
    #       ggsave(filename = paste("Ozone v year group by cluster - ", i, ".png", sep =""), plot = a, width = 7, height =5)
    #   }


#FOR CHICAGO AND ST. LOUIS: PLOT WITH NEW CLUSTER NAMES (MEAN DISTANCE FROM PEAK NO2 AND DIRECTION)

#CHICAGO
Chicago.cluster.names <- data.frame(Cluster = c("1","2a","2b","3","4","5","6","7","8","9"), 
                                Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE"),
                                Distance = c(22,40,66,40,47,72,19,13,84,74),
                                Label.Dist = c(22.5,38,66,42,47,71,18.5,13,84,75))
Chicago.clusters <- dplyr::filter(MDA8.5yr.clusters, Area == "Chicago")
Chicago.clusters <- left_join(Chicago.clusters, Chicago.cluster.names, by="Cluster")

#versus year group
    #  a <- ggplot(data = Chicago.clusters, aes(x = yr.bin, y = mean.4th, group=Cluster.name, color=Cluster.name)) +
    #   geom_point(size = 1, alpha=0.2) + geom_line(size=1) +
    #   xlab(NULL) + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - Chicago") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # ggsave(filename = "Ozone v year group by cluster name - Chicago.png", plot = a, width = 7, height =5)

#Versus year group (sep. lakeshore from inland)
    #  a <- ggplot(data = Chicago.clusters, aes(x = yr.bin, y = mean.4th, color=Cluster.name, linetype=mon.type, group=interaction(Cluster.name,mon.type))) +
    #   geom_point(size = 1, alpha=0.2) + geom_line(size=1) +
    #   xlab(NULL) + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - Chicago") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # ggsave(filename = "Ozone v year group by cluster name - lake v inland - Chicago.png", plot = a, width = 7, height =5)

    
#versus mean distance from peak NO2
    # a <- ggplot() +
    #   geom_point(data = Chicago.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), size = 2) + #geom_line(size=1) +
    #   geom_smooth(data = Chicago.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), method = "loess", se = FALSE) +
    #   xlab("Mean Distance from NO2 Peak (km)") + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - Chicago") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # a <- a + geom_text(data = Chicago.cluster.names, aes(x = Label.Dist, y = 57, label = Cluster.name), angle = 90, hjust=0)
    # ggsave(filename = "Ozone v distance by cluster name - Chicago.png", plot = a, width = 7, height =5)

    
#Separating lakeshore from non-lakeshore monitors
    
Chicago.clusters <- Chicago.clusters %>%
  dplyr::mutate(mon.type = ifelse(Cluster.name %in% c("66SE*","40SE*","72N*","19*"), "Lakeshore", "Inland"))
Chicago.inland <- dplyr::filter(Chicago.clusters, mon.type == "Inland")

    # a <- ggplot() +
    #   geom_point(data = Chicago.clusters, aes(x = Distance, y = mean.4th, color=yr.bin, shape=mon.type, group=interaction(yr.bin,mon.type)), size = 2) +
    #   geom_line(data = Chicago.inland, aes(x = Distance, y = mean.4th, color=yr.bin, group=yr.bin), size=1) +
    #   # geom_smooth(data = Chicago.inland, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), method = "loess", se = FALSE) +
    #   xlab("Mean Distance from NO2 Peak (km)") + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - Chicago") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # a <- a + geom_text(data = Chicago.cluster.names, aes(x = Label.Dist, y = 57, label = Cluster.name), angle = 90, hjust=0)
    # ggsave(filename = "Ozone v distance by cluster name - lake v inland - Chicago.png", plot = a, width = 7, height =5)


#ST. LOUIS
St.Louis.cluster.names <- data.frame(Cluster = c("1","2","3","4","5","6","7","8","A"), 
                                     Cluster.name = c("64N","22","26N","44NW","55S","84S","3","91N","10W"),
                                    Distance = c(64,22,26,44,55,84,3,91,10))
St.Louis.clusters <- dplyr::filter(MDA8.5yr.clusters, Area == "St. Louis")
St.Louis.clusters <- left_join(St.Louis.clusters, St.Louis.cluster.names, by="Cluster")
St.Louis.clusters$Cluster.name <- factor(St.Louis.clusters$Cluster.name, levels = c("3","10W","22","26N","44NW","55S","64N","84S","91N"))

#versus year group
    #  a <- ggplot(data = St.Louis.clusters, aes(x = yr.bin, y = mean.4th, group=Cluster.name, color=Cluster.name)) +
    #   geom_point(size = 1, alpha=0.2) + geom_line(size=1) +
    #   xlab(NULL) + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - St. Louis") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # ggsave(filename = "Ozone v year group by cluster name - St Louis.png", plot = a, width = 7, height =5)


#versus mean distance from peak NO2
    # a <- ggplot() +
    #   geom_point(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), size = 2) + 
    #   geom_line(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), size=1) +
    #   # geom_smooth(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), method = "loess", se = FALSE) +
    #   xlab("Mean Distance from NO2 Peak (km)") + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - St. Louis") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # a <- a + geom_text(data = St.Louis.cluster.names, aes(x = Distance, y = 57, label = Cluster.name), angle = 90, hjust=0)
    # ggsave(filename = "Ozone v distance by cluster name - St Louis.png", plot = a, width = 7, height =5)


#Separate cluster 55S out - doesn't follow pattern of other monitors. (Both sites are nearby large coal-fired EGUs, and one is very near a silica mining operation. Suspect titration from NOx emissions from these sources)

St.Louis.clusters <- St.Louis.clusters %>%
  dplyr::mutate(mon.type = ifelse(Cluster.name != "55S", "Most", "55S*"),
                Cluster.name = case_when(Cluster.name == "55S" ~ "55S*",
                                         Cluster.name != "55S" ~ Cluster.name))
St.Louis.most <- St.Louis.clusters %>%
  dplyr::filter(Cluster.name != "55S*")
St.Louis.clusters$mon.type <- factor(St.Louis.clusters$mon.type, levels = c("Most","55S*"))

#versus year group
    #  a <- ggplot(data = St.Louis.clusters, aes(x = yr.bin, y = mean.4th, linetype=mon.type, group=interaction(mon.type,Cluster.name), color=Cluster.name)) +
    #   geom_point(size = 1, alpha=0.2) + geom_line(size=1) +
    #   xlab(NULL) + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - St. Louis") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # ggsave(filename = "Ozone v year group by cluster name - sep 55S - St Louis.png", plot = a, width = 7, height =5)


#versus mean distance from peak NO2
    # a <- ggplot() +
    #   geom_point(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, shape = mon.type, group=interaction(mon.type,yr.bin), color=yr.bin), size = 2) +
    #   geom_line(data = St.Louis.most, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), size=1) +
    #   # geom_smooth(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), method = "loess", se = FALSE) +
    #   xlab("Mean Distance from NO2 Peak (km)") + ylab("Mean Fourth High (ppb)") + ggtitle("Ozone Trends by Cluster (Fourth high) - St. Louis") +
    #   scale_color_brewer(palette = "Spectral") +
    #   # ylim(56,97) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         # legend.title = element_blank(), legend.position = c(0.97,0.97), legend.justification = c(1,1),
    #         # legend.title = element_text(size = 14),
    #         plot.title = element_text(size = 20, hjust = 0))
    # a <- a + geom_text(data = St.Louis.cluster.names, aes(x = Distance, y = 57, label = Cluster.name), angle = 90, hjust=0)
    # ggsave(filename = "Ozone v distance by cluster name - sep 55S - St Louis.png", plot = a, width = 7, height =5)


# ST. LOUIS AND CHICAGO PLOTS - COMBINED FOR PUBLICATION

#Make manual color scales from "Spectral" scale - to make a few lines more visible
# brewer.pal(9, "Spectral")
St.Louis.pal <- c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#FCF45D","#C2E31E","#ABDDA4","#66C2A5","#3288BD") #make two middle colors darker
# brewer.pal(10, "Spectral")
Chicago.pal <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#C3DB42", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")


St.Louis.clusters$yr.bin <- factor(St.Louis.clusters$yr.bin, c("2017-21","2012-16","2007-11","2002-06","1997-01","1992-96"))
Chicago.clusters$yr.bin <- factor(Chicago.clusters$yr.bin, c("2017-21","2012-16","2007-11","2002-06","1997-01","1992-96"))
St.Louis.clusters$Cluster.name <- factor(St.Louis.clusters$Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N"))

# write.csv(St.Louis.clusters, "St Louis cluster mean MDA8 trends.csv", row.names = FALSE)
# write.csv(Chicago.clusters, "Chicago cluster mean MDA8 trends.csv", row.names = FALSE)


#St. Louis vs year group
    a <- ggplot(data = St.Louis.clusters, aes(x = yr.bin, y = mean.4th, color=Cluster.name, linetype=mon.type, group=interaction(Cluster.name,mon.type))) +
      geom_line(linewidth=1) + #geom_point(size = 1, alpha=0.2) + 
      xlab(NULL) + ylab("Mean Fourth High (ppb)") + 
      annotate("text", x= 1, y=91, label="St. Louis", size = 6, hjust=0, vjust=0) +
      scale_color_manual(values = St.Louis.pal) +
      # scale_color_brewer(palette = "Spectral") +
      scale_y_continuous(limits = c(55,93), breaks = seq(55,90, by=5)) +
      # ylim(56,97) +
      guides(color = guide_legend("legend title"), linetype = guide_legend("legend title")) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.spacing.y = unit(0, "cm"),
            legend.title = element_blank(),
            # legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
#St. Louis vs distance
St.Louis.clusters$yr.bin <- factor(St.Louis.clusters$yr.bin, c("1987-91","1992-96","1997-01","2002-06","2007-11","2012-16","2017-21"))
    
    b <- ggplot() +
      geom_point(data = St.Louis.clusters, aes(x = Distance, y = mean.4th, shape = mon.type, group=interaction(mon.type,yr.bin), color=yr.bin), size = 2) +
      geom_line(data = St.Louis.most, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), linewidth=1) +
      ylab("Mean Fourth High (ppb)") + labs(x = expression("Mean Distance from NO"[2]*" Peak (km)")) +
      annotate("text", x= 1, y=91, label="St. Louis", size = 6, hjust=1, vjust=0) +
      scale_color_viridis(option = "D", discrete = TRUE) +
      scale_y_continuous(limits = c(55,93), breaks = seq(55,90, by=5)) + 
      scale_shape_manual(values = c(NA, 17)) + #uses blank symbol for most clusters
      scale_x_reverse(limits = c(91,0), breaks = seq(0,80,by=20)) +
      guides(color = guide_legend(override.aes = list(shape = NA))) + #removes shape from legend
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.title = element_blank(),
            # legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    b <- b + geom_text(data = St.Louis.cluster.names, aes(x = Distance, y = 55, label = Cluster.name), angle = 90, hjust=0, size = 3)

#Chicago vs year group
    c <- ggplot(data = Chicago.clusters, aes(x = yr.bin, y = mean.4th, color=Cluster.name, linetype=mon.type, group=interaction(Cluster.name,mon.type))) +
      geom_line(linewidth=1) + #geom_point(size = 1, alpha=0.2) + 
      xlab(NULL) + ylab("Mean Fourth High (ppb)") + 
      annotate("text", x= 1, y=91, label="Chicago", size = 6, hjust=0, vjust=0) +
      # scale_color_brewer(palette = "Spectral") +
      scale_color_manual(values = Chicago.pal) +
      scale_y_continuous(limits = c(55,93), breaks = seq(55,90, by=5)) +
      # ylim(56,97) +
      # guides(color = "none", linetype = "none") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.title = element_blank(),
            legend.spacing.y = unit(0, "cm"),
            # legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))

#Chicago vs distance
Chicago.clusters$yr.bin <- factor(Chicago.clusters$yr.bin, c("1987-91","1992-96","1997-01","2002-06","2007-11","2012-16","2017-21"))
    
    d <- ggplot() +
      geom_point(data = Chicago.clusters, aes(x = Distance, y = mean.4th, shape = mon.type, group=interaction(mon.type,yr.bin), color=yr.bin), size = 2) +
      geom_line(data = Chicago.inland, aes(x = Distance, y = mean.4th, group=yr.bin, color=yr.bin), linewidth=1) +
      ylab("Mean Fourth High (ppb)") + labs(x = expression("Mean Distance from NO"[2]*" Peak (km)")) +
      annotate("text", x= 1, y=91, label="Chicago", size = 6, hjust=1, vjust=0) +
      scale_color_viridis(option = "D", discrete = TRUE) +
      scale_y_continuous(limits = c(55,93), breaks = seq(55,90, by=5)) +
      scale_shape_manual(values = c(NA, 17)) + #uses blank symbol for most clusters
      guides(color = guide_legend(override.aes = list(shape = NA))) + #removes shape from colorlegend
      scale_x_reverse(limits = c(91,0), breaks = seq(0,80,by=20)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.title = element_blank(),
            # legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    d <- d + geom_text(data = Chicago.cluster.names, aes(x = Label.Dist, y = 55, label = Cluster.name), angle = 90, hjust=0, size = 3)

# Combine and save plots
    
    Stl.Chi.trends.plot <- plot_grid(a,b,c,d, nrow=2, rel_widths = c(1.04,1), labels = c("(a)","(b)","(c)","(d)"), label_x = -0.025, align = "hv", axis="b")
    ggsave(filename = "St Louis-Chicago trends plots - 1992-21.png", plot=Stl.Chi.trends.plot, width = 8.5, height = 7)
    
  
    
    
    
## MAKE PLOTS OF MEAN FOURTH HIGH VERSUS MEAN SATELLITE NO2 FOR THE MOST RECENT PERIOD (2017-21)

#Import distance & NO2 by cluster
Chicago.NO2.cluster <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/manuscript - W-W and trends/Cluster NO2 and distance - Chicago.csv", header=TRUE)
St.Louis.NO2.cluster <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/manuscript - W-W and trends/Cluster NO2 and distance - St Louis.csv", header=TRUE)
St.Louis.NO2.cluster <- St.Louis.NO2.cluster %>%
  dplyr::mutate(Cluster.name = ifelse(Cluster.name == "22", "22.", Cluster.name))

#St. Louis    
St.Louis.MDA8.NO2.recent <- St.Louis.clusters %>%
  dplyr::filter(yr.bin == "2017-21") %>%
  dplyr::mutate(Cluster.name = case_when(Cluster.name == "22" ~ "22.", #ifelse wasn't working for some reason
                                          Cluster.name != "22" ~ Cluster.name))
  # dplyr::mutate(Cluster.name2 = ifelse(Cluster.name == "22", "22.", Cluster.name)) #add "." to cluster 22 to avoid duplication with Chicago cluster 22
St.Louis.MDA8.NO2.recent <- left_join(St.Louis.MDA8.NO2.recent, St.Louis.NO2.cluster, by="Cluster.name")

St.Louis.MDA8.NO2.recent.no55S <- dplyr::filter(St.Louis.MDA8.NO2.recent, Cluster.name != "55S*") #eliminate for curve fitting

St.Louis.MDA8.NO2.recent$Cluster.name <- factor(St.Louis.MDA8.NO2.recent$Cluster.name, c("3","10W","22.","26N","44NW","55S*","64N","84S","91N"))

# St.Louis.pal.short <- c("#D53E4F","#FDAE61","#FEE08B","#FCF45D","#C2E31E","#ABDDA4","#66C2A5","#3288BD") #without 10W (not currently present)
STL.ORD.pal <- c("3"="#D53E4F","10W"="#F46D43","22."="#FDAE61","26N"="#FEE08B","44NW"="#FCF45D","55S*"="#C2E31E","64N"="#ABDDA4","84S"="#66C2A5",
                 "91N"="#3288BD",
                 "13"="#9E0142", "19*"="#D53E4F", "22"="#F46D43", "40SE*"="#FDAE61", "40SW"="#FEE08B", "47NW"="#C3DB42", "66SE*"="#ABDDA4", 
                 "72N*"="#66C2A5", "74SE"="#3288BD", "84SW"="#5E4FA2")
STL.ORD.shapes <- c("3"=15,"10W"=15,"22."=15,"26N"=15,"44NW"=15,"55S*"=15,"64N"=15,"84S"=15,"91N"=15,
                    "13"=17, "19*"=17, "22"=17, "40SE*"=17, "40SW"=17, "47NW"=17, "66SE*"=17, 
                    "72N*"=17, "74SE"=17, "84SW"=17)

    # a <- ggplot(data = St.Louis.MDA8.NO2.recent, aes(x = mean.TROPOMI.NO2, y = mean.4th)) +
    #   geom_point(size = 2, aes(color=Cluster.name, group=Cluster.name)) +
    #   ylab("Mean Fourth High Ozone (ppb)") + labs(y = expression("Mean TROPOMI NO"[2]*" (10"^15*" molec/cm"^2*")")) +
    #   annotate("text", x= 2, y=75, label="St. Louis", size = 6, hjust=0, vjust=0) +
    #   scale_color_manual(values = St.Louis.pal.short) +
    #   scale_y_continuous(limits = c(63,77), breaks = seq(64,76, by=2)) + scale_x_continuous(limits = c(1.6,6.4), breaks = seq(2,6, by=1)) +
    #   guides(color = guide_legend("legend title"), linetype = guide_legend("legend title")) +
    #   theme(axis.text = element_text(size = 10, color = "black"),
    #         axis.text.x = element_text(size = 10),
    #         axis.title.y = element_text(size = 12),
    #         legend.text = element_text(size = 8),
    #         legend.spacing.y = unit(0, "cm"),
    #         legend.title = element_blank(),
    #         # legend.position = c(0.18,0.15),
    #         legend.key = element_blank(),
    #         # legend.background = element_rect(color = "black"),
    #         # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
    #         panel.background = element_rect(fill = "white"),
    #         axis.line = element_line(color = "black"),
    #         panel.grid.major = element_line(color = "gray90"))
    # a <- a + geom_smooth(data = St.Louis.MDA8.NO2.recent.no55S, aes(x = mean.TROPOMI.NO2, y = mean.4th),
    #                      method="loess", formula=y~x, se=FALSE, color="darkgray", size=0.5)

#Chicago
Chicago.MDA8.NO2.recent <- Chicago.clusters %>%
  dplyr::filter(yr.bin == "2017-21")
Chicago.MDA8.NO2.recent <- left_join(Chicago.MDA8.NO2.recent, Chicago.NO2.cluster, by="Cluster")

Chicago.MDA8.NO2.recent.nolake <- dplyr::filter(Chicago.MDA8.NO2.recent, !(Cluster.name.x %in% c("19*","40SE*","66SE*","72N*"))) #eliminate for curve fitting

Chicago.MDA8.NO2.recent$Cluster.name.x <- factor(Chicago.MDA8.NO2.recent$Cluster.name.x, c("13","19*","22","40SE*","40SW","47NW","66SE*","72N*","74SE","84SW"))

    # b <- ggplot(data = Chicago.MDA8.NO2.recent, aes(x = mean.TROPOMI.NO2, y = mean.4th)) +
    #   geom_point(size = 2, aes(color=Cluster.name.x, group=Cluster.name.x)) +
    #   ylab("Mean Fourth High Ozone (ppb)") + labs(x = expression("Mean TROPOMI NO"[2]*" (10"^15*" molec/cm"^2*")")) +
    #   annotate("text", x= 2, y=75, label="Chicago", size = 6, hjust=0, vjust=0) +
    #   scale_color_manual(values = Chicago.pal) +
    #   # scale_color_brewer(palette = "Spectral") +
    #   scale_y_continuous(limits = c(63,77), breaks = seq(64,76, by=2)) + scale_x_continuous(limits = c(1.6,6.4), breaks = seq(2,6, by=1)) +
    #   guides(color = guide_legend("legend title"), linetype = guide_legend("legend title")) +
    #   theme(axis.text = element_text(size = 10, color = "black"),
    #         # axis.text.x = element_text(size = 12),
    #         axis.title = element_text(size = 12),
    #         legend.text = element_text(size = 8),
    #         legend.spacing.y = unit(0, "cm"),
    #         legend.title = element_blank(),
    #         # legend.position = c(0.18,0.15),
    #         legend.key = element_blank(),
    #         # legend.background = element_rect(color = "black"),
    #         # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
    #         panel.background = element_rect(fill = "white"),
    #         axis.line = element_line(color = "black"),
    #         panel.grid.major = element_line(color = "gray90"))
    # b <- b + geom_smooth(data = Chicago.MDA8.NO2.recent.nolake, aes(x = mean.TROPOMI.NO2, y = mean.4th),
    #                      method="loess", formula=y~x, se=FALSE, color="darkgray", size=0.5)
    
#Make combined plot
    a <- ggplot() + #geom_line(data = St.Louis.MDA8.NO2.recent.no55S, aes(x = mean.TROPOMI.NO2, y = mean.4th), color="darkgray", size=0.75) +
      geom_smooth(data = St.Louis.MDA8.NO2.recent.no55S, aes(x = mean.TROPOMI.NO2, y = mean.4th),
                         method="loess", formula=y~x, se=FALSE, span=0.90, color="darkgray", size=0.75) +
      ylab("Mean Fourth High Ozone (ppb)") + labs(x = expression("Mean TROPOMI NO"[2]*" (10"^15*" molec/cm"^2*")")) +
      annotate("text", x= 2, y=71, label="St. Louis", size = 6, hjust=0, vjust=0) +
      scale_color_manual(values = STL.ORD.pal) + scale_shape_manual(values = STL.ORD.shapes) +
      scale_y_continuous(limits = c(63,77), breaks = seq(64,76, by=2)) + scale_x_continuous(limits = c(1.6,6.4), breaks = seq(2,6, by=1)) +
      # guides(color = guide_legend("legend title"), linetype = guide_legend("legend title")) +
      guides(color = guide_legend("legend title", ncol=2), shape = guide_legend("legend title", ncol=2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.spacing.y = unit(0, "cm"),
            legend.title = element_blank(),
            # legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    a <- a + geom_point(data = St.Louis.MDA8.NO2.recent, aes(x = mean.TROPOMI.NO2, y = mean.4th, color=Cluster.name, shape=Cluster.name, group=Cluster.name), size = 2.5)
    a <- a + geom_smooth(data = Chicago.MDA8.NO2.recent.nolake, aes(x = mean.TROPOMI.NO2, y = mean.4th),
                         method="loess", formula=y~x, se=FALSE, span=0.9, color="darkgray", size=0.75)
    # a <- a + geom_line(data = Chicago.MDA8.NO2.recent.nolake, aes(x = mean.TROPOMI.NO2, y = mean.4th), color="darkgray", size=0.75)
    a <- a + 
      geom_point(data = Chicago.MDA8.NO2.recent, aes(x = mean.TROPOMI.NO2, y = mean.4th, color=Cluster.name.x, shape=Cluster.name.x, group=Cluster.name.x), size = 2.5) +
      annotate("text", x= 5, y=73, label="Chicago", size = 6, hjust=0, vjust=0)
    # ggsave(filename = "Fourth high O3 vs TROPOMI NO2 - both.png", plot = a, width = 6, height = 4.5)
    # ggsave(filename = "Fourth high O3 vs TROPOMI NO2 - both-diff shapes.png", plot = a, width = 6, height = 4.5)
    ggsave(filename = "Fourth high O3 vs TROPOMI NO2 - both-diff shapes-loess.png", plot = a, width = 6, height = 4.5)


  
# ## PLOT CENTRAL CITY CLUSTERS VERSUS MONITORED NO2 OVER TIME  
# 
# NO2.yr.groups <- read.csv("NO2 concentrations by year groups - Chicago-St Louis.csv", header = TRUE)
#     
# St.Louis.NO2.yrs <- St.Louis.clusters %>%
#   dplyr::filter(Cluster.name %in% c("3","10W","22"))
# St.Louis.NO2.yrs <- left_join(St.Louis.NO2.yrs, NO2.yr.groups, by=c("Area"="CBSA", "yr.bin"="yr.bins"))
# St.Louis.NO2.yrs <- St.Louis.NO2.yrs %>%
#   dplyr::rename(NO2.mean = area.mean) %>%
#   dplyr::mutate(Cluster.name = case_when(Cluster.name == "22" ~ "22.", #ifelse wasn't working for some reason
#                                          Cluster.name != "22" ~ Cluster.name))
# St.Louis.NO2.yrs$Cluster.name <- factor(St.Louis.NO2.yrs$Cluster.name, c("3","10W","22."))
# 
# Chicago.NO2.yrs <- Chicago.clusters %>%
#   dplyr::filter(Cluster.name %in% c("13","22"))
# Chicago.NO2.yrs <- left_join(Chicago.NO2.yrs, NO2.yr.groups, by=c("Area"="CBSA", "yr.bin"="yr.bins"))
# Chicago.NO2.yrs <- Chicago.NO2.yrs %>%
#   dplyr::rename(NO2.mean = area.mean)
# Chicago.NO2.yrs$Cluster.name <- factor(Chicago.NO2.yrs$Cluster.name, c("13","22"))
# 
# 
# #Make combined plot
#     b <- ggplot() + geom_smooth(data = St.Louis.NO2.yrs, aes(x = NO2.mean, y = mean.4th, color=Cluster.name, group=Cluster.name),
#                                 method="loess", formula=y~x, se=FALSE, size=0.75) +
#       geom_point(data = St.Louis.NO2.yrs, aes(x = NO2.mean, y = mean.4th, color=Cluster.name, group=Cluster.name), size = 2) +
#       ylab("Mean Fourth High Ozone (ppb)") + labs(x = expression("Mean Monitored NO"[2]*" by Year Group (ppb)")) +
#       annotate("text", x= 6, y=87, label="St. Louis", size = 6, hjust=0, vjust=0) +
#       scale_color_manual(values = STL.ORD.pal) +
#       scale_y_continuous(limits = c(67,88.5), breaks = seq(68,88, by=2)) + scale_x_continuous(limits = c(4.2,20), breaks = seq(6,20, by=2)) +
#       guides(color = guide_legend("legend title")) +
#       theme(axis.text = element_text(size = 10, color = "black"),
#             axis.title = element_text(size = 12),
#             legend.text = element_text(size = 8),
#             legend.spacing.y = unit(0, "cm"),
#             legend.title = element_blank(),
#             # legend.position = c(0.18,0.15),
#             legend.key = element_blank(),
#             # legend.background = element_rect(color = "black"),
#             # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
#             panel.background = element_rect(fill = "white"),
#             axis.line = element_line(color = "black"),
#             panel.grid.major = element_line(color = "gray90"))
#     # a <- a + geom_point(data = St.Louis.NO2.yrs, aes(x = NO2.mean, y = mean.4th, color=Cluster.name, group=Cluster.name), size = 2)
#     b <- b + geom_smooth(data = Chicago.NO2.yrs, aes(x = NO2.mean, y = mean.4th, color=Cluster.name, group=Cluster.name),
#                          method="loess", formula=y~x, se=FALSE, size=0.75) +
#       geom_point(data = Chicago.NO2.yrs, aes(x = NO2.mean, y = mean.4th, color=Cluster.name, group=Cluster.name), size = 2) +
#       annotate("text", x= 16, y=78, label="Chicago", size = 6, hjust=0, vjust=0)
#     ggsave(filename = "Fourth high O3 vs monitored NO2 over time - both.png", plot = b, width = 6, height = 4.5)

    
## PLOT MODEL VOCs AND NOx VERSUS DISTANCE FROM THE CITY CENTER (NOT VERSUS CLUSTER DISTANCE) #######
    
#Import 2016 (aa2a) VOC (because didn't save VOCs from abc run)

VOC.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means VOCs - all.csv", header=TRUE)
VOC.2016.model <- VOC.2016.model %>%
  dplyr::select(1:4,5)

#Import 2016 (aa2a) NOx (to match VOCs)
NO2.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means - all areas.csv", header = TRUE)
NO2.2016.model <- NO2.2016.model %>%
  dplyr::select(1:4,9)

#Combine VOC and NOx model values
VOC.NO2.2016.model <- full_join(VOC.2016.model, NO2.2016.model, by=c("exceed.day","time","x","y"))

#Import distance from city center for grid cells

grid.city.center.dist <- read.csv("LADCO 12km grid cells-distance to city center.csv", header=TRUE)
grid.city.center.dist <- grid.city.center.dist %>%
  dplyr::filter(NEAR_FID != -1) %>%
  dplyr::mutate(dist.km = NEAR_DIST / 1000, Area = ifelse(NEAR_FID == 1, "Chicago", ifelse(NEAR_FID == 7, "St. Louis", "other"))) %>%
  dplyr::select(2,3,9,8) %>%
  dplyr::filter(Area %in% c("Chicago","St. Louis"))

#add on to NOx & VOC model data
VOC.NO2.2016.model <- left_join(VOC.NO2.2016.model, grid.city.center.dist, by=c("x","y"))

CHI.STL.VOC.NO2 <- VOC.NO2.2016.model %>%
  dplyr::filter(Area %in% c("Chicago","St. Louis") & exceed.day == Area)

#calculate means by distance - daytime (9 am - 4 pm)
CHI.STL.dist.VOC.NO2 <- CHI.STL.VOC.NO2 %>%
  dplyr::filter(time != "peak.NO2") %>%
  dplyr::group_by(Area,x,y,dist.km) %>%
  dplyr::summarise(daytime.VOC = mean(VOC.2016), daytime.NO2 = mean(NO2.2016)) %>% #average building O3 and peak O3 times by monitor
  dplyr::ungroup() %>%
  dplyr::mutate(km.bins = ifelse(dist.km <10, "0-10", ifelse(dist.km <20, "10-20", ifelse(dist.km <30, "20-30", ifelse(dist.km <40, "30-40", ifelse(dist.km <50, "40-50",
                                 ifelse(dist.km <60, "50-60", ifelse(dist.km <70, "60-70", ifelse(dist.km <80, "70-80", ifelse(dist.km <90, "80-90", "90-100")))))))))) %>%
  dplyr::group_by(Area, km.bins) %>%
  dplyr::summarise(VOC = mean(daytime.VOC), NO2 = mean(daytime.NO2)) %>% #average concentrations at all monitors within each cluster
  # dplyr::summarise(VOC = mean(VOC.2016), NO2 = mean(NO2.2016)) %>% #average concentrations at all monitors within each cluster
  dplyr::ungroup() %>%
  pivot_longer(VOC:NO2, names_to = "param", values_to = "mean.day.conc") %>%
  dplyr::group_by(Area,param) %>%
  dplyr::mutate(frax.of.max = mean.day.conc/max(mean.day.conc)) %>%
  dplyr::ungroup()

CHI.STL.dist.VOC.NO2$km.bins <- factor(CHI.STL.dist.VOC.NO2$km.bins, levels = c("90-100","80-90","70-80","60-70","50-60","40-50","30-40","20-30","10-20","0-10"))

    # a <- ggplot(CHI.STL.dist.VOC.NO2, aes(x=km.bins, y=frax.of.max, color=Area, group=Area)) + facet_wrap(~param) + geom_point() +
    a <- ggplot(CHI.STL.dist.VOC.NO2, aes(x=km.bins, y=mean.day.conc, color=Area, group=Area)) + 
      facet_wrap(~param, scales = "free_y") + geom_line(size = 1) + #geom_point() +
      # scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=0.2)) + #scale_x_reverse(limits = c(100,0), breaks = seq(0,100,by=20)) +
      scale_color_manual(values = c("#E41A1C","#377EB8")) +
      geom_hline(aes(yintercept = 0), color="lightgray") +
      # labs(x=expression("Distance from city center (km)")) + ylab("Fraction of maximum concentration") +
      labs(x=expression("Distance from city center (km)")) + ylab("Mean daytime concentration (ppb)") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, vjust=0.5, angle = 90),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.background = element_rect(fill = "white"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            strip.text = element_text(size=10),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray90"))
    # ggsave(filename = "Model VOC-NOx vs distance from downtown - Chicago-St Louis.png", plot=a, width = 6, height = 3.5)
    ggsave(filename = "Model VOC-NOx vs distance from downtown-conc - Chicago-St Louis.png", plot=a, width = 6, height = 3.5)

# #calculate means by distance (peak NO2 time: )
# CHI.STL.dist.VOC.NO2.am <- CHI.STL.VOC.NO2 %>%
#   dplyr::filter(time == "peak.NO2") %>%
#   # dplyr::group_by(Area,x,y,dist.km) %>%
#   # dplyr::summarise(daytime.VOC = mean(VOC.2016), daytime.NO2 = mean(NO2.2016)) %>% #average building O3 and peak O3 times by monitor
#   # dplyr::ungroup() %>%
#   dplyr::mutate(km.bins = ifelse(dist.km <10, "0-10", ifelse(dist.km <20, "10-20", ifelse(dist.km <30, "20-30", ifelse(dist.km <40, "30-40", ifelse(dist.km <50, "40-50",
#                                  ifelse(dist.km <60, "50-60", ifelse(dist.km <70, "60-70", ifelse(dist.km <80, "70-80", ifelse(dist.km <90, "80-90", "90-100")))))))))) %>%
#   dplyr::group_by(Area, km.bins) %>%
#   dplyr::summarise(VOC = mean(VOC.2016), NO2 = mean(NO2.2016)) %>% #average concentrations at all monitors within each cluster
#   dplyr::ungroup() %>%
#   pivot_longer(VOC:NO2, names_to = "param", values_to = "mean.day.conc") %>%
#   dplyr::group_by(Area,param) %>%
#   dplyr::mutate(frax.of.max = mean.day.conc/max(mean.day.conc)) %>%
#   dplyr::ungroup()
# 
# CHI.STL.dist.VOC.NO2.am$km.bins <- factor(CHI.STL.dist.VOC.NO2.am$km.bins, levels = c("90-100","80-90","70-80","60-70","50-60","40-50","30-40","20-30","10-20","0-10"))
# 
#     a <- ggplot(CHI.STL.dist.VOC.NO2.am, aes(x=km.bins, y=frax.of.max, color=Area, group=Area)) + facet_wrap(~param) + geom_point() +
#       scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=0.2)) + #scale_x_reverse(limits = c(100,0), breaks = seq(0,100,by=20)) +
#       scale_color_manual(values = c("#E41A1C","#377EB8")) +
#       labs(x=expression("Distance from city center (km)")) + ylab("Fraction of maximum concentration") +
#       theme(axis.text = element_text(size = 10, color = "black"),
#             axis.text.x = element_text(size = 10, vjust=0.5, angle = 90),
#             axis.title = element_text(size = 12),
#             legend.text = element_text(size = 10),
#             legend.title = element_blank(),
#             legend.key = element_blank(),
#             legend.background = element_rect(fill = "white"),
#             # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
#             strip.text = element_text(size=10),
#             panel.background = element_rect(fill = "white"),
#             axis.line = element_line(color = "black"),
#             panel.grid.major = element_line(color = "gray90"),
#             panel.grid.minor = element_line(color = "gray90"))
#     ggsave(filename = "Model VOC-NOx vs distance from downtown - early am - Chicago-St Louis.png", plot=a, width = 6, height = 3.5)

    
## PLOT NOX AND VOC EMISSIONS OVER TIME FROM ILLINOIS AND MISSOURI ##########################
# PLOT TRENDS IN ANTHROPOGENIC VOC EMISSIONS (BECAUSE MONITORING DATA IS SPARSE) OVER TIME - but these are missing biogenics, which are really big
# emissions data from: https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data

VOC.NOx.emissions <- read.csv("C:/Users/afdic/OneDrive/Documents/emissions/state_tier1_caps-national emissions trends through 2021.csv", header=TRUE)

VOC.NOx.emissions.IL.MO <- VOC.NOx.emissions %>%
  dplyr::filter(Pollutant %in% c("VOC","NOX") & State %in% c("IL","MO")) %>%
  pivot_longer(emissions90:emissions21, names_to = "year", values_to = "tons.1000") %>%
  dplyr::mutate(year = as.numeric(substr(year, 10, 11))) %>%
  dplyr::mutate(year = year + ifelse(year >=90, 1900, 2000)) %>%
  dplyr::group_by(State, year, Pollutant) %>%
  dplyr::summarize(total.tons.1000 = sum(tons.1000, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1995, "1990", ifelse( year == 1996, "1996", ifelse(year <= 2001, "1997-01", ifelse(year <= 2006, "2002-06", 
                                 ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::group_by(State, Pollutant, yr.bins) %>%
  dplyr::summarize(mean.1000.tons = mean(total.tons.1000, na.rm=TRUE)) %>%
  dplyr::ungroup()
# write.table(VOC.emissions.LADCO, "clipboard", sep="\t", row.names = FALSE)

# Plot anthropogenic VOC emissions by year group
    # a <- ggplot(data=VOC.NOx.emissions.IL.MO) +
    #   geom_line(aes(x=yr.bins, y=mean.1000.tons, color=State, group=State), size=1) + facet_wrap(~Pollutant) +
    #   xlab(NULL) + ylab("Mean emissions (1000 tons)") + ggtitle(paste0("Mean Anthropogenic VOC and NOx Emissions")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,1000) +
    #   scale_color_brewer(palette = "Paired") +
    #   scale_y_continuous(breaks = seq(0,1000,by=100)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("VOC-NOx emissions means by year bins - IL-MO.png"), plot = a, width = 6, height =4)

#attempt to account for ozone season biogenic emissions - add onto anthropogenics using annual LADCO model numbers for 2016 (provided by Zac via email, 7/8/22 (IL) and 7/17/2024 (MO))
VOC.emiss.biogenic <- data.frame(State = c("IL","MO"), biogenic.1000.tons = c(422.736, 1158.094))

VOC.NOx.emissions.IL.MO <- full_join(VOC.NOx.emissions.IL.MO, VOC.emiss.biogenic, by="State")
VOC.NOx.emissions.IL.MO <- VOC.NOx.emissions.IL.MO %>%
  dplyr::rename(anthrop.1000.tons = mean.1000.tons) %>%
  dplyr::mutate(total.1000.tons = ifelse(Pollutant == "VOC", anthrop.1000.tons + biogenic.1000.tons, anthrop.1000.tons))

write.table(VOC.NOx.emissions.IL.MO, "clipboard", sep="\t", row.names = FALSE)

VOC.NOx.emissions.IL.MO$yr.bins <- factor(VOC.NOx.emissions.IL.MO$yr.bins,
                                          levels = c("2017-21","2012-16","2007-11","2002-06","1997-01","1996","1990"))

VOC.biogenic.line <- data.frame(Pollutant = "VOC",
                                State = c(rep("IL", 7),rep("MO",7)),
                                yr.bins = rep(c("2017-21","2012-16","2007-11","2002-06","1997-01","1996","1990"),2),
                                biogenic.1000.tons = c(rep(422.736, 7),rep(1158.094,7)))

#Plot total VOC emissions by year group
    b <- ggplot(data=VOC.NOx.emissions.IL.MO) +
      geom_line(aes(x=yr.bins, y=total.1000.tons, color=State, group=State), size=1) + 
      facet_wrap(~Pollutant, scales = "free_y") +
      xlab(NULL) + ylab("Total emissions (1000 tons)") + #ggtitle(paste0("Mean Total VOC and NOx Emissions")) +
      # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
      geom_hline(yintercept = 0, color="darkgray") +
      ylim(0,1800) + scale_color_manual(values = c("#E41A1C","#377EB8")) +
      # scale_color_brewer(palette = "Paired") +
      scale_y_continuous(breaks = seq(0,1800,by=200)) +
      # geom_hline(aes(yintercept = 422.736), size=0.5) + geom_hline(aes(yintercept = 1158.094), size=0.5) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, vjust=0.5, angle = 90),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.background = element_rect(fill = "white"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            strip.text = element_text(size=10),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray90"))
      # theme(axis.text = element_text(size = 14, color = "black"),
      #       axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
      #       axis.title = element_text(size = 16),
      #       legend.text = element_text(size = 12),
      #       panel.background = element_rect(fill = "white", color = "black"),
      #       legend.key=element_rect(fill="white", color="white"),
      #       panel.grid.major = element_line(color = "gray90"),
      #       panel.grid.minor = element_line(color = "gray90"),
      #       legend.title = element_blank(),
      #       plot.title = element_text(size = 18, hjust = 0))
    b <- b + geom_line(data=VOC.biogenic.line, aes(x = yr.bins, y = biogenic.1000.tons, color=State, group=State), linetype = 2)
    ggsave(filename = paste0("Total VOC-NOx emissions means by year bins.png"), plot = b, width = 6, height = 3.5)

#Combine VOC-NOx model concentration and emissions plots

    paper.precursor.plot <- cowplot::plot_grid(a, b, nrow=2, ncol=1, align = "hv")
    ggsave(filename = "NOx-VOC combined emissions v time-conc v dist.png", plot = paper.precursor.plot, 
           width = 6, height = 7)
    





# ## PLOT MODEL VOCs and NOx VERSUS CLUSTER DISTANCE #############################################
#     
# #Import 2016 (aa2a) VOC (because didn't save VOCs from abc run)
# 
# VOC.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means VOCs - all.csv", header=TRUE)
# VOC.2016.model <- VOC.2016.model %>%
#   dplyr::select(1:4,5)
# 
# #Import 2016 (aa2a) NOx (to match VOCs)
# NO2.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means - all areas.csv", header = TRUE)
# NO2.2016.model <- NO2.2016.model %>%
#   dplyr::select(1:4,9)
# 
# #Combine VOC and NOx model values
# VOC.NO2.2016.model <- full_join(VOC.2016.model, NO2.2016.model, by=c("exceed.day","time","x","y"))
# 
# #Pull in lat/long
# grid.lat.long <- read.csv("Lat-long for LADCO grid cells.csv", header=TRUE)
# 
# VOC.NO2.2016.model <- left_join(VOC.NO2.2016.model, grid.lat.long, by = c("x","y"))
# model.lat <- unique(VOC.NO2.2016.model$Latitude) #makes list of unique latitudes from the model to join to closest monitor latitude
# model.lon <- unique(VOC.NO2.2016.model$Longitude)
# 
# #Import cluster/monitor lat/long to combine with VOC data
# cluster.mon.lat.long <- read.csv("Cluster monitors lat-long.csv", header=TRUE)
# 
# #Selects just Chicago and St. Louis clusters and adds lat/long that match the values in the VOC model grids
# CHI.STL.clusters <- cluster.mon.lat.long %>%
#   dplyr::filter(Area %in% c("Chicago","St. Louis")) 
# 
# # CHI.STL.lat.long.box <- CHI.STL.clusters %>% #one-time: get estimates of boxes for two cities to narrow grid list below
# #   dplyr::group_by(Area) %>%
# #   dplyr::summarise(min.lat = min(Latitude), max.lat = max(Latitude), min.lon = min(Longitude), max.lon = max(Longitude)) %>%
# #   dplyr::ungroup()
# 
# #Narrow VOCs to just boxes around Chicago and St. Louis monitors
# CHI.STL.box.VOC.NO2 <- VOC.NO2.2016.model %>%
#   dplyr::mutate(Area = ifelse(Latitude > 41.15 & Latitude < 42.65 & Longitude > -88.35 & Longitude < -86.55, "Chicago",
#                               ifelse(Latitude > 37.80 & Latitude < 39.50 & Longitude > -90.95 & Longitude < -89.55, "St. Louis", "other"))) %>%
#   dplyr::filter(Area != "other", exceed.day == Area) #matches exceedance days (for VOCs) to the area
#   
# 
# #Determine the closest grid cell to each monitor
# require("FNN")
# mon.lat.long <- CHI.STL.clusters[,c("Latitude","Longitude")] #select just lat and long
# VOC.NO2.lat.long <- CHI.STL.box.VOC.NO2[,c("Latitude","Longitude")]
# mon.grid.matches <- knnx.index(VOC.NO2.lat.long, mon.lat.long, k=1)
# 
# 
# #Pull in model grid lat/long at monitors, merge data frames, and calculate averages by cluster
# #Combine building O3 (9-12) and peak O3 (13-16) times for a "daytime" concentration (9-16:59)
# CHI.STL.mon.VOC.NO2 <- CHI.STL.clusters
# CHI.STL.mon.VOC.NO2$grid.lat <- CHI.STL.box.VOC.NO2$Latitude[mon.grid.matches[,1]]
# CHI.STL.mon.VOC.NO2$grid.lon <- CHI.STL.box.VOC.NO2$Longitude[mon.grid.matches[,1]]
# 
# CHI.STL.mon.VOC.NO2 <- left_join(CHI.STL.mon.VOC.NO2, CHI.STL.box.VOC.NO2, by=c("grid.lat"="Latitude", "grid.lon"="Longitude", "Area"), 
#                                  relationship = "many-to-many")
# 
# CHI.STL.cluster.VOC.NO2 <- CHI.STL.mon.VOC.NO2 %>%
#   dplyr::filter(time != "peak.NO2") %>%
#   dplyr::group_by(Area,Site,Cluster) %>%
#   dplyr::summarise(daytime.VOC = mean(VOC.2016), daytime.NO2 = mean(NO2.2016)) %>% #average building O3 and peak O3 times by monitor
#   dplyr::ungroup() %>%
#   dplyr::group_by(Area, Cluster) %>%
#   dplyr::summarise(mean.day.VOC = mean(daytime.VOC), mean.day.NO2 = mean(daytime.NO2)) %>% #average concentrations at all monitors within each cluster
#   dplyr::ungroup()
# 
# #Add meaningful names for clusters - based on distance from the area of peak NO2 and a direction indicator (if absent, it means monitors are distributed ~ evenly)
# cluster.names <- data.frame(Area = c(rep("Chicago",10),rep("St. Louis",9)),
#   Cluster = c("1","2a","2b","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","A"), 
#   Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE","64N","22","26N","44NW","55S*","84S","3","91N","10W"),
#   distance = c(22,40,66,40,47,72,19,13,84,74,64,22,26,44,55,84,3,91,10))
# 
# CHI.STL.cluster.VOC.NO2 <- left_join(CHI.STL.cluster.VOC.NO2, cluster.names, by=c("Area", "Cluster"))
# 
# #calculate fraction of peak NO2 or VOC
# CHI.STL.cluster.VOC.NO2 <- CHI.STL.cluster.VOC.NO2 %>%
#   dplyr::group_by(Area) %>%
#   dplyr::mutate(frax.VOC = mean.day.VOC/max(mean.day.VOC), frax.NO2 = mean.day.NO2/max(mean.day.NO2)) %>%
#   dplyr::ungroup() 
# 
# CHI.STL.cluster.long <- CHI.STL.cluster.VOC.NO2 %>%
#   dplyr::select(1,2,5:8) %>%
#   pivot_longer(frax.VOC:frax.NO2, names_to = "param", values_to = "fraction.of.max") %>%
#   dplyr::mutate(param = ifelse(param == "frax.VOC", "VOC", "NO2"))
# 
#     # a <- ggplot(CHI.STL.cluster.long, aes(x=distance, y=fraction.of.max, color=Area, group=Area)) + facet_wrap(~param) + geom_point() +
#     #   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=0.2)) + scale_x_reverse(limits = c(100,0), breaks = seq(0,100,by=20)) +
#     #   scale_color_manual(values = c("#E41A1C","#377EB8")) +
#     #   labs(x=expression("Mean distance from NO"[2]*" peak (km)")) + ylab("Fraction of maximum concentration") +
#     #   theme(axis.text = element_text(size = 10, color = "black"),
#     #         axis.text.x = element_text(size = 10, vjust=0.5),
#     #         axis.title = element_text(size = 12),
#     #         legend.text = element_text(size = 10),
#     #         legend.title = element_blank(),
#     #         legend.key = element_blank(),
#     #         legend.background = element_rect(fill = "white"),
#     #         # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
#     #         strip.text = element_text(size=10),
#     #         panel.background = element_rect(fill = "white"),
#     #         axis.line = element_line(color = "black"),
#     #         panel.grid.major = element_line(color = "gray90"),
#     #         panel.grid.minor = element_line(color = "gray90"))
#     # ggsave(filename = "Model VOC-NOx vs distance from peak - Chicago-St Louis.png", plot=a, width = 6, height = 3.5)
    


    
    
    
    
    
## PLOT SIGNIFICANCE OF T-TEST VERSUS WEEKDAY-WEEKEND DIFFERENCE (to estimate transitional range)
STL.WW.results <- read.csv("St Louis cluster W-W diffs-means-by cluster - 1987-2021.csv", header = TRUE)
CHI.WW.results <- read.csv("Chicago cluster W-W diffs-means-by cluster - 1987-2021.csv", header = TRUE)

STL.CHI.WW.results <- bind_rows(STL.WW.results, CHI.WW.results)
STL.CHI.WW.results <- STL.CHI.WW.results %>%
  dplyr::filter(city != "")

    # a <- ggplot(data = STL.CHI.WW.results, aes(x=wd.we.diff, y=p.value, color=Cluster.name, shape=city, group=interaction(Cluster.name, city))) +
    #   geom_point() + scale_shape_manual(values = c(1,15)) + 
    #   scale_x_continuous(breaks = seq(-12,10,by=2)) + scale_y_continuous(breaks = seq(0,1,by=0.2)) +
    #   geom_vline(xintercept = 0, color = "darkgray") + geom_hline(yintercept = 0.05, color = "darkgray") +
    #   xlab("Weekday-Weekend Mean MDA8 Difference (ppb)") + ylab("p value") + 
    #   ggtitle("Significance of Welch's t-test versus W-W Differences") + 
    #   guides(col = guide_legend(ncol = 2)) + 
    #   theme(axis.text = element_text(size = 10, color = "black"),
    #         axis.title = element_text(size = 12),
    #         legend.text = element_text(size = 8),
    #         legend.spacing.y = unit(0, "cm"),
    #         legend.title = element_blank(),
    #         # legend.position = c(0.18,0.15),
    #         legend.key = element_blank(),
    #         # legend.background = element_rect(color = "black"),
    #         # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
    #         panel.background = element_rect(fill = "white"),
    #         axis.line = element_line(color = "black"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray95"))
    # a <- a + geom_smooth(data = STL.CHI.WW.results, aes(x=wd.we.diff, y=p.value, group=1), se=FALSE, color="gray50", linewidth=0.25)
    # ggsave(filename = "Significance vs W-W difference - St Louis-Chicago.png", plot=a, width = 6, height=4.5)





# MAKE MAPS SHOWING CHANGES - FOURTH HIGH VALUES (average for each monitor for each 5-year bin) #####################################

#Make plots for paper - Chicago and St. Louis

#Select just monitors used in trends plots (above) for mapping
mons.for.trends <- cluster.mon.lat.long %>%
  dplyr::select(Site,Area,Cluster) %>%
  dplyr::filter(Area %in% c("Chicago","St. Louis")) %>%
  dplyr::select(Site)

MDA8.5yr.Chi.Stl <- left_join(mons.for.trends, MDA8.5yr.avgs.by.mon, by="Site")

# write.csv(MDA8.5yr.Chi.Stl, "Mean MDA8s by monitors - Chicago-St Louis.csv", row.names = FALSE)

#Subset for each set of years to plot (try 1987-91, 1992-1996, and 2017-21)
MDA8.Chi.87.91 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "Chicago" & yr.bin == "1987-91")
MDA8.Chi.92.96 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "Chicago" & yr.bin == "1992-96")
MDA8.Chi.17.21 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "Chicago" & yr.bin == "2017-21")

MDA8.Stl.87.91 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "St. Louis" & yr.bin == "1987-91")
MDA8.Stl.92.96 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "St. Louis" & yr.bin == "1992-96")
MDA8.Stl.17.21 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "St. Louis" & yr.bin == "2017-21")

#SETUP BASIC MAPS
    states <- map_data("state")
    region.states <- subset(states, region %in% c("wisconsin","illinois","indiana","michigan","ohio","pennsylvania","minnesota","iowa","missouri","kentucky","west virginia"))
    
    region.map <- ggplot(data=region.states, mapping=aes(x=long, y=lat, group=group)) + geom_polygon(fill="white") +
      geom_polygon(color="black", fill=NA) + coord_fixed(ratio=1.3) + xlab(NULL) + ylab(NULL) + theme_void()

#Add counties for zooms
    counties <- map_data("county")
    region.counties <- subset(counties, region %in% c("wisconsin","illinois","indiana","michigan","ohio","pennsylvania","minnesota","iowa","missouri","kentucky","west virginia"))
    
    region.map.counties <- ggplot() + 
      geom_polygon(data=region.counties, mapping=aes(x=long, y=lat, group=group), color="gray", fill=NA) +
      geom_polygon(data=region.states, mapping=aes(x=long, y=lat, group=group), color="black", fill=NA) +
      xlab(NULL) + ylab(NULL) + theme_void() # + coord_fixed(ratio=1.3)

#Setup for map zooms with county borders
    # norm.4th.map.counties <- region.map.counties + geom_point(data=yr.subset, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
    #   ggtitle(paste0("Mean Fourth High Values - ", i)) +
    #   coord_map(xlim = c(-92.4,-80.8), ylim = c(37.25,47)) +
    #   # scale_fill_distiller(palette="RdYlBu", direction = -1, name="%", limits = c(55,100), breaks = seq(55,100,by=5)) +
    #   scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(50,115), breaks = seq(50,115,by=5)) +
    #   theme(plot.title = element_text(size = 14, hjust = 0),
    #         plot.background = element_rect(fill = "white", color="white"),
    #         panel.background = element_rect(fill = "white", color="white"))


    # Chicago.extent <- extent(-88.6,-86.4,41.0,42.8)
    # St.Louis.extent <- extent(-91.3,-89.45,37.7,39.5)

    
# #for 1987-91 to 2017-21 maps (different color schemes for different starting years)
#     
# #Chicago maps
#     
#     Chi.1987.91.map <- region.map.counties + 
#       geom_point(data=MDA8.Chi.87.91, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
#       coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
#       # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
#       scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
#       # annotate("text", x=-87, y=42.6, label = "1987-91", size=6) +
#       theme(legend.text = element_text(size = 14),
#         plot.background = element_rect(fill = "white", color="white"),
#         panel.background = element_rect(fill = "white", color="white"),
#         plot.margin = unit(c(1,1,1,1), "lines"))
#     
#     Chi.2017.21.map <- region.map.counties + 
#       geom_point(data=MDA8.Chi.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
#       coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
#       # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
#       scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
#       # annotate("text", x=-87, y=42.6, label = "2017-21", size=6) +
#       theme(legend.text = element_text(size = 14),
#             plot.background = element_rect(fill = "white", color="white"),
#             panel.background = element_rect(fill = "white", color="white"))
#     
# #St. Louis maps
#     
#     Stl.1987.91.map <- region.map.counties + 
#       geom_point(data=MDA8.Stl.87.91, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
#       coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
#       # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
#       scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
#       annotate("text", x=-90.3, y=39.4, label = "1987-91", size=6, hjust=0.5) +
#       theme(legend.text = element_text(size = 14),
#             plot.background = element_rect(fill = "white", color="white"),
#             panel.background = element_rect(fill = "white", color="white"))
#     
#     Stl.2017.21.map <- region.map.counties + 
#       geom_point(data=MDA8.Stl.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
#       coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
#       # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
#       scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
#       annotate("text", x=-90.3, y=39.4, label = "2017-21", size=6, hjust=0.5) +
#       theme(legend.text = element_text(size = 14),
#             legend.key.size = unit(1, "cm"),
#             plot.background = element_rect(fill = "white", color="white"),
#             panel.background = element_rect(fill = "white", color="white"))
#     
# #Extract legend
#     legend <- cowplot::get_legend(Stl.2017.21.map + theme(legend.box.margin = margin(0,0,0,12), return_all = TRUE))
#     
# #remove legends from plots
#     Chi.1987.91.map <- Chi.1987.91.map + ggplot2::theme(legend.position="none")
#     Chi.2017.21.map <- Chi.2017.21.map + ggplot2::theme(legend.position="none")
#     Stl.1987.91.map <- Stl.1987.91.map + ggplot2::theme(legend.position="none")
#     Stl.2017.21.map <- Stl.2017.21.map + ggplot2::theme(legend.position="none")
#     
# #Combine plots    
#     paper.plot <- cowplot::plot_grid(Stl.1987.91.map, Stl.2017.21.map, Chi.1987.91.map, Chi.2017.21.map, nrow=2, ncol=2, 
#                                      labels = c("(a)","(b)","(c)","(d)"), label_x = -0.025, align = "hv") 
# #Add in legend
#     paper.plot <- cowplot::plot_grid(paper.plot, legend, rel_widths = c(3,0.6)) + theme(panel.background = element_rect(fill = "white", color="white"))
#     
#     ggsave(filename = "Trends MDA8 maps - St Louis-Chicago 1987-2021.png", plot = paper.plot, width=6.5, height = 6)
    

#for 1992-96 to 2017-21 maps (different color schemes for different starting years)
    
#Chicago maps
    
    Chi.1992.96.map <- region.map.counties + 
      geom_point(data=MDA8.Chi.92.96, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      # annotate("text", x=-87, y=42.6, label = "1987-91", size=6) +
      theme(legend.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    
    Chi.2017.21.map <- region.map.counties + 
      geom_point(data=MDA8.Chi.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      # annotate("text", x=-87, y=42.6, label = "2017-21", size=6) +
      theme(legend.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"))
    
    #St. Louis maps
    
    Stl.1992.96.map <- region.map.counties + 
      geom_point(data=MDA8.Stl.92.96, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"))
    
    Stl.2017.21.map <- region.map.counties + 
      geom_point(data=MDA8.Stl.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      # scale_fill_distiller(palette="RdYlBu", direction = -1, name="ppb", limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      annotate("text", x=-90.3, y=39.4, label = "2017-21", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"))
    
    #Extract legend
    legend <- cowplot::get_legend(Stl.2017.21.map + theme(legend.box.margin = margin(0,0,0,12), return_all = TRUE))
    
    #remove legends from plots
    Chi.1992.96.map <- Chi.1992.96.map + ggplot2::theme(legend.position="none")
    Chi.2017.21.map <- Chi.2017.21.map + ggplot2::theme(legend.position="none")
    Stl.1992.96.map <- Stl.1992.96.map + ggplot2::theme(legend.position="none")
    Stl.2017.21.map <- Stl.2017.21.map + ggplot2::theme(legend.position="none")
    
    #Combine plots    
    paper.plot <- cowplot::plot_grid(Stl.1992.96.map, Stl.2017.21.map, Chi.1992.96.map, Chi.2017.21.map, nrow=2, ncol=2, 
                                     labels = c("(a)","(b)","(c)","(d)"), label_x = -0.025, align = "hv") 
    #Add in legend
    paper.plot <- cowplot::plot_grid(paper.plot, legend, rel_widths = c(3,0.6)) + theme(panel.background = element_rect(fill = "white", color="white"))
    
    ggsave(filename = "Trends MDA8 maps - St Louis-Chicago 1992-2021.png", plot = paper.plot, width=6.5, height = 6)    

    
#Make plots of all year groups for St. Louis and Chicago separately
    
#St. Louis - from 1992-96
Stl.MDA8 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "St. Louis" & yr.bin != "1987-91")

    a <- region.map.counties + facet_wrap(~yr.bin) +
      geom_point(data=Stl.MDA8, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      # scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      # annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            strip.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    ggsave(filename = "St Louis MDA8 maps - 1992-2021.png", plot = a, width = 8.5, height = 6.25)
    
#Chicago - from 1992-96
Chi.MDA8 <- dplyr::filter(MDA8.5yr.Chi.Stl, Area == "Chicago" & yr.bin != "1987-91")
    
    a <- region.map.counties + facet_wrap(~yr.bin) +
      geom_point(data=Chi.MDA8, mapping=aes(x=Longitude, y=Latitude, group=1, fill=mean.4th), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      # scale_fill_gradientn(colors = matlab.like(56), name=NULL, limits = c(60,115), breaks = seq(60,115,by=5)) +
      scale_fill_gradientn(colors = matlab.like(42), name=NULL, limits = c(60,101), breaks = seq(60,100,by=5)) +
      # annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            strip.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    ggsave(filename = "Chicago MDA8 maps - 1992-2021.png", plot = a, width = 8.5, height = 6.25)
    
      

    








## PLOT NO2 TRENDS OVER TIME AND OVER DISTANCE FROM THE CITY CENTER ####################################

## LOAD NO2 (HOURLY) #############################################################

#1995-2020  
NO2.CO.data.95.20 <- read.table("C:/Users/afdic/OneDrive/Documents/ozone/obs-sensitivity/weekday-weekend/AMP501_1970835-hourly NO2 95-20.txt", header=FALSE, fill=TRUE, sep="|")
NO2.CO.select.95.20 <- NO2.CO.data.95.20[,c(3:6,9,11,12,13)]
NO2.CO.select.95.20 <- dplyr::rename(NO2.CO.select.95.20, State=V3, County=V4, Mon=V5, param=V6, units=V9, date=V11, hour=V12, value=V13)

#1991-1994  
NO2.data.91.94 <- read.table("AMP501_2013886-NO2 1991-94.txt", header=FALSE, fill=TRUE, sep="|")
NO2.select.91.94 <- NO2.data.91.94[,c(3:6,9,11,12,13)]
NO2.select.91.94 <- dplyr::rename(NO2.select.91.94, State=V3, County=V4, Mon=V5, param=V6, units=V9, date=V11, hour=V12, value=V13)

# #2021 
NO2.data.21 <- read.table("./datafiles/AMP501_2016763-LADCO HCHO-NO2 2021 certified.txt", header=FALSE, fill=TRUE, sep="|")
NO2.select.21 <- dplyr::filter(NO2.data.21, V6 == "42602") #filter out HCHO data
NO2.select.21 <- NO2.data.21[,c(3:6,9,11,12,13)]
NO2.select.21 <- dplyr::rename(NO2.select.21, State=V3, County=V4, Mon=V5, param=V6, units=V9, date=V11, hour=V12, value=V13)

NO2.select <- rbind(NO2.CO.select.95.20, NO2.select.91.94, NO2.select.21)
# NO2.CO.select <- rbind(NO2.CO.select.95.20, NO2.select.91.94, NO2.select.21)

#Select just NO2 data for R5 and shared NAA data
NO2.select <- filter(NO2.select, param == "42602" &
                          (State %in% c(17,18,26,27,39,55) |
                          (State==29 & County %in% c(99,183,189,510)) |
                          (State==21 & County %in% c(29,111,185,15,37,117))))

#Format site number and date
NO2.select$County <- str_pad(NO2.select$County, 3, pad = "0")
NO2.select$Mon <- str_pad(NO2.select$Mon, 4, pad = "0")
NO2.select$Site <- as.numeric(paste(NO2.select$State, NO2.select$County, NO2.select$Mon, sep=""))
# NO2.select$datetime <- as.POSIXct(paste(NO2.select$date,NO2.select$hour),tz="", "%Y%m%d %H:%M")
NO2.select$date <- as.Date(as.character(NO2.select$date), "%Y%m%d")

#Convert units (to ppb)
NO2.select <- NO2.select %>%
  dplyr::mutate(units = as.numeric(units), value = as.numeric(value)) %>%
  dplyr::mutate(conc.ppb = ifelse(units==7, value*1000, value))
NO2.select <- NO2.select[,c(9,1,2,3,4,6,7,10)]


#Select just midday values (9:00 to 3:00, as Olivia used - actually, 9:00 am to 2:59 pm)
NO2.midday <- NO2.select %>%
  dplyr::filter(hour %in% c("09:00","10:00","11:00","12:00","13:00","14:00")) %>%
  dplyr::group_by(State,County,Mon,date,Site) %>%
  dplyr::summarize(midday.ppb = mean(conc.ppb, na.rm=TRUE)) %>%
  ungroup 

#pull in NAA info (includes the Twin Cities - not a NAA)
LADCO.counties <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/obs-sensitivity/weekday-weekend/NAA area counties-LADCO-updated-wMN.csv",header=TRUE)
NO2.midday$County <- as.numeric(NO2.midday$County)
NO2.midday$State <- as.numeric(NO2.midday$State)

NO2.midday <- left_join(NO2.midday, LADCO.counties, by=c("State"="fips_state","County"="fips_cnty")) #add NAA ID
NO2.midday <- NO2.midday %>%
  filter(!is.na(NAA.2015)) %>% #removes monitors not in a NAA
  dplyr::mutate(year = as.numeric(format(date, "%Y")), weekday = format(date, "%a"), month = format(date, "%m"))

#Calculate ozone season (May-September) averages
NO2.midday.oz.seas <- NO2.midday %>%
  dplyr::select(Site, NAA.2015, date, year, month, midday.ppb) %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  group_by(Site, NAA.2015, year) %>%
  dplyr::summarize(NO2.mean = mean(midday.ppb, na.rm=TRUE)) %>%
  ungroup()

#Plot data for all NO2 monitors in each NAA

NO2.midday.oz.seas$Site <- as.character(NO2.midday.oz.seas$Site)

NAA.all.list <- unique(NO2.midday.oz.seas$NAA.2015)

    # for (i in NAA.all.list)
    # {
    #   NAA.subset <- subset(NO2.midday.oz.seas, NO2.midday.oz.seas$NAA.2015 == i)
    # 
    #   #Plot showing all monitors
    #   a <- ggplot(data=NAA.subset) +
    #     geom_line(aes(x=year, y=NO2.mean, color=Site, group=Site), size=1, linetype=1) +
    #     xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 - ", i)) +
    #     # scale_color_brewer(palette = "Paired") +
    #     # scale_color_manual(values = c("green4","royalblue2","mediumorchid3")) +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 8),
    #           legend.title = element_blank(),
    #           plot.title = element_text(size = 16, hjust = 0))
    # ggsave(filename = paste0("NO2 conc by year - all - ", i, ".png"), plot = a, width = 7, height =5)
    # }

# Apply new method for selecting monitors (as of January 2022) - think will create more meaningful trends
# 3 criteria: (1) Only include sites in core counties in NAA (with a few exceptions), (2) eliminate all near-road monitors, (3) only include records with at least 5 years of data. Exceptions: 
#Twin Cities didn't have any NO2 monitors in the core counties after 2002 (except NR), so include all counties. 
#St. Louis: include one monitor in E. St. Louis with a very long record and similar concentrations to St. Louis City monitors, but eliminate the other monitor in this county. 
#Louisville: eliminate a monitor on the far outskirts (rural) of the core county. 
#Cleveland: eliminate two monitors because they didn't operate after 2008 and have much lower ozone than the one monitor that did (GT Craig), which is essentially an informal near-road monitor. The GT Craig monitor is not a good representative of NO2 in Cleveland, but it's all we have for most of the time period so stick with it and caveat it.
#Detroit: eliminate one monitor that operated 2012-2018 with very different NO2 than the only other (non-NR) monitor operating then.

NO2.complete.sites <- NO2.midday.oz.seas %>%
  dplyr::mutate(st.cty = substr(Site, start=1, stop=5)) %>% #list state-county codes
  dplyr::filter(st.cty %in% c("17031","39061","39035","26163","21111","29510","29189","17163","27003","27037","27053","27123","27171","55079","55071")) %>% #includes just central areas
  dplyr::filter(!Site %in% c("390350066","390350070","171630900","211110051","261630094","390350043")) %>% #eliminates sites from areas as described above
  dplyr::filter(!(Site %in% c("170310119","170310219","390610048","390350073","261630095","261630093","211110075","295100094","291890016","270530962","270370480","550790056"))) %>% #eliminate NR sites
  dplyr::filter(!is.na(NO2.mean)) %>%
  dplyr::group_by(Site, NAA.2015) %>%
  dplyr::summarize(yr.count = n_distinct(year), start=min(year), end=max(year)) %>%
  dplyr::filter(yr.count >=5) %>% #Keep only monitors with at least 5 years of data
  ungroup()

#Convert to vector form for filtering below
NO2.complete.list <- unique(NO2.complete.sites$Site)

NO2.complete <- dplyr::filter(NO2.midday.oz.seas, (Site %in% NO2.complete.list)) #just select data for complete-ish monitors (>5 years of data)
# Eliminate 2021 data for one CO monitor because it only reported 0's for 2021 (it's a NCORE monitor - looks like it never reports to AirNow Tech; 261630001 in Detroit)
# NO2.CO.complete <- dplyr::filter(NO2.CO.complete, !(param=="CO" & Site==261630001 & year==2021))
NO2.complete$Site <- as.character(NO2.complete$Site)


#Plot average NO2  over time 

NAA.list <- unique(NO2.complete$NAA.2015)

      # for (j in NAA.list)
      # {
      #   NAA.subset <- subset(NO2.complete, NO2.complete$NAA.2015 == j)
      # 
      #   #Plot showing all monitors
      #   a <- ggplot(data=NAA.subset) +
      #     geom_line(aes(x=year, y=NO2.mean, color=Site, group=Site), size=1, linetype=1) +
      #     xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations - ",j)) +
      #     scale_color_brewer(palette = "Paired") +
      #     # scale_color_manual(values = c("green4","royalblue2","mediumorchid3")) +
      #     theme(axis.text = element_text(size = 14, color = "black"),
      #           axis.text.x = element_text(size = 14,  vjust=0.5),
      #           axis.title = element_text(size = 16),
      #           legend.text = element_text(size = 8),
      #           legend.title = element_blank(),
      #           plot.title = element_text(size = 18, hjust = 0))
      # 
      #   ggsave(filename = paste0("NO2 conc by year-complete mons - ", j, ".png"), plot = a, width = 7, height =5)
      # 
      #   #Plot average for each NAA
      #   b <- ggplot(data=NAA.subset) +
      #     stat_summary(fun = mean, geom="line", aes(x=year, y=NO2.mean), color="blue") +
      #     xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations ",j)) +
      #     geom_hline(yintercept = 0, color="darkgray") +
      #     theme(axis.text = element_text(size = 14, color = "black"),
      #           axis.text.x = element_text(size = 14,  vjust=0.5),
      #           axis.title = element_text(size = 16),
      #           legend.text = element_text(size = 8),
      #           legend.title = element_blank(),
      #           plot.title = element_text(size = 18, hjust = 0))
      # 
      #   ggsave(filename = paste0("NO2 conc means by year-complete mons - ", j, ".png"), plot = b, width = 7, height =5)
      # 
      #   #Plot boxplots of annual means for each group of years at each monitor for each NAA
      #   NAA.subset$year <- as.character(NAA.subset$year) #to make boxplot plot correctly
      # 
      #   c <- ggplot(data = NAA.subset) + geom_boxplot(aes(x=year, y=NO2.mean), alpha=0.5) +
      #     xlab(NULL) + ylab(paste0("Annual Mean NO2 (ppb)")) + ggtitle("Annual NO2 - ", j) +
      #     geom_hline(yintercept = 0, color="darkgray") +
      #     ylim(0,40) +
      #     # scale_fill_brewer(palette = "Set1") +
      #     # annotate("text", x= 5, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
      #     theme(axis.text = element_text(size = 14, color = "black"),
      #           axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
      #           axis.title = element_text(size = 16),
      #           legend.text = element_text(size = 12),
      #           legend.title = element_blank(),
      #           # legend.position = c(0.95,0.85), legend.justification = c(1,1),
      #           plot.title = element_text(size = 18, hjust = 0))
      #       ggsave(filename = paste0("NO2 conc means by year-boxplots - ", j, ".png"), plot = c, width = 6, height =4)
      # }


#Plot average for each NAA - for all areas in the same plot
    # b <- ggplot(data=NO2.complete) +
    #   stat_summary(fun = mean, geom="line", aes(x=year, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations")) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14,  vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 8),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 conc means by year-complete mons - all.png"), plot = b, width = 7, height =5)




#Calculate averages for 5- fixed year periods 
NO2.complete <- dplyr::mutate(NO2.complete, yr.bins = ifelse(year <= 1995, "1991-95", ifelse( year <= 2000, "1996-00", ifelse(year <= 2005, "2001-05", 
                                                             ifelse(year <= 2010, "2006-10", ifelse(year <= 2015, "2011-15", "2016-21"))))))
NO2.fixed.bins <- NO2.complete %>%
  dplyr::group_by(NAA.2015, yr.bins) %>%
  dplyr::summarize(NO2.mean = mean(NO2.mean, na.rm=TRUE)) %>%
  ungroup()

# #Export list of NO2 monitors
# NO2.mon.list <- NO2.CO.complete %>%
#   filter(param == "NO2") %>%
#   select(1,2,4) %>%
#   distinct() %>%
#   mutate(year=paste0("X",year), present="X") %>%
#   pivot_wider(names_from = "year", values_from = "present")
# write.table(NO2.mon.list, "clipboard", sep="\t", row.names = FALSE)

#Plot fixed year average concentrations
      # for (j in NAA.list)
      # {
      #   NAA.subset <- subset(NO2.fixed.bins, NO2.fixed.bins$NAA.2015 == j)
      # 
      #     #Plot average for each NAA - with legend
      #     a <- ggplot(data=NAA.subset) +
      #       geom_line(aes(x=yr.bins, y=NO2.mean, group=1), size=1, color="blue") +
      #       xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 Concentrations - ", j)) +
      #       # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
      #       geom_hline(yintercept = 0, color="darkgray") +
      #       ylim(0,25) +
      #       # scale_color_brewer(palette = "Set1") +
      #       theme(axis.text = element_text(size = 14, color = "black"),
      #             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
      #             axis.title = element_text(size = 16),
      #             legend.text = element_text(size = 12),
      #             legend.title = element_blank(),
      #             plot.title = element_text(size = 18, hjust = 0))
      #     ggsave(filename = paste0("NO2 conc means by year bins - ", j, ".png"), plot = a, width = 6, height =4)
      # 
      #     }
        

#Plot fixed year average concentrations with all NAAs on one plot - no Manitowoc or Milwaukee (for ozone distance from city center analysis)
NO2.fixed.bins.most <- dplyr::filter(NO2.fixed.bins, !(NAA.2015 %in% c("Milwaukee","Manitowoc")))

    # a <- ggplot(data=NO2.fixed.bins.most) +
    #   geom_line(aes(x=yr.bins, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 Concentrations")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,25) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 12, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 conc means by year bins - most areas.png"), plot = a, width = 6, height =4)



#Early morning NO2 (5:00-8:59 LDT = 4:00-7:59 LST = 4,5,6,7:00) ############################################

#Select just early morning values 
NO2.am <- NO2.select %>%
  dplyr::filter(hour %in% c("04:00","05:00","06:00","07:00")) %>%
  dplyr::group_by(State,County,Mon,date,Site) %>%
  dplyr::summarize(midday.ppb = mean(conc.ppb, na.rm=TRUE)) %>%
  ungroup 

#pull in NAA info (includes the Twin Cities - not a NAA)
NO2.am$County <- as.numeric(NO2.am$County)
NO2.am$State <- as.numeric(NO2.am$State)

NO2.am <- left_join(NO2.am, LADCO.counties, by=c("State"="fips_state","County"="fips_cnty")) #add NAA ID
NO2.am <- NO2.am %>%
  filter(!is.na(NAA.2015)) %>% #removes monitors not in a NAA
  dplyr::mutate(year = as.numeric(format(date, "%Y")), weekday = format(date, "%a"), month = format(date, "%m"))

#Calculate ozone season (May-September) averages
NO2.am.oz.seas <- NO2.am %>%
  dplyr::select(Site, NAA.2015, date, year, month, midday.ppb) %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  group_by(Site, NAA.2015, year) %>%
  dplyr::summarize(NO2.mean = mean(midday.ppb, na.rm=TRUE)) %>%
  ungroup()

#Select just data for complete sites (determined above)
NO2.am.complete <- dplyr::filter(NO2.am.oz.seas, (Site %in% NO2.complete.list)) #just select data for complete-ish monitors (>5 years of data)
NO2.am.complete$Site <- as.character(NO2.am.complete$Site)

#Plot average NO2 by year

# Plot average for each NAA - for all areas in the same plot
    # b <- ggplot(data=NO2.am.complete) +
    #   stat_summary(fun = mean, geom="line", aes(x=year, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean Early Morning NO2 concentrations")) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   scale_x_continuous(breaks = seq(1990,2020,by=5), minor_breaks = seq(1990,2021,by=1)) + scale_y_continuous(breaks=seq(0,30,by=5)) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14,  vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 8),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 am conc means by year-complete mons - all.png"), plot = b, width = 7, height =5)


#Calculate averages for 5- fixed year periods - just for 2001-2020
NO2.am.complete <- dplyr::mutate(NO2.am.complete, yr.bins = ifelse(year <= 1995, "1991-95", ifelse( year <= 2000, "1996-00", ifelse(year <= 2005, "2001-05", 
                                                                                                                              ifelse(year <= 2010, "2006-10", ifelse(year <= 2015, "2011-15", "2016-21"))))))
NO2.am.fixed.bins <- NO2.am.complete %>%
  dplyr::group_by(NAA.2015, yr.bins) %>%
  dplyr::summarize(NO2.mean = mean(NO2.mean, na.rm=TRUE)) %>%
  ungroup()


#Plot fixed year average concentrations with all NAAs on one plot - no Manitowoc or Milwaukee (for ozone distance from city center analysis)
NO2.am.fixed.bins.most <- dplyr::filter(NO2.am.fixed.bins, !(NAA.2015 %in% c("Milwaukee","Manitowoc")))
write.table(NO2.am.fixed.bins.most, "clipboard", sep="\t", row.names = FALSE)

    # a <- ggplot(data=NO2.am.fixed.bins.most) +
    #   geom_line(aes(x=yr.bins, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean Early Morning NO2 Concentrations")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,32) + scale_y_continuous(breaks = seq(0,32,by=4)) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 am conc means by year bins - most areas.png"), plot = a, width = 6, height =4)



#Early afternoon NO2 - "peak O3" period to match period for modeling (13:00-16:59 LDT = 12:00-15:59 LST = 12,13,14,15:00) ############################################

#Select just early morning values 
NO2.pm <- NO2.select %>%
  dplyr::filter(hour %in% c("12:00","13:00","14:00","15:00")) %>%
  dplyr::group_by(State,County,Mon,date,Site) %>%
  dplyr::summarize(midday.ppb = mean(conc.ppb, na.rm=TRUE)) %>%
  ungroup 

#pull in NAA info (includes the Twin Cities - not a NAA)
NO2.pm$County <- as.numeric(NO2.pm$County)
NO2.pm$State <- as.numeric(NO2.pm$State)

NO2.pm <- left_join(NO2.pm, LADCO.counties, by=c("State"="fips_state","County"="fips_cnty")) #add NAA ID
NO2.pm <- NO2.pm %>%
  filter(!is.na(NAA.2015)) %>% #removes monitors not in a NAA
  dplyr::mutate(year = as.numeric(format(date, "%Y")), weekday = format(date, "%a"), month = format(date, "%m"))

#Calculate ozone season (May-September) averages
NO2.pm.oz.seas <- NO2.pm %>%
  dplyr::select(Site, NAA.2015, date, year, month, midday.ppb) %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  group_by(Site, NAA.2015, year) %>%
  dplyr::summarize(NO2.mean = mean(midday.ppb, na.rm=TRUE)) %>%
  ungroup()

#Select just data for complete sites (determined above)
NO2.pm.complete <- dplyr::filter(NO2.pm.oz.seas, (Site %in% NO2.complete.list)) #just select data for complete-ish monitors (>5 years of data)
NO2.pm.complete$Site <- as.character(NO2.pm.complete$Site)

#Plot average NO2 by year

# Plot average for each NAA - for all areas in the same plot
    # b <- ggplot(data=NO2.pm.complete) +
    #   stat_summary(fun = mean, geom="line", aes(x=year, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean Early Afternoon NO2 concentrations")) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   scale_x_continuous(breaks = seq(1990,2020,by=5), minor_breaks = seq(1990,2021,by=1)) + scale_y_continuous(breaks=seq(0,30,by=5)) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14,  vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 8),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 pm conc means by year-complete mons - all.png"), plot = b, width = 7, height =5)


#Calculate averages for 5- fixed year periods - just for 2001-2020
NO2.pm.complete <- dplyr::mutate(NO2.pm.complete, yr.bins = ifelse(year <= 1995, "1991-95", ifelse( year <= 2000, "1996-00", ifelse(year <= 2005, "2001-05", 
                                                                                                                                    ifelse(year <= 2010, "2006-10", ifelse(year <= 2015, "2011-15", "2016-21"))))))
NO2.pm.fixed.bins <- NO2.pm.complete %>%
  dplyr::group_by(NAA.2015, yr.bins) %>%
  dplyr::summarize(NO2.mean = mean(NO2.mean, na.rm=TRUE)) %>%
  ungroup()


#Plot fixed year average concentrations with all NAAs on one plot - no Manitowoc or Milwaukee (for ozone distance from city center analysis)
NO2.pm.fixed.bins.most <- dplyr::filter(NO2.pm.fixed.bins, !(NAA.2015 %in% c("Milwaukee","Manitowoc")))

    # a <- ggplot(data=NO2.pm.fixed.bins.most) +
    #   geom_line(aes(x=yr.bins, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean Early Afternoon NO2 Concentrations")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,32) +
    #   scale_color_brewer(palette = "Paired") +
    #   scale_y_continuous(breaks = seq(0,28,by=4)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 pm conc means by year bins - most areas.png"), plot = a, width = 6, height =4)



#24-hour average NO2  ############################################

#Select just early morning values 
NO2.24h <- NO2.select %>%
  dplyr::group_by(State,County,Mon,date,Site) %>%
  dplyr::summarize(midday.ppb = mean(conc.ppb, na.rm=TRUE)) %>%
  ungroup 

#pull in NAA info (includes the Twin Cities - not a NAA)
NO2.24h$County <- as.numeric(NO2.24h$County)
NO2.24h$State <- as.numeric(NO2.24h$State)

NO2.24h <- left_join(NO2.24h, LADCO.counties, by=c("State"="fips_state","County"="fips_cnty")) #add NAA ID
NO2.24h <- NO2.24h %>%
  filter(!is.na(NAA.2015)) %>% #removes monitors not in a NAA
  dplyr::mutate(year = as.numeric(format(date, "%Y")), weekday = format(date, "%a"), month = format(date, "%m"))

#Calculate ozone season (May-September) averages
NO2.24h.oz.seas <- NO2.24h %>%
  dplyr::select(Site, NAA.2015, date, year, month, midday.ppb) %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  group_by(Site, NAA.2015, year) %>%
  dplyr::summarize(NO2.mean = mean(midday.ppb, na.rm=TRUE)) %>%
  ungroup()

#Select just data for complete sites (determined above)
NO2.24h.complete <- dplyr::filter(NO2.24h.oz.seas, (Site %in% NO2.complete.list)) #just select data for complete-ish monitors (>5 years of data)
NO2.24h.complete$Site <- as.character(NO2.24h.complete$Site)

#Plot average NO2 by year

# Plot average for each NAA - for all areas in the same plot
    # b <- ggplot(data=NO2.24h.complete) +
    #   stat_summary(fun = mean, geom="line", aes(x=year, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean 24-Hour Average NO2 concentrations")) + scale_y_continuous(breaks=seq(0,30,by=5)) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   scale_x_continuous(breaks = seq(1990,2020,by=5), minor_breaks = seq(1990,2021,by=1)) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14,  vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 8),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 24h conc means by year-complete mons - all.png"), plot = b, width = 7, height =5)


#Calculate averages for 5- fixed year periods - just for 2001-2020
NO2.24h.complete <- dplyr::mutate(NO2.24h.complete, yr.bins = ifelse(year <= 1995, "1991-95", ifelse( year <= 2000, "1996-00", ifelse(year <= 2005, "2001-05", 
                                                                                                                                    ifelse(year <= 2010, "2006-10", ifelse(year <= 2015, "2011-15", "2016-21"))))))
NO2.24h.fixed.bins <- NO2.24h.complete %>%
  dplyr::group_by(NAA.2015, yr.bins) %>%
  dplyr::summarize(NO2.mean = mean(NO2.mean, na.rm=TRUE)) %>%
  ungroup()


#Plot fixed year average concentrations with all NAAs on one plot - no Manitowoc or Milwaukee (for ozone distance from city center analysis)
NO2.24h.fixed.bins.most <- dplyr::filter(NO2.24h.fixed.bins, !(NAA.2015 %in% c("Milwaukee","Manitowoc")))

    # a <- ggplot(data=NO2.24h.fixed.bins.most) +
    #   geom_line(aes(x=yr.bins, y=NO2.mean, color=NAA.2015, group=NAA.2015), size=1) +
    #   xlab(NULL) + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean 24-Hour Average NO2 Concentrations")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,32) +
    #   scale_color_brewer(palette = "Paired") +
    #   scale_y_continuous(breaks = seq(0,28,by=4)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("NO2 24h conc means by year bins - most areas.png"), plot = a, width = 6, height =4)







## DETERMINE NO2 TRENDS WITH DISTANCE FROM THE CITY CENTER - BASED ON 2016 abc MODELING #############################################

library("M3")        # http://cran.r-project.org/web/packages/M3/
library("rasterVis") # http://cran.r-project.org/web/packages/rasterVis/


# #One-time: Determine distance of grid cells from city center. Import grid cell/lat/long crosswalk then export to csv to import into ArcGIS to determine distances.
# 
#Import lat/long - grid cell crosswalk

lcc.file <- "C:/Users/afdic/OneDrive/Documents/ozone/modeling/model sensitivity analysis/GRIDCRO2D_d01_2016182-lat-long-12km grids.ncf"

# read input netCDF I/O API data ---
#Note: lcc = Lambert conformal conic projection

lcc.proj4 <- M3::get.proj.info.M3(lcc.file)
lcc.crs <- sp::CRS(lcc.proj4)


#Define LADCO region extent
e.LADCO <- extent(190, 320, 100, 230)
# 
# 
# ## Read in Lat/Long 
# Lat.raster <- raster::brick(lcc.file, varname="LAT")
# Lat.raster@crs <- lcc.crs
# Lat.raster.ladco <- crop(Lat.raster, e.LADCO)
# 
# Lat.LADCO <- as.data.frame(Lat.raster.ladco, xy=TRUE) #converts raster into data frame
# Lat.LADCO <- dplyr::rename(Lat.LADCO, Latitude = X1)
# 
# Lon.raster <- raster::brick(lcc.file, varname="LON")
# Lon.raster@crs <- lcc.crs
# Lon.raster.ladco <- crop(Lon.raster, e.LADCO)
# 
# Lon.LADCO <- as.data.frame(Lon.raster.ladco, xy=TRUE) #converts raster into data frame
# Lon.LADCO <- dplyr::rename(Lon.LADCO, Longitude = X1)
# 
# Lat.Long.LADCO <- left_join(Lat.LADCO, Lon.LADCO, by=c("x","y"))
# 
# write.csv(Lat.Long.LADCO, "Lat-long for LADCO grid cells.csv", row.names = FALSE)

#Export metadata to identify the water identifier
# nc.data <- nc_open(lcc.file)
# {
#   sink("GRIDCRO2D_d01_2016182-lat-long-12km grids-metadata.txt")
#   print(nc.data)
#   sink()
# }
# 
# ## Read in open water
# Water.raster <- raster::brick(lcc.file, varname="LWMASK")
# Water.raster@crs <- lcc.crs
# Water.raster.ladco <- crop(Water.raster, e.LADCO)
# 
# Water.LADCO <- as.data.frame(Water.raster.ladco, xy=TRUE) #converts raster into data frame
# Water.LADCO <- dplyr::rename(Water.LADCO, Water = X1)




#Import distance from city center for grid cells

grid.city.center.dist <- read.csv("LADCO 12km grid cells-distance to city center.csv", header=TRUE)
grid.city.center.dist <- grid.city.center.dist %>%
  dplyr::filter(NEAR_FID != -1) %>%
  dplyr::mutate(dist.km = NEAR_DIST / 1000)
grid.city.center.dist <- left_join(grid.city.center.dist, city.codes, by="NEAR_FID")
grid.city.center.dist.narrow <- dplyr::select(grid.city.center.dist, 2,3,9,8)

#Import 2016 (abc) NO2

NO2.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/modeling/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means - all areas-2028.csv", header=TRUE)
NO2.2016.model <- NO2.2016.model %>%
  dplyr::select(1:4,9) %>%
  dplyr::filter(time != "build.O3")

NO2.2016.model <- full_join(NO2.2016.model, grid.city.center.dist.narrow, by=c("x","y"))

NO2.2016.model.to.use <- NO2.2016.model %>%
  dplyr::filter(!is.na(city)) %>% #keep only cells within 100km of city center
  dplyr::filter(exceed.day == city | (city == "Minneapolis" & exceed.day == "Chicago")) #keep only exceedance days at each area - for Minneapolis, use Chicago exceed days


#Plot distance from city center
city.list <- unique(NO2.2016.model.to.use$city)
time.list <- unique(NO2.2016.model.to.use$time)

    # for (j in city.list)
    # {
    #   NAA.subset <- subset(NO2.2016.model.to.use, NO2.2016.model.to.use$city == j)
    # 
    #   #Plot showing all monitors
    #   a <- ggplot(data=NAA.subset) +
    #     geom_point(aes(x=dist.km, y=NO2.2016, color=time, group=time), size=1) +
    #     xlab("Distance from city center (km)") + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations - ",j)) +
    #     scale_color_brewer(palette = "Set1") +
    #     scale_x_continuous(breaks = seq(0,100,by=20)) +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 8),
    #           legend.title = element_blank(),
    #           plot.title = element_text(size = 18, hjust = 0))
    # 
    #   ggsave(filename = paste0("Model NO2 by distance from city center - all points-", j, ".png"), plot = a, width = 7, height =5)
    # 
    #   #Plot best-fit line
    #   b <- ggplot(data=NAA.subset) +
    #     geom_smooth(aes(x=dist.km, y=NO2.2016, color=time, group=time), se=FALSE) +
    #     xlab("Distance from city center (km)") + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations ",j)) +
    #     geom_hline(yintercept = 0, color="darkgray") +
    #     scale_color_brewer(palette = "Set1") +
    #     scale_x_continuous(breaks = seq(0,100,by=20)) +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 8),
    #           legend.title = element_blank(),
    #           plot.title = element_text(size = 18, hjust = 0))
    # 
    #   ggsave(filename = paste0("Model NO2 by distance from city center - best fit ", j, ".png"), plot = b, width = 7, height =5)
    # }
  

#Plot average for each NAA - for all areas in the same plot - by time (peak NO2 and peak O3)

time.list <- unique(NO2.2016.model.to.use$time)

    # for (i in time.list)
    # {
    #   time.subset <- subset(NO2.2016.model.to.use, NO2.2016.model.to.use$time == i)
    # 
    #   b <- ggplot(data=time.subset) +
    #     geom_smooth(aes(x=dist.km, y=NO2.2016, color=city, group=city), size=1, se=FALSE) +
    #     xlab("Distance from city center (km)") + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations - ", i)) +
    #     geom_hline(yintercept = 0, color="darkgray") +
    #     scale_color_brewer(palette = "Paired") +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 8),
    #           legend.title = element_blank(),
    #           plot.title = element_text(size = 18, hjust = 0))
    #   ggsave(filename = paste0("Model NO2 by distance from city center - best fit-all-", i, ".png"), plot = b, width = 7, height =5)
    # }

NO2.2016.model.mean.dist <- NO2.2016.model.to.use %>%
  dplyr::mutate(km.bins = ifelse(dist.km <10, "0-10", ifelse(dist.km <20, "10-20", ifelse(dist.km <30, "20-30", ifelse(dist.km <40, "30-40", ifelse(dist.km <50, "40-50",
                                 ifelse(dist.km <60, "50-60", ifelse(dist.km <70, "60-70", ifelse(dist.km <80, "70-80", ifelse(dist.km <90, "80-90", "90-100")))))))))) %>%
  dplyr::group_by(city,km.bins,time) %>%
  dplyr::summarize(mean.NO2.2016 = mean(NO2.2016, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(city = ifelse(city=="Minneapolis","Twin Cities",city))
# write.table(NO2.2016.model.mean.dist, "clipboard", sep="\t", row.names = FALSE) #import into Excel

    # for (i in time.list)
    # {
    #   time.subset <- subset(NO2.2016.model.mean.dist, NO2.2016.model.mean.dist$time == i)
    # 
    #   b <- ggplot(data=time.subset) +
    #     geom_line(aes(x=km.bins, y=mean.NO2.2016, color=city, group=city), size=1) +
    #     xlab("Distance from city center (km)") + ylab("Mean NO2 Concentration (ppb)") + ggtitle(paste0("Mean NO2 concentrations - ", i)) +
    #     geom_hline(yintercept = 0, color="darkgray") +
    #     scale_color_brewer(palette = "Paired") +
    #     scale_y_continuous(breaks = seq(0,28,by=4)) +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5, angle=90),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 12),
    #           legend.title = element_blank(),
    #           panel.background = element_rect(fill = "white", color = "black"),
    #           legend.key=element_rect(fill="white"),
    #           panel.grid.major = element_line(color = "gray90"),
    #           panel.grid.minor = element_line(color = "gray90"),
    #           plot.title = element_text(size = 18, hjust = 0))
    #   ggsave(filename = paste0("Model NO2 by distance from city center - binned-all-", i, ".png"), plot = b, width = 6, height =4)
    # }




## DETERMINE VOC TRENDS WITH DISTANCE FROM THE CITY CENTER - BASED ON 2016 aa2a MODELING #############################################

library("M3")        # http://cran.r-project.org/web/packages/M3/
library("rasterVis") # http://cran.r-project.org/web/packages/rasterVis/


# #One-time: Determine distance of grid cells from city center. Import grid cell/lat/long crosswalk then export to csv to import into ArcGIS to determine distances.
# 
#Import lat/long - grid cell crosswalk

lcc.file <- "C:/Users/afdic/OneDrive/Documents/ozone/modeling/model sensitivity analysis/GRIDCRO2D_d01_2016182-lat-long-12km grids.ncf"

# read input netCDF I/O API data ---
#Note: lcc = Lambert conformal conic projection

lcc.proj4 <- M3::get.proj.info.M3(lcc.file)
lcc.crs <- sp::CRS(lcc.proj4)


#Define LADCO region extent
e.LADCO <- extent(190, 320, 100, 230)
# 
# 
# ## Read in Lat/Long 
# Lat.raster <- raster::brick(lcc.file, varname="LAT")
# Lat.raster@crs <- lcc.crs
# Lat.raster.ladco <- crop(Lat.raster, e.LADCO)
# 
# Lat.LADCO <- as.data.frame(Lat.raster.ladco, xy=TRUE) #converts raster into data frame
# Lat.LADCO <- dplyr::rename(Lat.LADCO, Latitude = X1)
# 
# Lon.raster <- raster::brick(lcc.file, varname="LON")
# Lon.raster@crs <- lcc.crs
# Lon.raster.ladco <- crop(Lon.raster, e.LADCO)
# 
# Lon.LADCO <- as.data.frame(Lon.raster.ladco, xy=TRUE) #converts raster into data frame
# Lon.LADCO <- dplyr::rename(Lon.LADCO, Longitude = X1)
# 
# Lat.Long.LADCO <- left_join(Lat.LADCO, Lon.LADCO, by=c("x","y"))
# 
# write.csv(Lat.Long.LADCO, "Lat-long for LADCO grid cells.csv", row.names = FALSE)

#Export metadata to identify the water identifier
# nc.data <- nc_open(lcc.file)
# {
#   sink("GRIDCRO2D_d01_2016182-lat-long-12km grids-metadata.txt")
#   print(nc.data)
#   sink()
# }
# 
# ## Read in open water
# Water.raster <- raster::brick(lcc.file, varname="LWMASK")
# Water.raster@crs <- lcc.crs
# Water.raster.ladco <- crop(Water.raster, e.LADCO)
# 
# Water.LADCO <- as.data.frame(Water.raster.ladco, xy=TRUE) #converts raster into data frame
# Water.LADCO <- dplyr::rename(Water.LADCO, Water = X1)




#Import distance from city center for grid cells

grid.city.center.dist <- read.csv("LADCO 12km grid cells-distance to city center.csv", header=TRUE)
grid.city.center.dist <- grid.city.center.dist %>%
  dplyr::filter(NEAR_FID != -1) %>%
  dplyr::mutate(dist.km = NEAR_DIST / 1000)
grid.city.center.dist <- left_join(grid.city.center.dist, city.codes, by="NEAR_FID")
grid.city.center.dist.narrow <- dplyr::select(grid.city.center.dist, 2,3,9,8)

#Import 2016 (aa2a) VOC (because didn't save VOCs from abc run)

VOC.2016.model <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/modeling/model sensitivity analysis/figures/timeseries/Model sens - exceedance day means VOCs - all.csv", header=TRUE)
VOC.2016.model <- VOC.2016.model %>%
  dplyr::select(1:4,5) %>%
  dplyr::filter(time != "build.O3")

VOC.2016.model <- full_join(VOC.2016.model, grid.city.center.dist.narrow, by=c("x","y"))

VOC.2016.model.to.use <- VOC.2016.model %>%
  dplyr::filter(!is.na(city)) %>% #keep only cells within 100km of city center
  dplyr::filter(exceed.day == city | (city == "Minneapolis" & exceed.day == "Chicago")) #keep only exceedance days at each area - for Minneapolis, use Chicago exceed days


#Plot distance from city center
city.list <- unique(VOC.2016.model.to.use$city)
time.list <- unique(VOC.2016.model.to.use$time)

# for (j in city.list)
# {
#   NAA.subset <- subset(VOC.2016.model.to.use, VOC.2016.model.to.use$city == j)
# 
#   #Plot showing all monitors
#   a <- ggplot(data=NAA.subset) +
#     geom_point(aes(x=dist.km, y=VOC.2016, color=time, group=time), size=1) +
#     xlab("Distance from city center (km)") + ylab("Mean VOC Concentration (ppb)") + ggtitle(paste0("Mean VOC concentrations - ",j)) +
#     scale_color_brewer(palette = "Set1") +
#     scale_x_continuous(breaks = seq(0,100,by=20)) +
#     theme(axis.text = element_text(size = 14, color = "black"),
#           axis.text.x = element_text(size = 14,  vjust=0.5),
#           axis.title = element_text(size = 16),
#           legend.text = element_text(size = 8),
#           legend.title = element_blank(),
#           plot.title = element_text(size = 18, hjust = 0))
# 
#   ggsave(filename = paste0("Model VOC by distance from city center - all points-", j, ".png"), plot = a, width = 7, height =5)
# 
#   #Plot best-fit line
#   b <- ggplot(data=NAA.subset) +
#     geom_smooth(aes(x=dist.km, y=VOC.2016, color=time, group=time), se=FALSE) +
#     xlab("Distance from city center (km)") + ylab("Mean VOC Concentration (ppb)") + ggtitle(paste0("Mean VOC concentrations ",j)) +
#     geom_hline(yintercept = 0, color="darkgray") +
#     scale_color_brewer(palette = "Set1") +
#     scale_x_continuous(breaks = seq(0,100,by=20)) +
#     theme(axis.text = element_text(size = 14, color = "black"),
#           axis.text.x = element_text(size = 14,  vjust=0.5),
#           axis.title = element_text(size = 16),
#           legend.text = element_text(size = 8),
#           legend.title = element_blank(),
#           plot.title = element_text(size = 18, hjust = 0))
# 
#   ggsave(filename = paste0("Model VOC by distance from city center - best fit ", j, ".png"), plot = b, width = 7, height =5)
# }


#Plot average for each NAA - for all areas in the same plot - by time (peak VOC and peak O3)

time.list <- unique(VOC.2016.model.to.use$time)

# for (i in time.list)
# {
#   time.subset <- subset(VOC.2016.model.to.use, VOC.2016.model.to.use$time == i)
# 
#   b <- ggplot(data=time.subset) +
#     geom_smooth(aes(x=dist.km, y=VOC.2016, color=city, group=city), size=1, se=FALSE) +
#     xlab("Distance from city center (km)") + ylab("Mean VOC Concentration (ppb)") + ggtitle(paste0("Mean VOC concentrations - ", i)) +
#     geom_hline(yintercept = 0, color="darkgray") +
#     scale_color_brewer(palette = "Paired") +
#     theme(axis.text = element_text(size = 14, color = "black"),
#           axis.text.x = element_text(size = 14,  vjust=0.5),
#           axis.title = element_text(size = 16),
#           legend.text = element_text(size = 8),
#           legend.title = element_blank(),
#           plot.title = element_text(size = 18, hjust = 0))
#   ggsave(filename = paste0("Model VOC by distance from city center - best fit-all-", i, ".png"), plot = b, width = 7, height =5)
# }

VOC.2016.model.mean.dist <- VOC.2016.model.to.use %>%
  dplyr::mutate(km.bins = ifelse(dist.km <10, "0-10", ifelse(dist.km <20, "10-20", ifelse(dist.km <30, "20-30", ifelse(dist.km <40, "30-40", ifelse(dist.km <50, "40-50",
                                                                                                                                                    ifelse(dist.km <60, "50-60", ifelse(dist.km <70, "60-70", ifelse(dist.km <80, "70-80", ifelse(dist.km <90, "80-90", "90-100")))))))))) %>%
  dplyr::group_by(city,km.bins,time) %>%
  dplyr::summarize(mean.VOC.2016 = mean(VOC.2016, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(city = ifelse(city=="Minneapolis","Twin Cities",city))
# write.table(VOC.2016.model.mean.dist, "clipboard", sep="\t", row.names = FALSE)

    # for (i in time.list)
    # {
    #   time.subset <- subset(VOC.2016.model.mean.dist, VOC.2016.model.mean.dist$time == i)
    # 
    #   b <- ggplot(data=time.subset) +
    #     geom_line(aes(x=km.bins, y=mean.VOC.2016, color=city, group=city), size=1) +
    #     xlab("Distance from city center (km)") + ylab("Mean VOC Concentration (ppb)") + ggtitle(paste0("Mean VOC concentrations - ", i)) +
    #     geom_hline(yintercept = 0, color="darkgray") +
    #     scale_color_brewer(palette = "Paired") +
    #     scale_y_continuous(breaks = seq(0,180,by=20)) +
    #     theme(axis.text = element_text(size = 14, color = "black"),
    #           axis.text.x = element_text(size = 14,  vjust=0.5, angle=90),
    #           axis.title = element_text(size = 16),
    #           legend.text = element_text(size = 12),
    #           legend.title = element_blank(),
    #           panel.background = element_rect(fill = "white", color = "black"),
    #           legend.key=element_rect(fill="white"),
    #           panel.grid.major = element_line(color = "gray90"),
    #           panel.grid.minor = element_line(color = "gray90"),
    #           plot.title = element_text(size = 18, hjust = 0))
    #   ggsave(filename = paste0("Model VOC by distance from city center - binned-all-", i, ".png"), plot = b, width = 6, height =4)
    # }



# PLOT TRENDS IN ANTHROPOGENIC VOC EMISSIONS (BECAUSE MONITORING DATA IS SPARSE) OVER TIME - but these are missing biogenics, which are really big
# emissions data from: https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data

VOC.emissions <- read.csv("C:/Users/afdic/OneDrive/Documents/emissions/state_tier1_caps-national emissions trends through 2021.csv", header=TRUE)

VOC.emissions.LADCO <- VOC.emissions %>%
  dplyr::filter(Pollutant == "VOC" & State %in% c("IL","IN","MI","MN","OH","WI")) %>%
  pivot_longer(emissions90:emissions21, names_to = "year", values_to = "tons.1000") %>%
  dplyr::mutate(year = as.numeric(substr(year, 10, 11))) %>%
  dplyr::mutate(year = year + ifelse(year >=90, 1900, 2000)) %>%
  dplyr::group_by(State, year) %>%
  dplyr::summarize(total.tons.1000 = sum(tons.1000, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1995, "1990", ifelse( year <= 2000, "1996-00", ifelse(year <= 2005, "2001-05", 
                          ifelse(year <= 2010, "2006-10", ifelse(year <= 2015, "2011-15", "2016-21")))))) %>%
  dplyr::group_by(State, yr.bins) %>%
  dplyr::summarize(mean.1000.tons = mean(total.tons.1000, na.rm=TRUE)) %>%
  dplyr::ungroup()
# write.table(VOC.emissions.LADCO, "clipboard", sep="\t", row.names = FALSE)

#Plot anthropogenic VOC emissions by year group
    # a <- ggplot(data=VOC.emissions.LADCO) +
    #   geom_line(aes(x=yr.bins, y=mean.1000.tons, color=State, group=State), size=1) +
    #   xlab(NULL) + ylab("Mean VOC emissions (1000 tons)") + ggtitle(paste0("Mean Anthropogenic VOC Emissions")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,1000) +
    #   scale_color_brewer(palette = "Paired") +
    #   scale_y_continuous(breaks = seq(0,1000,by=100)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("VOC emissions means by year bins.png"), plot = a, width = 6, height =4)

#attempt to account for ozone season biogenic emissions - add onto anthropogenics using annual LADCO model numbers for 2016 (provided by Zac via email, 7/8/22)
VOC.emiss.biogenic <- data.frame(State = c("IL","IN","MI","MN","OH","WI"), biogenic.1000.tons = c(422.736, 279.976, 593.916, 510.385, 360.156, 484.780))

VOC.emissions.LADCO <- full_join(VOC.emissions.LADCO, VOC.emiss.biogenic, by="State")
VOC.emissions.LADCO <- VOC.emissions.LADCO %>%
  dplyr::rename(anthrop.1000.tons = mean.1000.tons) %>%
  dplyr::mutate(total.1000.tons = anthrop.1000.tons + biogenic.1000.tons)

write.table(VOC.emissions.LADCO, "clipboard", sep="\t", row.names = FALSE)

#Plot anthropogenic VOC emissions by year group
    # a <- ggplot(data=VOC.emissions.LADCO) +
    #   geom_line(aes(x=yr.bins, y=total.1000.tons, color=State, group=State), size=1) +
    #   xlab(NULL) + ylab("Mean VOC emissions (1000 tons)") + ggtitle(paste0("Mean Total VOC Emissions")) +
    #   # annotate("text", x= 4, y=ylim, label=j, size = 6, hjust=1, vjust=1) +
    #   geom_hline(yintercept = 0, color="darkgray") +
    #   ylim(0,1600) +
    #   scale_color_brewer(palette = "Paired") +
    #   scale_y_continuous(breaks = seq(0,1600,by=200)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         panel.background = element_rect(fill = "white", color = "black"),
    #         legend.key=element_rect(fill="white"),
    #         panel.grid.major = element_line(color = "gray90"),
    #         panel.grid.minor = element_line(color = "gray90"),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Total VOC emissions means by year bins.png"), plot = a, width = 6, height =4)



