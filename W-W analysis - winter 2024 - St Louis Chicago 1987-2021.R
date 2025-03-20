################################################################################################
#                                                                                              #
#         CALCULATING WEEKDAY-WEEKEND DIFFERENCES: ST. LOUIS & CHICAGO 1992-2021               #
#EXTENDING ANALYSIS TO LONGER TIME PERIOD USING OZONE-CONDUCIVE MET FROM ORIGINAL CART ANALYSIS#
#                   APPLY WEEKDAY-WEEKEND ANALYSIS TO INDIVIDUAL MONITORS                      #
#                              WINTER 2024 - for publication                                   #
#                                                                                              #
################################################################################################


require("tidyverse")
require("gridExtra")
require("RColorBrewer")
require("Hmisc")
require("cowplot")
require("gmodels") #for the chi-squared test
require("broom") #to "tidy" the statistical test
require("berryFunctions") #allows saving the warning indications for chi squared test


## IMPORT MDA8 VALUES 1987-2021 (used for trends over space and time analysis)
#exported from AQS in state pairs. When I tried to do all the NAAs together, it didn't download all the data for some reason.
#Below narrow to just St. Louis (IL-MO) and Chicago (IL-IN-WI)

setwd("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time")

## Import 2001-21 data
MDA8.down.IL.IN <- read.table("./datafiles/AMP350MX_2010479-IL-IN 2001-21.txt", sep="|", header=TRUE, fill=TRUE)
MDA8.down.MN.WI <- read.table("./datafiles/AMP350MX_2010573-MN-WI 2001-21.txt", sep="|", header=TRUE, fill=TRUE)
MDA8.down.KY.OH <- read.table("./datafiles/AMP350MX_2010569-KY-MO 2001-21.txt", sep="|", header=TRUE, fill=TRUE) #Had to search for and replace a "'" in the document for it to read

setwd("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/weekday-weekend")
MDA8.down.1987.2000 <- read.table("./datafiles/AMP350MX_2156832- MDA8s 1987-2000.txt", sep="|", header=TRUE, fill=TRUE)

## Import 1987-2000 data
MDA8.data <- rbind(MDA8.down.1987.2000, MDA8.down.IL.IN, MDA8.down.MN.WI, MDA8.down.KY.OH)
rm(MDA8.down.1987.2000, MDA8.down.IL.IN, MDA8.down.MN.WI, MDA8.down.KY.OH)

MDA8.data <- MDA8.data %>%
  dplyr::filter(RAW.DATA.TYPE...2 == 2) %>%
  dplyr::select(2:4,6,17,18) %>%
  dplyr::mutate(COUNTY_CODE = str_pad(COUNTY_CODE, 3, pad = "0"), SITE_ID = str_pad(SITE_ID, 4, pad = "0"), Site = paste0(STATE_CODE, COUNTY_CODE, SITE_ID),
                SAMPLE_DATE = as.Date(as.character(SAMPLE_DATE), "%Y%m%d"), year = format(SAMPLE_DATE, "%Y"), month = format(SAMPLE_DATE, "%m"),
                VALUE_0 = as.numeric(VALUE_0)*1000) %>%
  dplyr::rename(State=STATE_CODE, County=COUNTY_CODE, Monitor=SITE_ID, Date = SAMPLE_DATE, MDA8 = VALUE_0) %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  dplyr::mutate(Site=as.numeric(Site))

#Import closest city label (All monitors within 100 km. Also lat/long, distance to city center and to a lake, but I won't use these now)
lat.long.city <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Ozone monitor sites 1989-2022-city centers-lake dist.csv", header=TRUE) #import most monitors
lat.long.missing <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Ozone monitor sites - missing_dist to cities-lakes.csv", header=TRUE) #import monitors missing from above
lat.long.city <- rbind(lat.long.city, lat.long.missing)
lat.long.Lo <- lat.long.city %>% #modify Cooley Canal line for retired Lo Serv. site (which it replaced)
  dplyr::filter(Site == 390950035) %>%
  dplyr::mutate(Site = 390950034)
lat.long.city <- rbind(lat.long.city, lat.long.Lo)
city.codes <- data.frame(NEAR_FID = seq(1,7,by=1), city = c("Chicago","Cincinnati","Cleveland","Detroit","Louisville","Minneapolis","St. Louis"))


MDA8.data <- left_join(MDA8.data, lat.long.city, by="Site")
MDA8.data <- left_join(MDA8.data, city.codes, by="NEAR_FID")

#Narrow to just monitors within 100km of a city
city.O3 <- MDA8.data %>%
  dplyr::filter(!is.na(city) & NEAR_DIST < 100000) %>%
  dplyr::select(17,7,4,5,6)

#Import site data
site.info <- read.csv("R5-plus ozone monitoring names and codes-Nov2021-with MN.csv", header=TRUE)
site.info <- dplyr::select(site.info, 1,2)

#Add monitor names for most monitors
city.O3 <- left_join(city.O3, site.info, by="Site")
city.O3 <- city.O3 %>%
  dplyr::mutate(year = as.numeric(format(Date, "%Y")), weekday = format(Date, "%a"))

#Remove 2016 Cooley Canal data (have for August-September, when LOW SER was also operating)
city.O3 <- city.O3 %>%
  dplyr::filter(!(Site == 390950035 & year == 2016))

# #Check which monitors from NAA record are missing from this record. (Know this one is missing St. Louis's Alton monitor (171190008))
# NAA.O3.complete <- read.csv("city 100 km MDA8 ozone 2002-2020 - 10-yr mons.csv", header=TRUE) #with new set of ozone monitors
# NAA.O3.complete <- NAA.O3.complete %>%
#   dplyr::select(5,1,6) %>%
#   dplyr::distinct()
# NAA.city.diffs <- left_join(NAA.O3.complete, city.O3, by=c("Site"))
# NAA.city.diffs <- NAA.city.diffs %>%
#   dplyr::select(1:4) %>%
#   dplyr::distinct() %>%
#   dplyr::filter(is.na(city))

#Change site labels for NINE monitors that moved during the record (added in Hamilton/Crawford Woods in Cincinnati: shifted to Crawford Woods in 2017. Rename as Crawford Woods)
city.O3 <- city.O3 %>%
  dplyr::mutate(Site = ifelse(Site == 171190008, 171190120, ifelse(Site == 211110027, 211110080, ifelse(Site == 551010017, 551010020,
                              ifelse(Site == 391030003, 391030004, ifelse(Site == 390950034, 390950035, ifelse(Site == 391550009, 391550013,
                                     ifelse(Site == 390170004, 390170023,
                                            ifelse(Site == 170831001, 170830117, ifelse(Site == 291130003, 291130004, Site))))))))),
                Site.Name.new = ifelse(Site == 171190120, "Alton-HM Sch", ifelse(Site == 211110080, "Carrithers MS",
                                       ifelse(Site == 551010020, "Racine-P&D", ifelse(Site == 391030004, "Chippewa Lake",
                                              ifelse(Site == 390950035, "Cooley Canal", ifelse(Site == 391550013, "Kinsman",
                                                     ifelse(Site == 390170023, "Crawford Woods",
                                                            ifelse(Site == 170830117, "Jerseyville", ifelse(Site == 291130004, "Foley West", Site.Name.new))))))))),
                Site.Name.new = ifelse(is.na(Site.Name.new), Site, Site.Name.new)) # substitute site number for name if name is missing


# #Change site labels for monitors that moved during the record (added in Hamilton/Crawford Woods in Cincinnati: shifted to Crawford Woods in 2017. Rename as Crawford Woods)
# city.O3 <- city.O3 %>%
#   dplyr::mutate(Site = ifelse(Site == 171190008, 171190120, ifelse(Site == 211110027, 211110080, ifelse(Site == 551010017, 551010020,
#                               ifelse(Site == 391030003, 391030004, ifelse(Site == 390950034, 390950035, ifelse(Site == 391550009, 391550013,
#                                      ifelse(Site == 390170004, 390170023, Site))))))),
#                 Site.Name.new = ifelse(Site == 171190120, "Alton-HM Sch", ifelse(Site == 211110080, "Carrithers MS",
#                                        ifelse(Site == 551010020, "Racine-P&D", ifelse(Site == 391030004, "Chippewa Lake",
#                                               ifelse(Site == 390950035, "Cooley Canal", ifelse(Site == 391550013, "Kinsman",
#                                                      ifelse(Site == 390170023, "Crawford Woods", Site.Name.new))))))),
#                 Site.Name.new = ifelse(is.na(Site.Name.new), Site, Site.Name.new)) # substitute site number for name if name is missing
#
city.O3.NA <- city.O3 %>%
  dplyr::filter(is.na(Site.Name.new)) %>%
  dplyr::select(1,2) %>%
  dplyr::distinct()


#Determine completeness of different monitors' records - and select only monitors with at least 10 years of data from 1987-2021
#(Originally selected only monitors with at least 18 years of data & data in 2020)
city.complete <- city.O3 %>%
  dplyr::group_by(Site, Site.Name.new, city) %>%
  # dplyr::filter(year>2001) %>%
  dplyr::summarize(yr.count = n_distinct(year), start=min(year), end=max(year)) %>%
  dplyr::filter(yr.count >=10) %>%
  #dplyr::filter(end == 2020) %>%
  ungroup()
city.complete.list <- city.complete$Site

city.O3.complete <- city.O3 %>%
  dplyr::filter(Site %in% city.complete.list) %>% #just select data for complete monitors (>10 years of data)
  dplyr::mutate(Site.Name.new = ifelse(Site==170310064, "Univ. Chicago", ifelse(Site==170310072, "Jardine", ifelse(Site==180890030, "Whiting",
                ifelse(Site==391550009, "Kinsman", Site.Name.new))))) %>% #fill in missing site names for discontinued monitors
  dplyr::filter(city != "Minneapolis") #remove Twin Cities data


#Remove duplicate POCs (select long-term POC - determined by inspecting record)
city.O3.complete <- city.O3.complete %>%
  filter(!(Site.Name.new=="Northbrook" & POC==2) & !(Site.Name.new=="Detroit-E 7 Mile" & POC==1) & !(Site.Name.new=="Oak Park" & POC==1) &
           !(Site.Name.new=="Orchard Farm" & POC==2) & !(Site.Name.new=="West Alton" & POC==2) & !(Site.Name.new=="Blair Street" & POC==2) &
           !(Site.Name.new=="Maryland Heights" & POC==2) & !(Site.Name.new=="Pacific" & POC==2))

#Select just St. Louis and Chicago data
St.Louis.ozone.MDA8 <- city.O3.complete %>%
  dplyr::filter(city == "St. Louis")
# write.csv(St.Louis.ozone.MDA8, "St Louis MDA8 ozone 1987-2021 - 10-yr mons.csv", row.names = FALSE)

Chicago.ozone.MDA8 <- city.O3.complete %>%
  dplyr::filter(city == "Chicago")
# write.csv(Chicago.ozone.MDA8, "Chicago MDA8 ozone 1987-2021 - 10-yr mons.csv", row.names = FALSE)



## RUN WEEKDAY-WEEKEND ANALYSIS FOR 1987-2021 FOR INDIVIDUAL MONITORS #################################################################

## ST. LOUIS: RUN WEEKDAY-WEEKEND ANALYSIS ON CART RESULTS AND DAILY OZONE ############################################################
#Use CART analysis previously run for these sites

#Use a wider set of ozone data than used for the CART analysis

St.Louis.O3 <- read.csv("St Louis MDA8 ozone 1987-2021 - 10-yr mons.csv", header=TRUE)
St.Louis.O3 <- St.Louis.O3 %>%
  dplyr::mutate(Date = as.Date(Date))

#Load ozone-conducive days (determined from airport met using criteria determined from CART for 2001-2020 in "St Louis-Chicago O3-conducive days - extended to 1987-2021.R")

STL.high.O3.days <- read.csv("St Louis - ozone conducive days - 1987-2021.csv", header = TRUE)
STL.high.O3.days <- STL.high.O3.days %>%
  dplyr::mutate(date = as.Date(date))

St.Louis.high.O3.nodes <- inner_join(STL.high.O3.days, St.Louis.O3, by = c("date"="Date"))

# #CART data
# node.day <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/CART/2001-2020 CART - for W-W/St Louis CART data - 2001-2020.csv", header=TRUE)
# 
# St.Louis.narrow <- node.day %>%
#   dplyr::select(Date, month, year, node) %>%
#   distinct() %>%
#   mutate(Date = as.POSIXct(Date))
# 
# St.Louis.O3.CART <- left_join(St.Louis.O3, St.Louis.narrow, by = c("Date","year"))
# St.Louis.O3.CART <- filter(St.Louis.O3.CART, !is.na(node))
# 
# ##LOOK AT HIGH-OZONE NODES (#14,20,21,27, defined as mean ozone > 60 ppb). ALSO RUN FOR JUST NODES >65 ppb (top-2 nodes - 20 and 21) --> USE THIS
# #Decided to use data without Saturdays, with 2020, and to look at it both as an average of all nodes and by individual nodes.
# # --> Run code both ways: average of all nodes and by individual nodes. Will need to modify the code slightly.

unique(St.Louis.high.O3.nodes$Site)

St.Louis.high.O3.nodes.means <- St.Louis.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                  ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

St.Louis.high.O3.nodes.means.long <- pivot_longer(St.Louis.high.O3.nodes.means, weekday.mean:weekend.count,
                                                  names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")

# #Plot of mean MDA8 per year/group bin
St.Louis.high.O3.no1987 <- dplyr::filter(St.Louis.high.O3.nodes.means.long, yr.bins != "1987-91")


#get change in mean MDA8 for each monitor - only monitors operating the whole time
St.Louis.high.O3.no1987.means.summary <- St.Louis.high.O3.no1987 %>%
  dplyr::select(-count) %>%
  pivot_wider(names_from = day.type, values_from = mean) %>%
  dplyr::group_by(city, Site, Site.Name.new) %>%
  dplyr::mutate(start.yr.bin = min(yr.bins), end.yr.bin = max(yr.bins)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(yr.bins == "1992-96" | yr.bins == "2017-21") %>%
  dplyr::group_by(city, Site, Site.Name.new) %>%
  dplyr::summarize(wd.change = weekday[yr.bins=="1992-96"] - weekday[yr.bins=="2017-21"],
                   we.change = weekend[yr.bins=="1992-96"] - weekend[yr.bins=="2017-21"]) %>%
  dplyr::ungroup() 
mean(St.Louis.high.O3.no1987.means.summary$wd.change, na.rm = TRUE)

    # c <- ggplot(data = St.Louis.high.O3.no1987) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Site) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   # scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - St. Louis\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 12, color = "black"),
    #         axis.text.x = element_text(size = 12, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 14),
    #         legend.text = element_text(size = 10),
    #         legend.title = element_blank(),
    #         strip.text = element_text(size = 10),
    #         plot.title = element_text(size = 16, hjust = 0))
    # ggsave(filename = "St Louis mean MDA8 fixed yrs- 1992-21 - high O3 nodes-means-by mon-for SI.png", plot=c, width = 9, height = 6.5)
# 
# 
# #Plot of number of days per year/group/node bin
#     c <- ggplot(data = St.Louis.high.O3.nodes.means.long) +
#       geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
#       # geom_hline(yintercept = 70, color="darkgray") +
#       # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
#       scale_linetype_manual(values = c("dashed","solid")) +
#       xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - St. Louis\nHigh O3 Nodes")) +
#       scale_color_brewer(palette="Paired") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title.y = element_text(size = 16),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank(),
#             plot.title = element_text(size = 18, hjust = 0))
#     ggsave(filename = paste0("St Louis count fixed yrs- 1987-21 - high O3 nodes-means-by mon.png"), plot=c, width = 9, height = 6.5)
# 
# 
# #weekday-weekend diff (%) plot
#     a <- ggplot(data = St.Louis.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
#       geom_line(aes(x=yr.bins, y=wd.we.diff, color=Site.Name.new, group=Site.Name.new), size=1) + #for mean plots
#       xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nSt. Louis High O3 Nodes")) +
#       # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
#       # scale_color_brewer(palette="Paired") +
#       scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(23)) +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title.y = element_text(size = 16),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank(),
#             plot.title = element_text(size = 18, hjust = 0))
#     ggsave(filename = paste0("St Louis - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 - high O3 nodes-means-by mon.png"), plot=a, width = 9, height = 6.5)


# #Run Welch's t-test and save statistics in combined data frame. 
# St.Louis.high.O3.nodes <- St.Louis.high.O3.nodes %>%
#   dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
#                           ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))),
#                 day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>%
#   dplyr::filter(day != "other") 
# 
# St.Louis.means.wd <- filter(St.Louis.high.O3.nodes, day == "weekday")
# St.Louis.means.we <- filter(St.Louis.high.O3.nodes, day == "weekend")
# 
# mon.list <- unique(St.Louis.high.O3.nodes.means.long$Site.Name.new) #will be i below
# # mon.list <- mon.list[mon.list != "Arnold West"] #will have to run this monitor separately because it's missing one year set
# # yr.list <- unique(St.Louis.high.O3.nodes.means.long$yr.bins) #will be k below
# 
# t.test.St.Louis <- data.frame() #creates blank data frame that will be filled below
# 
# # # #setup for Arnold West (no 2002-06)
# # mon.list <- "Arnold West"
# # yr.list <- c("2007-11","2012-16","2017-21")
# # yr.list <- c("2012-16","2017-21") #use for individual node analysis because not enough days in 2007-11
# 
# for (i in mon.list)
# {
#   NAA.mon.subset.wd <- subset(St.Louis.means.wd, St.Louis.means.wd$Site.Name.new == i)
#   NAA.mon.subset.we <- subset(St.Louis.means.we, St.Louis.means.we$Site.Name.new == i)
#   #Determine which year groups had > 2 days to allow the chi-squared test and use this to determine which years to loop over
#   yrs.enough.days <- NAA.mon.subset.we %>%
#     dplyr::group_by(yr.bins) %>%
#     dplyr::summarise(n.days = length(date)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(n.days > 1)
#   yr.list <- unique(yrs.enough.days$yr.bins)
#   
#   # for (j in node.list)
#   # {
#   #   NAA.group.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$node == j)
#   #   NAA.group.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$node == j)
# 
#   for (k in yr.list)
#   {
#     NAA.group.yr.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$yr.bins == k)
#     NAA.group.yr.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$yr.bins == k)
#     
#     t.test.results <- t.test(NAA.group.yr.subset.wd$MDA8, NAA.group.yr.subset.we$MDA8) #runs test
#     t.test.result.temp <- tidy(t.test.results) #converts results into tidy form (data frame)
#     t.test.result.temp$Site.Name.new <- i
#     # t.test.result.temp$node <- j
#     t.test.result.temp$yr.bins <- k
#     
#     # options(warn = 2) #changes warnings to errors for below
#     # chi.result.temp$warn <- berryFunctions::is.error(chisq.test(NAA.group.yr.subset$day, NAA.group.yr.subset$exceed, correct = FALSE))
#     # options(warn = 0) #changes warnings back into warnings (not errors)
#     
#     t.test.St.Louis <- rbind(t.test.St.Louis, t.test.result.temp)
#     
#   }
# }
# # }
# 
# #Add t-test results to means and export:
# St.Louis.high.O3.nodes.t.test <- full_join(St.Louis.high.O3.nodes.means, t.test.St.Louis, by=c("yr.bins","Site.Name.new"))
# # write.csv(St.Louis.high.O3.nodes.t.test, "St Louis W-W diffs-means-by mon-1987-2021.csv", row.names = FALSE)


# Make plots of monitors grouped by clusters - weekday means and W-W differences (for paper SI)
#Import cluster labels
cluster.labels <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Cluster monitors lat-long.csv", header=TRUE)

St.Louis.high.O3.no1987 <- left_join(St.Louis.high.O3.no1987, cluster.labels, by="Site")
St.Louis.high.O3.no1987 <- St.Louis.high.O3.no1987 %>%
  dplyr::mutate(Cluster = ifelse(Site == 171191009, 2, Cluster)) #add labels for old Maryville moniter 

#add final cluster names
cluster.names <- data.frame(city = c(rep("Chicago",10), rep("St. Louis",9)),
                            Cluster = c("1","2a","2b","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","A"), 
                            Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE","64N","22","26N","44NW","55S*","84S","3","91N","10W"))
St.Louis.high.O3.no1987 <- left_join(St.Louis.high.O3.no1987, cluster.names, by=c("city","Cluster"))
St.Louis.high.O3.no1987.wd <- St.Louis.high.O3.no1987 %>%
  dplyr::filter(day.type == "weekday") %>% #select just weekday means for plotting
  dplyr::mutate(Site = as.character(Site), Cluster.name = factor(Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N")))

    # #Plot of weekday mean MDA8
    # a <- ggplot(data=St.Louis.high.O3.no1987.wd, aes(x=yr.bins, y=mean, color=Site, group=Site)) + geom_line() + facet_wrap(.~Cluster.name) +
    #   ylab("Mean Weekday MDA8 Ozone (ppb)") + xlab(NULL) + ggtitle("St. Louis Monitor Mean Weekday MDA8 by Cluster") +
    #         theme(axis.text = element_text(size = 14, color = "black"),
    #               axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #               axis.title.y = element_text(size = 16),
    #               legend.text = element_text(size = 12),
    #               legend.title = element_blank(),
    #               plot.title = element_text(size = 18, hjust = 0))
    #       ggsave(filename = paste0("St Louis - Weekday mean MDA8 by mon grouped by cluster.png"), plot=a, width = 9,
    #              height = 6.5)
    # 
    # #Plot of mean W-W differences          
    # a <- ggplot(data=St.Louis.high.O3.no1987.wd, aes(x=yr.bins, y=wd.we.diff, color=Site, group=Site)) + geom_line() + facet_wrap(.~Cluster.name) +
    #   ylab("Weekday-Weekend MDA8 Difference (ppb)") + xlab(NULL) + ggtitle("St. Louis Weekday-Weekend Diff. by Cluster") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("St Louis - Weekday-Weekend MDA8 diff by mon grouped by cluster.png"), plot=a, width = 9,
    #        height = 6.5)



## ST. LOUIS - RUN WEEKDAY-WEEKEND ANALYSIS ON MONITOR CLUSTERS ####################################

#Import cluster labels
cluster.labels <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Cluster monitors lat-long.csv", header=TRUE)

St.Louis.high.O3.nodes <- left_join(St.Louis.high.O3.nodes, cluster.labels, by="Site")
St.Louis.high.O3.nodes <- St.Louis.high.O3.nodes %>%
  dplyr::mutate(Cluster = ifelse(Site == 171191009, 2, Cluster)) #add labels for old Maryville moniter 

#add final cluster names
cluster.names <- data.frame(city = c(rep("Chicago",10), rep("St. Louis",9)),
                            Cluster = c("1","2a","2b","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","A"), 
                            Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE","64N","22","26N","44NW","55S*","84S","3","91N","10W"))
St.Louis.high.O3.nodes <- left_join(St.Louis.high.O3.nodes, cluster.names, by = c("city","Cluster"))


#Calculate means and differences by cluster and plot
#Updating code from "weekday-weekend analysis - fall 2023 - CART by monitor CLUSTERS-all 100km mons.R"

St.Louis.high.O3.nodes.means <- St.Louis.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Cluster, Cluster.name, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

St.Louis.high.O3.nodes.means.long <- pivot_longer(St.Louis.high.O3.nodes.means, weekday.mean:weekend.count,
                                                  names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")

# #Plot of mean MDA8 per year/group bin
#     c <- ggplot(data = St.Louis.high.O3.nodes.means.long) +
#       geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Cluster.name) + #for mean plots
#       geom_hline(yintercept = 70, color="darkgray") +
#       # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
#       scale_linetype_manual(values = c("dashed","solid")) +
#       xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - St. Louis clusters\nHigh O3 Nodes")) +
#       scale_color_brewer(palette="Paired") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title.y = element_text(size = 16),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank(),
#             plot.title = element_text(size = 18, hjust = 0))
#     ggsave(filename = "St Louis clusters mean MDA8 fixed yrs- 1987-21 - 100km.png", plot=c, width = 9, height = 6.5)
# 
# 
# #Plot of number of days per year/group/node bin
#     c <- ggplot(data = St.Louis.high.O3.nodes.means.long) +
#       geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Cluster.name) + #for mean plots
#       # geom_hline(yintercept = 70, color="darkgray") +
#       # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
#       scale_linetype_manual(values = c("dashed","solid")) +
#       xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - St. Louis clusters\nHigh O3 Nodes")) +
#       scale_color_brewer(palette="Paired") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title.y = element_text(size = 16),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank(),
#             plot.title = element_text(size = 18, hjust = 0))
#     ggsave(filename = paste0("St Louis clusters count fixed yrs- 1987-21 - 100km.png"), plot=c, width = 9, height = 6.5)
# 
# 
# # #weekday-weekend diff (%) plot
#     a <- ggplot(data = St.Louis.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
#       geom_line(aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=1) + #for mean plots
#       xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nSt. Louis clusters")) +
#       # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
#       scale_color_brewer(palette="Paired") +
#       # scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(14)) +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title.y = element_text(size = 16),
#             legend.text = element_text(size = 12),
#             legend.title = element_blank(),
#             plot.title = element_text(size = 18, hjust = 0))
#     ggsave(filename = paste0("St Louis clusters - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 -100km.png"), plot=a, width = 9, height = 6.5)


#Run Welch's t-test and save statistics in combined data frame. 
St.Louis.high.O3.nodes <- St.Louis.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS,
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>%
  dplyr::filter(day != "other") 

St.Louis.means.wd <- filter(St.Louis.high.O3.nodes, day == "weekday")
St.Louis.means.we <- filter(St.Louis.high.O3.nodes, day == "weekend")

cluster.list <- unique(St.Louis.high.O3.nodes.means.long$Cluster.name)


t.test.St.Louis <- data.frame() #creates blank data frame that will be filled below

for (i in cluster.list)
{
  NAA.mon.subset.wd <- subset(St.Louis.means.wd, St.Louis.means.wd$Cluster.name == i)
  NAA.mon.subset.we <- subset(St.Louis.means.we, St.Louis.means.we$Cluster.name == i)
  #Determine which year groups had > 2 days to allow the chi-squared test and use this to determine which years to loop over
  yrs.enough.days <- NAA.mon.subset.we %>%
    dplyr::group_by(yr.bins) %>%
    dplyr::summarise(n.days = length(date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n.days > 1)
  yr.list <- unique(yrs.enough.days$yr.bins)
  
  
  # for (j in node.list)
  # {
  #   NAA.group.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$node == j)
  #   NAA.group.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$node == j)
  
  for (k in yr.list)
  {
    NAA.group.yr.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$yr.bins == k)
    NAA.group.yr.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$yr.bins == k)
    
    t.test.results <- t.test(NAA.group.yr.subset.wd$MDA8, NAA.group.yr.subset.we$MDA8) #runs test
    t.test.result.temp <- tidy(t.test.results) #converts results into tidy form (data frame)
    t.test.result.temp$Cluster.name <- i
    # t.test.result.temp$node <- j
    t.test.result.temp$yr.bins <- k
    
    # options(warn = 2) #changes warnings to errors for below
    # chi.result.temp$warn <- berryFunctions::is.error(chisq.test(NAA.group.yr.subset$day, NAA.group.yr.subset$exceed, correct = FALSE))
    # options(warn = 0) #changes warnings back into warnings (not errors)
    
    t.test.St.Louis <- rbind(t.test.St.Louis, t.test.result.temp)
    
  }
}
# }


#Add t-test results to means and export:
St.Louis.high.O3.nodes.t.test <- full_join(St.Louis.high.O3.nodes.means, t.test.St.Louis, by=c("yr.bins","Cluster.name"))
# write.csv(St.Louis.high.O3.nodes.t.test, "St Louis cluster W-W diffs-means-by cluster - 1987-2021.csv", row.names = FALSE)


# ST. LOUIS: MAKE PLOTS FOR THE PAPER ########################################################################
#Modified from "weekday-weekend analysis LADCO CLUSTERS - with CART-combining - Oct 2023.R"

St.Louis.high.O3.nodes <- read.csv("St Louis cluster W-W diffs-means-by cluster - 1987-2021.csv", header = TRUE)

## REMOVE 1987-91 !!!!!!!!!!!!!!!!!!!!!!!!!!!!
St.Louis.high.O3.nodes <- dplyr::filter(St.Louis.high.O3.nodes, yr.bins != "1987-91")

#Remove year groups with < 9 days in them (to be consistent with cut for Chicago)

St.Louis.high.O3.gt10d <- St.Louis.high.O3.nodes %>%
  dplyr::filter(weekend.count >= 9)

St.Louis.high.O3.gt10d.long <- St.Louis.high.O3.gt10d %>%
  dplyr::select(1:8) %>%
  pivot_longer(weekday.mean:weekend.count, names_to = c("day",".value"), names_pattern = "([a-z]*)\\.([a-z]*)")

St.Louis.signif <- dplyr::filter(St.Louis.high.O3.gt10d, p.value <= 0.05)

#St. Louis plots

St.Louis.high.O3.gt10d$Cluster.name <- factor(St.Louis.high.O3.gt10d$Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N"))
St.Louis.high.O3.gt10d.long$Cluster.name <- factor(St.Louis.high.O3.gt10d.long$Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N"))
St.Louis.signif$Cluster.name <- factor(St.Louis.signif$Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N"))

St.Louis.pal <- c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#FCF45D","#C2E31E","#ABDDA4","#66C2A5","#3288BD") #make two middle colors darker (same palette as for trends plots)

# #MDA8 trend
    a <- ggplot(data = St.Louis.high.O3.gt10d.long) + #coord_fixed(1/9) +
      geom_line(aes(x=yr.bins, y=mean, color=Cluster.name, linetype=day, group=interaction(Cluster.name,day)), size=1) +
      scale_y_continuous(limits = c(45,80), breaks= seq(45,80,by=5)) +
      scale_linetype_manual(values = c("dashed","solid")) +
      xlab(NULL) + ylab("Mean MDA8 (ppb)") +
      annotate("text", x= 4, y=78, label="St. Louis", size = 6, hjust=1, vjust=0) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = St.Louis.pal) +
      guides(color = FALSE, linetype = guide_legend()) +
      #guides(color=guide_legend(ncol=2)) +
      #guides(linetype=guide_legend(ncol=2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.position = c(0.18,0.15),
            legend.key = element_blank(),
            legend.background = element_rect(color = "black"),
            legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))

#weekday-weekend diff (%) plot - WITH T-TEST RESULTS
    b <- ggplot() + geom_line(data = St.Louis.high.O3.gt10d, aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=1) +
      geom_point(data = St.Louis.signif, aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=3) +
      geom_hline(yintercept = 0, color="darkgray") + #coord_fixed(1/6.5) +
      annotate("text", x= 0.6, y=8, label="St. Louis", size = 6, hjust=0, vjust=0) +
      xlab(NULL) + ylab("W-W Mean MDA8 Diff. (ppb)") +
      scale_y_continuous(limits = c(-12.7, 9), breaks = seq(-12,8,by=2)) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = St.Louis.pal) +
      guides(color = guide_legend(override.aes = list(shape = NA))) +
      # guides(shape=FALSE, size=FALSE) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))

    # Combine and save plots
    
    Stl.WW.plot <- plot_grid(a,b, nrow=1, rel_widths = c(1,1.25), labels = c("(a)","(b)"), label_x = -0.025)
    # ggsave(filename = "St Louis W-W MDA8-diff plot 1987-2021.png", plot=Stl.WW.plot, width = 8, height = 4)
    ggsave(filename = "St Louis W-W MDA8-diff plot 1992-2021.png", plot=Stl.WW.plot, width = 8, height = 4)
    
  
# MAKE MDA8 VALUE PLOT - JUST FOR ST. LOUIS, FACETED BY CLUSTER   
      
    c <- ggplot(data = St.Louis.high.O3.gt10d.long) + facet_wrap(~Cluster.name, ncol=3) + #coord_fixed(1/9) +
      geom_line(aes(x=yr.bins, y=mean, color=Cluster.name, linetype=day, group=interaction(Cluster.name,day)), size=1) +
      scale_y_continuous(limits = c(46,85), breaks= seq(50,85,by=5)) +
      scale_linetype_manual(values = c("dashed","solid")) +
      xlab(NULL) + ylab("Mean MDA8 (ppb)") +
      # annotate("text", x= 7, y=90, label="St. Louis", size = 6, hjust=1, vjust=0) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = St.Louis.pal) +
      guides(color = FALSE, linetype = guide_legend()) +
      # guides(color=guide_legend(ncol=3)) +
      #guides(linetype=guide_legend(ncol=2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            strip.text = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.position = c(0.89,0.23),
            legend.key = element_blank(),
            legend.background = element_rect(color = "black"),
            legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    # ggsave(filename = "St Louis W-W conc plot 1987-2021.png", plot=c, width = 6, height = 6.2)
    ggsave(filename = "St Louis W-W conc plot 1992-2021.png", plot=c, width = 6, height = 6.2)
    
    

    
    
    
## RUN WEEKDAY-WEEKEND ANALYSIS FOR 1987-2021 FOR INDIVIDUAL MONITORS #################################################################

## CHICAGO: RUN WEEKDAY-WEEKEND ANALYSIS ON CART RESULTS AND DAILY OZONE ############################################################
#Use CART analysis previously run for these sites

#Use a wider set of ozone data than used for the CART analysis

Chicago.O3 <- read.csv("Chicago MDA8 ozone 1987-2021 - 10-yr mons.csv", header=TRUE)
Chicago.O3 <- Chicago.O3 %>%
  dplyr::filter(!(Site %in% c(170971002,170310042,551010042))) %>% #remove 3 sites - Racine is too far away, Sears Tower too unusual, and the third site was missed for trends for some reason so exclude here
  dplyr::mutate(Date = as.Date(Date),
                CART = ifelse(Site %in% c(170971007,550590019,550590002), "Chicago-North",
                              ifelse(Site >= 180000000 & Site < 190000000, "Chicago-Indiana", "Chicago-Central")))

#Separate by area
Chicago.North.O3 <- dplyr::filter(Chicago.O3, CART == "Chicago-North")
Chicago.Central.O3 <- dplyr::filter(Chicago.O3, CART == "Chicago-Central")
Chicago.IN.O3 <- dplyr::filter(Chicago.O3, CART == "Chicago-Indiana")

#Load ozone-conducive days (determined from airport met and HYSPLIT parameters using criteria determined from CART for 2001-2020 in "St Louis-Chicago O3-conducive days - extended to 1987-2021.R")

#Chicago North
Chicago.North.high.O3.days <- read.csv("Chicago North - ozone conducive days - 1987-2021.csv", header = TRUE)
Chicago.North.high.O3.days <- Chicago.North.high.O3.days %>%
  dplyr::mutate(date = as.Date(date))

Chicago.North.high.O3.nodes <- inner_join(Chicago.North.high.O3.days, Chicago.North.O3, by = c("date"="Date"))

#Calculate means
Chicago.North.high.O3.nodes.means <- Chicago.North.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

Chicago.North.high.O3.nodes.means.long <- pivot_longer(Chicago.North.high.O3.nodes.means, weekday.mean:weekend.count,
                                                  names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")

    # # #Plot of mean MDA8 per year/group bin
    # c <- ggplot(data = Chicago.North.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - Chicago North\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Chicago North mean MDA8 fixed yrs- 1987-21 - high O3 nodes-means-by mon.png", plot=c, width = 9, height = 6.5)
    # 
    # 
    # #Plot of number of days per year/group/node bin
    # c <- ggplot(data = Chicago.North.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   # geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - Chicago North\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago North count fixed yrs- 1987-21 - high O3 nodes-means-by mon.png"), plot=c, width = 9, height = 6.5)
    # 
    # 
    # #weekday-weekend diff (%) plot
    # a <- ggplot(data = Chicago.North.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
    #   geom_line(aes(x=yr.bins, y=wd.we.diff, color=Site.Name.new, group=Site.Name.new), size=1) + #for mean plots
    #   xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nChicago North High O3 Nodes")) +
    #   # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
    #   # scale_color_brewer(palette="Paired") +
    #   scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(23)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago North - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 - high O3 nodes-means-by mon.png"), plot=a, width = 9, height = 6.5)

    
#Chicago Central
Chicago.Central.high.O3.days <- read.csv("Chicago Central - ozone conducive days - 1987-2021.csv", header = TRUE)
Chicago.Central.high.O3.days <- Chicago.Central.high.O3.days %>%
  dplyr::mutate(date = as.Date(date))

Chicago.Central.high.O3.nodes <- inner_join(Chicago.Central.high.O3.days, Chicago.Central.O3, by = c("date"="Date"))

#Calculate means
Chicago.Central.high.O3.nodes.means <- Chicago.Central.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

Chicago.Central.high.O3.nodes.means.long <- pivot_longer(Chicago.Central.high.O3.nodes.means, weekday.mean:weekend.count,
                                                       names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")
    
    # # #Plot of mean MDA8 per year/group bin
    # c <- ggplot(data = Chicago.Central.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - Chicago Central\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Chicago Central mean MDA8 fixed yrs- 1987-21 - high O3 nodes-means-by mon.png", plot=c, width = 9, height = 6.5)
    # 
    # 
    # #Plot of number of days per year/group/node bin
    # c <- ggplot(data = Chicago.Central.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   # geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - Chicago Central\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago Central count fixed yrs- 1987-21 - high O3 nodes-means-by mon.png"), plot=c, width = 9, height = 6.5)
    # 
    # 
    # #weekday-weekend diff (%) plot
    # a <- ggplot(data = Chicago.Central.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
    #   geom_line(aes(x=yr.bins, y=wd.we.diff, color=Site.Name.new, group=Site.Name.new), size=1) + #for mean plots
    #   xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nChicago Central High O3 Nodes")) +
    #   # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
    #   # scale_color_brewer(palette="Paired") +
    #   scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(23)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago Central - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 - high O3 nodes-means-by mon.png"), plot=a, width = 9, height = 6.5)
    
#Chicago Indiana
Chicago.Indiana.high.O3.days <- read.csv("Chicago Indiana - ozone conducive days - 1987-2021.csv", header = TRUE)
Chicago.Indiana.high.O3.days <- Chicago.Indiana.high.O3.days %>%
  dplyr::mutate(date = as.Date(date))

Chicago.Indiana.high.O3.nodes <- inner_join(Chicago.Indiana.high.O3.days, Chicago.IN.O3, by = c("date"="Date"))

#Calculate means
Chicago.Indiana.high.O3.nodes.means <- Chicago.Indiana.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

Chicago.Indiana.high.O3.nodes.means.long <- pivot_longer(Chicago.Indiana.high.O3.nodes.means, weekday.mean:weekend.count,
                                                       names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")
    
    # # #Plot of mean MDA8 per year/group bin
    # c <- ggplot(data = Chicago.Indiana.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - Chicago Indiana\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Chicago Indiana mean MDA8 fixed yrs- 1987-21 - high O3 nodes-means-by mon.png", plot=c, width = 9, height = 6.5)
    # 
    # 
    # #Plot of number of days per year/group/node bin
    # c <- ggplot(data = Chicago.Indiana.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Site.Name.new) + #for mean plots
    #   # geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - Chicago Indiana\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago Indiana count fixed yrs- 1987-21 - high O3 nodes-means-by mon.png"), plot=c, width = 9, height = 6.5)
    # 
    # 
    # #weekday-weekend diff (%) plot
    # a <- ggplot(data = Chicago.Indiana.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
    #   geom_line(aes(x=yr.bins, y=wd.we.diff, color=Site.Name.new, group=Site.Name.new), size=1) + #for mean plots
    #   xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nChicago Indiana High O3 Nodes")) +
    #   # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
    #   # scale_color_brewer(palette="Paired") +
    #   scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(23)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago Indiana - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 - high O3 nodes-means-by mon.png"), plot=a, width = 9, height = 6.5)
    


## CHICAGO - RUN WEEKDAY-WEEKEND ANALYSIS ON MONITOR CLUSTERS ####################################

#Import cluster labels
cluster.labels <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Cluster monitors lat-long.csv", header=TRUE)

#Combine ozone-conducive days data for three areas
Chicago.high.O3.nodes <- bind_rows(Chicago.North.high.O3.nodes, Chicago.Central.high.O3.nodes, Chicago.Indiana.high.O3.nodes)
Chicago.high.O3.nodes <- left_join(Chicago.high.O3.nodes, cluster.labels, by="Site")

#add final cluster names
cluster.names <- data.frame(city = c(rep("Chicago",10), rep("St. Louis",9)),
                            Cluster = c("1","2a","2b","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","A"), 
                            Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE","64N","22","26N","44NW","55S*","84S","3","91N","10W"))
Chicago.high.O3.nodes <- left_join(Chicago.high.O3.nodes, cluster.names, by = c("city","Cluster"))
Chicago.high.O3.nodes <- dplyr::filter(Chicago.high.O3.nodes, !is.na(Cluster))

## DROP THREE MONITORS WITH < 10 YEARS OF DATA (EXCLUDING 1987-91) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Chicago.high.O3.nodes <- dplyr::filter(Chicago.high.O3.nodes, !(Site %in% c(170310037, 170310063, 180891016)))



#Make plot of mean MDA8 values by monitor (for SI)

Chicago.high.O3.no1987.means <- Chicago.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other" & yr.bins != "1987-91") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

Chicago.high.O3.no1987.means.long <- pivot_longer(Chicago.high.O3.no1987.means, weekday.mean:weekend.count,
                                                 names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")
#get change in mean MDA8 for each monitor - only monitors operating the whole time
Chicago.high.O3.no1987.means.summary <- Chicago.high.O3.no1987.means %>%
  dplyr::group_by(city, Site, Site.Name.new) %>%
  dplyr::mutate(start.yr.bin = min(yr.bins), end.yr.bin = max(yr.bins)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(yr.bins == "1992-96" | yr.bins == "2017-21") %>%
  dplyr::group_by(city, Site, Site.Name.new) %>%
  dplyr::summarize(wd.change = weekday.mean[yr.bins=="1992-96"] - weekday.mean[yr.bins=="2017-21"],
                   we.change = weekend.mean[yr.bins=="1992-96"] - weekend.mean[yr.bins=="2017-21"]) %>%
  dplyr::ungroup() 
mean(Chicago.high.O3.no1987.means.summary$we.change)
  


    # # #Plot of mean MDA8 per year/group bin
    # c <- ggplot(data = Chicago.high.O3.no1987.means.long) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Site) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - Chicago \nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Chicago all mean MDA8 fixed yrs- 1992-21 - high O3 nodes-means-by mon-for SI.png", plot=c, width = 10, height = 7)


# Make plots of monitors grouped by clusters - weekday means and W-W differences (for paper SI)
#Import cluster labels
cluster.labels <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/trends over space and time/Cluster monitors lat-long.csv", header=TRUE)

Chicago.high.O3.no1987.means <- left_join(Chicago.high.O3.no1987.means, cluster.labels, by="Site")

#add final cluster names
cluster.names <- data.frame(city = c(rep("Chicago",10), rep("St. Louis",9)),
                            Cluster = c("1","2a","2b","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","A"), 
                            Cluster.name = c("22","40SW","66SE*","40SE*","47NW","72N*","19*","13","84SW","74SE","64N","22","26N","44NW","55S*","84S","3","91N","10W"))
Chicago.high.O3.no1987.means <- left_join(Chicago.high.O3.no1987.means, cluster.names, by=c("city","Cluster"))
Chicago.high.O3.no1987.means <- Chicago.high.O3.no1987.means %>%
  # dplyr::filter(day.type == "weekday") %>% #select just weekday means for plotting
  dplyr::mutate(Site = as.character(Site))
  # Cluster.name = factor(Cluster.name, levels = c("3","10W","22","26N","44NW","55S*","64N","84S","91N")))

    # #Plot of weekday mean MDA8
    # a <- ggplot(data=Chicago.high.O3.no1987.means, aes(x=yr.bins, y=weekday.mean, color=Site, group=Site)) + geom_line() + facet_wrap(.~Cluster.name) +
    #   ylab("Mean Weekday MDA8 Ozone (ppb)") + xlab(NULL) + ggtitle("Chicago Monitor Mean Weekday MDA8 by Cluster") +
    #         theme(axis.text = element_text(size = 14, color = "black"),
    #               axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #               axis.title.y = element_text(size = 16),
    #               legend.text = element_text(size = 12),
    #               legend.title = element_blank(),
    #               plot.title = element_text(size = 18, hjust = 0))
    #       ggsave(filename = paste0("Chicago - Weekday mean MDA8 by mon grouped by cluster.png"), plot=a, width = 9,
    #              height = 6.5)
    # 
    # #Plot of mean W-W differences
    # a <- ggplot(data=Chicago.high.O3.no1987.means, aes(x=yr.bins, y=wd.we.diff, color=Site, group=Site)) + geom_line() + facet_wrap(.~Cluster.name) +
    #   ylab("Weekday-Weekend MDA8 Difference (ppb)") + xlab(NULL) + ggtitle("Chicago Weekday-Weekend Diff. by Cluster") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago - Weekday-Weekend MDA8 diff by mon grouped by cluster.png"), plot=a, width = 9,
    #        height = 6.5)



#Calculate means and differences by cluster and plot
#Updating code from "weekday-weekend analysis - fall 2023 - CART by monitor CLUSTERS-all 100km mons.R"

Chicago.high.O3.nodes.means <- Chicago.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Cluster, Cluster.name, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

Chicago.high.O3.nodes.means.long <- pivot_longer(Chicago.high.O3.nodes.means, weekday.mean:weekend.count,
                                                  names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)")

    # #Plot of mean MDA8 per year/group bin
    # c <- ggplot(data = Chicago.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=mean, color=day.type, group=day.type), size=1) + facet_wrap(~Cluster.name) + #for mean plots
    #   geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Mean MDA8 (ppb)") + ggtitle(paste0("Mean MDA8 - Chicago clusters\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Chicago clusters mean MDA8 fixed yrs- 1987-21 - 100km.png", plot=c, width = 9, height = 6.5)
    # 
    # 
    # #Plot of number of days per year/group/node bin
    # c <- ggplot(data = Chicago.high.O3.nodes.means.long) +
    #   geom_line(aes(x=yr.bins, y=count, color=day.type, group=day.type), size=1) + facet_wrap(~Cluster.name) + #for mean plots
    #   # geom_hline(yintercept = 70, color="darkgray") +
    #   # scale_y_continuous(limits = c(0,22.6), breaks= seq(0,22,by=2)) +
    #   scale_linetype_manual(values = c("dashed","solid")) +
    #   xlab(NULL) + ylab("Number of days") + ggtitle(paste0("Days per node - Chicago clusters\nHigh O3 Nodes")) +
    #   scale_color_brewer(palette="Paired") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago clusters count fixed yrs- 1987-21 - 100km.png"), plot=c, width = 9, height = 6.5)
    # 
    # 
    # # #weekday-weekend diff (%) plot
    # a <- ggplot(data = Chicago.high.O3.nodes.means) + geom_hline(yintercept = 0, color="darkgray") +
    #   geom_line(aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=1) + #for mean plots
    #   xlab(NULL) + ylab("Weekday-Weekend Mean MDA8 Diff. (ppb)") + ggtitle(paste0("Weekday-Weekend MDA8 Differences \nChicago clusters")) +
    #   # scale_y_continuous(limits = c(-11.2,10.2), breaks = seq(-10,8,by=2), minor_breaks = seq(-11,10,by=0.5)) +
    #   scale_color_brewer(palette="Paired") +
    #   # scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(14)) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Chicago clusters - Weekday-weekend mean MDA8 diffs fixed yrs - 1987-21 -100km.png"), plot=a, width = 9, height = 6.5)


#Run Welch's t-test and save statistics in combined data frame. 
Chicago.high.O3.nodes <- Chicago.high.O3.nodes %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), #USE FULL SET OF YEARS,
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>%
  dplyr::filter(day != "other") 

Chicago.means.wd <- filter(Chicago.high.O3.nodes, day == "weekday")
Chicago.means.we <- filter(Chicago.high.O3.nodes, day == "weekend")

cluster.list <- unique(Chicago.high.O3.nodes.means.long$Cluster.name)


t.test.Chicago <- data.frame() #creates blank data frame that will be filled below

for (i in cluster.list)
{
  NAA.mon.subset.wd <- subset(Chicago.means.wd, Chicago.means.wd$Cluster.name == i)
  NAA.mon.subset.we <- subset(Chicago.means.we, Chicago.means.we$Cluster.name == i)
  #Determine which year groups had > 2 days to allow the chi-squared test and use this to determine which years to loop over
  yrs.enough.days <- NAA.mon.subset.we %>%
    dplyr::group_by(yr.bins) %>%
    dplyr::summarise(n.days = length(date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n.days > 1)
  yr.list <- unique(yrs.enough.days$yr.bins)
  
  
  # for (j in node.list)
  # {
  #   NAA.group.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$node == j)
  #   NAA.group.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$node == j)
  
  for (k in yr.list)
  {
    NAA.group.yr.subset.wd <- subset(NAA.mon.subset.wd, NAA.mon.subset.wd$yr.bins == k)
    NAA.group.yr.subset.we <- subset(NAA.mon.subset.we, NAA.mon.subset.we$yr.bins == k)
    
    t.test.results <- t.test(NAA.group.yr.subset.wd$MDA8, NAA.group.yr.subset.we$MDA8) #runs test
    t.test.result.temp <- tidy(t.test.results) #converts results into tidy form (data frame)
    t.test.result.temp$Cluster.name <- i
    # t.test.result.temp$node <- j
    t.test.result.temp$yr.bins <- k
    
    # options(warn = 2) #changes warnings to errors for below
    # chi.result.temp$warn <- berryFunctions::is.error(chisq.test(NAA.group.yr.subset$day, NAA.group.yr.subset$exceed, correct = FALSE))
    # options(warn = 0) #changes warnings back into warnings (not errors)
    
    t.test.Chicago <- rbind(t.test.Chicago, t.test.result.temp)
    
  }
}
# }


#Add t-test results to means and export:
Chicago.high.O3.nodes.t.test <- full_join(Chicago.high.O3.nodes.means, t.test.Chicago, by=c("yr.bins","Cluster.name"))
# write.csv(Chicago.high.O3.nodes.t.test, "Chicago cluster W-W diffs-means-by cluster - 1987-2021.csv", row.names = FALSE)


# CHICAGO: MAKE PLOTS FOR THE PAPER ########################################################################
#Modified from "weekday-weekend analysis LADCO CLUSTERS - with CART-combining - Oct 2023.R"

Chicago.high.O3.nodes <- read.csv("Chicago cluster W-W diffs-means-by cluster - 1987-2021.csv", header = TRUE)

## REMOVE 1987-91 !!!!!!!!!!!!!!!!!!!!!!!!!!!!
Chicago.high.O3.nodes <- dplyr::filter(Chicago.high.O3.nodes, yr.bins != "1987-91")

#Remove year groups with < 9 days in them (to be consistent with cut for Chicago)

Chicago.high.O3.gt10d <- Chicago.high.O3.nodes %>%
  dplyr::filter(weekend.count >= 9)

Chicago.high.O3.gt10d.long <- Chicago.high.O3.gt10d %>%
  dplyr::select(1:8) %>%
  pivot_longer(weekday.mean:weekend.count, names_to = c("day",".value"), names_pattern = "([a-z]*)\\.([a-z]*)")

Chicago.signif <- dplyr::filter(Chicago.high.O3.gt10d, p.value <= 0.05)

#Chicago plots

Chicago.high.O3.gt10d$Cluster.name <- factor(Chicago.high.O3.gt10d$Cluster.name, 
                                             levels = c("13","19*","22","40SE*","40SW","47NW","66SE*","72N*","74SE","84SW"))
Chicago.high.O3.gt10d.long$Cluster.name <- factor(Chicago.high.O3.gt10d.long$Cluster.name, 
                                                  levels = c("13","19*","22","40SE*","40SW","47NW","66SE*","72N*","74SE","84SW"))
Chicago.signif$Cluster.name <- factor(Chicago.signif$Cluster.name, levels = c("13","19*","22","40SE*","40SW","47NW","66SE*","72N*","74SE","84SW"))

Chicago.pal <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#C3DB42", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2") #make two middle colors darker (same palette as for trends plots)

    # #MDA8 trend
    c <- ggplot(data = Chicago.high.O3.gt10d.long) + #coord_fixed(1/9) +
      geom_line(aes(x=yr.bins, y=mean, color=Cluster.name, linetype=day, group=interaction(Cluster.name,day)), size=1) +
      scale_y_continuous(limits = c(48,95), breaks= seq(50,95,by=5)) +
      scale_linetype_manual(values = c("dashed","solid")) +
      xlab(NULL) + ylab("Mean MDA8 (ppb)") +
      annotate("text", x= 6, y=90, label="Chicago", size = 6, hjust=1, vjust=0) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = Chicago.pal) +
      guides(color = FALSE, linetype = guide_legend()) +
      #guides(color=guide_legend(ncol=2)) +
      #guides(linetype=guide_legend(ncol=2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.position = c(0.76,0.76),
            legend.key = element_blank(),
            legend.background = element_rect(color = "black"),
            legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
    #weekday-weekend diff (%) plot - WITH T-TEST RESULTS
    d <- ggplot() + geom_line(data = Chicago.high.O3.gt10d, aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=1) +
      geom_point(data = Chicago.signif, aes(x=yr.bins, y=wd.we.diff, color=Cluster.name, group=Cluster.name), size=3) +
      geom_hline(yintercept = 0, color="darkgray") + #coord_fixed(1/6.5) +
      annotate("text", x= 6, y=8, label="Chicago", size = 6, hjust=1, vjust=0) +
      xlab(NULL) + ylab("W-W Mean MDA8 Diff. (ppb)") +
      # scale_y_continuous(limits = c(-10, 10), breaks = seq(-10,10,by=2)) +
      scale_y_continuous(limits = c(-12.7, 9), breaks = seq(-12,8,by=2)) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = Chicago.pal) +
      guides(color = guide_legend(override.aes = list(shape = NA))) +
      # guides(shape=FALSE, size=FALSE) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
    # Combine and save plots
    
    ORD.WW.plot <- plot_grid(c,d, nrow=1, rel_widths = c(1,1.25), labels = c("(a)","(b)"), label_x = -0.025)
    # ggsave(filename = "Chicago W-W MDA8-diff plot 1987-2021.png", plot=ORD.WW.plot, width = 8, height = 4)
    ggsave(filename = "Chicago W-W MDA8-diff plot 1992-2021.png", plot=ORD.WW.plot, width = 8, height = 4)

    
#MAKE COMBINED ST. LOUIS-CHICAGO PLOTS
#Run St. Louis plots above first (lines 467-540), then combine
    
    STL.ORD.WW.plot <- plot_grid(a,b,c,d, nrow=2, rel_widths = c(1,1.25), labels = c("(a)","(b)","(c)","(d)"), label_x = -0.025)
    # ggsave(filename = "St Louis-Chicago W-W MDA8-diff plot 1987-2021.png", plot=STL.ORD.WW.plot, width = 8, height = 8)
    ggsave(filename = "St Louis-Chicago W-W MDA8-diff plot 1992-2021.png", plot=STL.ORD.WW.plot, width = 8, height = 8)

    
#MAKE COMBINED PLOT - JUST W-W DIFFERENCES (NO MDA8 VALUES)
    
    STL.ORD.WW.only.plot <- plot_grid(b,d, nrow=1, rel_widths = c(1,1), labels = c("(a)","(b)"), label_x = -0.025)
    # ggsave(filename = "St Louis-Chicago W-W MDA8-diff only plot 1987-2021.png", plot=STL.ORD.WW.only.plot, width = 8, height = 4)
    ggsave(filename = "St Louis-Chicago W-W MDA8-diff only plot 1992-2021.png", plot=STL.ORD.WW.only.plot, width = 8, height = 4)

# MAKE MDA8 VALUE PLOT - JUST FOR CHICAGO, FACETED BY CLUSTER   
    
    c <- ggplot(data = Chicago.high.O3.gt10d.long) + facet_wrap(~Cluster.name, ncol=3) + #coord_fixed(1/9) +
      geom_line(aes(x=yr.bins, y=mean, color=Cluster.name, linetype=day, group=interaction(Cluster.name,day)), size=1) +
      scale_y_continuous(limits = c(48,85), breaks= seq(50,85,by=5)) +
      scale_linetype_manual(values = c("dashed","solid")) +
      xlab(NULL) + ylab("Mean MDA8 (ppb)") +
      # annotate("text", x= 7, y=90, label="Chicago", size = 6, hjust=1, vjust=0) +
      # scale_color_brewer(palette="Spectral") +
      scale_color_manual(values = Chicago.pal) +
      guides(color = FALSE, linetype = guide_legend()) +
      # guides(color=guide_legend(ncol=3)) +
      #guides(linetype=guide_legend(ncol=2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            strip.text = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.position = c(0.89,0.43),
            legend.key = element_blank(),
            legend.background = element_rect(color = "black"),
            legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    # ggsave(filename = "Chicago W-W conc plot 1987-2021.png", plot=c, width = 6, height = 8)
    ggsave(filename = "Chicago W-W conc plot 1992-2021.png", plot=c, width = 6, height = 8)

    
    
## MAKE MAPS OF WEEKDAY-WEEKEND DIFFERENCES
    
#St. Louis data
St.Louis.high.O3.nodes.map <- inner_join(STL.high.O3.days, St.Louis.O3, by = c("date"="Date"))

St.Louis.high.O3.nodes.means.map <- St.Louis.high.O3.nodes.map %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), 
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

St.Louis.high.O3.nodes.means.map <- left_join(St.Louis.high.O3.nodes.means.map, lat.long.city, by="Site") #add on long/lat
St.Louis.high.O3.nodes.means.map <- St.Louis.high.O3.nodes.means.map %>%
  dplyr::select(1:4, 11, 12, 9) %>%
  dplyr::filter(yr.bins != "1987-91" & !is.nan(wd.we.diff))

#Chicago data
#Chicago North
Chicago.North.high.O3.nodes.map <- inner_join(Chicago.North.high.O3.days, Chicago.North.O3, by = c("date"="Date"))
Chicago.North.high.O3.nodes.means.map <- Chicago.North.high.O3.nodes.map %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), 
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

#Chicago Central
Chicago.Central.high.O3.nodes.map <- inner_join(Chicago.Central.high.O3.days, Chicago.Central.O3, by = c("date"="Date"))
Chicago.Central.high.O3.nodes.means.map <- Chicago.Central.high.O3.nodes.map %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), 
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup()

#Chicago Indiana
Chicago.Indiana.high.O3.nodes.map <- inner_join(Chicago.Indiana.high.O3.days, Chicago.IN.O3, by = c("date"="Date"))
Chicago.Indiana.high.O3.nodes.means.map <- Chicago.Indiana.high.O3.nodes.map %>%
  dplyr::mutate(yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21")))))), 
                day = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday %in% c("Sun"), "weekend", "other"))) %>% #without Saturday
  dplyr::filter(day != "other") %>%
  dplyr::group_by(city, Site, Site.Name.new, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.mean = mean(MDA8[day=="weekday"], na.rm=TRUE), weekend.mean = mean(MDA8[day=="weekend"], na.rm=TRUE),
                   weekday.count = length(MDA8[day=="weekday"]), weekend.count = length(MDA8[day=="weekend"]), wd.we.diff = weekday.mean - weekend.mean) %>%
  ungroup() 

#Combined
Chicago.high.O3.nodes.means.map <- bind_rows(Chicago.North.high.O3.nodes.means.map, Chicago.Central.high.O3.nodes.means.map, 
                                             Chicago.Indiana.high.O3.nodes.means.map)


Chicago.high.O3.nodes.means.map <- left_join(Chicago.high.O3.nodes.means.map, lat.long.city, by="Site") #add on long/lat
Chicago.high.O3.nodes.means.map <- Chicago.high.O3.nodes.means.map %>%
  dplyr::select(1:4, 11, 12, 9) %>%
  dplyr::filter(yr.bins != "1987-91" & !(Site %in% c(170310037, 170310063, 180891016, 551010020))) #drop sites with short records & Racine

#Save file with weekday, weekend, W-W diffs (comment out select() line above before saving)
# write.csv(Chicago.high.O3.nodes.means.map, "Chicago W-W means and diffs from 1992.csv", row.names = FALSE)

#Subset for each set of years to plot (1992-1996 and 2017-21)
WW.Chi.92.96 <- dplyr::filter(Chicago.high.O3.nodes.means.map, yr.bins == "1992-96")
WW.Chi.17.21 <- dplyr::filter(Chicago.high.O3.nodes.means.map, yr.bins == "2017-21")

WW.Stl.92.96 <- dplyr::filter(St.Louis.high.O3.nodes.means.map, yr.bins == "1992-96")
WW.Stl.17.21 <- dplyr::filter(St.Louis.high.O3.nodes.means.map, yr.bins == "2017-21")

    
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

    
#for 1992-96 to 2017-21 maps (different color schemes for different starting years)
    
#Chicago maps
    
    Chi.1992.96.map <- region.map.counties + 
      geom_point(data=WW.Chi.92.96, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
      # annotate("text", x=-87, y=42.6, label = "1987-91", size=6) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    
    Chi.2017.21.map <- region.map.counties + 
      geom_point(data=WW.Chi.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
      # annotate("text", x=-87, y=42.6, label = "2017-21", size=6) +
      theme(legend.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"))
    
#St. Louis maps
    
    Stl.1992.96.map <- region.map.counties + 
      geom_point(data=WW.Stl.92.96, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
      annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"))
    
    Stl.2017.21.map <- region.map.counties + 
      geom_point(data=WW.Stl.17.21, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
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
    
    ggsave(filename = "Weekday-weekend maps - St Louis-Chicago 1992-2021.png", plot = paper.plot, width=6.5, height = 6)    
    
    
#Make plots of all year groups for St. Louis and Chicago separately
    
#St. Louis - from 1992-96
    Stl.WW <- dplyr::filter(St.Louis.high.O3.nodes.means.map, yr.bins != "1987-91")
    
    a <- region.map.counties + facet_wrap(~yr.bins) +
      geom_point(data=Stl.WW, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-91.25,-89.25), ylim = c(37.6,39.4)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
      # annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            strip.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    ggsave(filename = "St Louis WW maps - 1992-2021.png", plot = a, width = 8.5, height = 6.25)
    
#Chicago - from 1992-96
    Chi.WW <- dplyr::filter(Chicago.high.O3.nodes.means.map, yr.bins != "1987-91")
    
    a <- region.map.counties + facet_wrap(~yr.bins) +
      geom_point(data=Chi.WW, mapping=aes(x=Longitude, y=Latitude, group=1, fill=wd.we.diff), shape=21, size=4) +
      coord_map(xlim = c(-88.46,-86.42), ylim = c(41.13,42.75)) + 
      scale_fill_gradientn(colors = matlab.like(32), name=NULL, limits = c(-19,12), breaks = seq(-16,12,by=4)) +
      # annotate("text", x=-90.3, y=39.4, label = "1992-96", size=6, hjust=0.5) +
      theme(legend.text = element_text(size = 14),
            legend.key.size = unit(1, "cm"),
            strip.text = element_text(size = 14),
            plot.background = element_rect(fill = "white", color="white"),
            panel.background = element_rect(fill = "white", color="white"),
            plot.margin = unit(c(1,1,1,1), "lines"))
    ggsave(filename = "Chicago WW maps - 1992-2021.png", plot = a, width = 8.5, height = 6.25)
    
    
    
    
    
    
      
## IMPORT NO2 CONCENTRATIONS ######################################################################
#Import hourly data - will select just data from 9:00-14:59 LST from May to September on ozone-conducive days

## PULL AQS DATA FROM THE API ########################################################

# require(RAQSAPI)
# 
# #Log in to AQS Data Mart API
# 
# # #First time set key (password)
# # keyring::key_set(service = "AQSDatamart", username = "...")
# 
# datamartAPI_user <- "..."
# server <- "AQSDatamart"
# aqs_credentials(username = datamartAPI_user, key = key_get(service = server, username = datamartAPI_user ) )
# 
# #Download daily PM2.4 24-hour average data - loop through states and years. (Can't download > year at a time, and calls are limited in size as well)
# CBSA.list <- c("St. Louis, MO-IL","Chicago-Naperville-Elgin, IL-IN-WI")
# CBSA.code.list <- c("41180","16980")
# year.list <- as.character(seq(1987,2021,by=1))
# 
# NO2.data.down <- data.frame()
# 
# for(i in CBSA.code.list)
# {
#   for(j in year.list)
#   {
#     temp <- RAQSAPI::aqs_sampledata_by_cbsa(parameter = "42602", cbsa_code = i,
#                                                bdate = as.Date(paste0(j,"0501"), format="%Y%m%d"), edate = as.Date(paste0(j,"0930"), format="%Y%m%d"))
#     NO2.data.down <- bind_rows(NO2.data.down,temp)
#   }
# }
#     
# write.csv(NO2.data.down, "NO2 data - 1987-2021 - St Louis-Chicago.csv", row.names = FALSE)

#Open file with NO2 data
NO2.data.raw <- read.csv("NO2 data - 1987-2021 - St Louis-Chicago.csv", header = TRUE)
    
NO2.data <- NO2.data.raw %>%
  dplyr::mutate(county_code = str_pad(county_code, 3, pad = "0"), site_number = str_pad(site_number, 4, pad = "0"),
                Site = paste0(state_code, county_code, site_number), date_local = as.Date(date_local, "%Y-%m-%d"), year = format(date_local, "%Y"),
                month = format(date_local, "%m"), CBSA = ifelse(cbsa_code == 41180, "St. Louis", "Chicago")) %>%
  dplyr::rename(State=state_code, County=county_code, Monitor=site_number, Date = date_local, NO2.1hr = sample_measurement) %>%
  dplyr::select(33,30,5,10,11,31,32,24,14)

#Calculate midday average NO2 by monitor for midday (9-14), May-September days & select just weekday (Tues-Thurs) or weekend (Sun) days
#Drop POC - only Northbrook had >1 POC - average these two on a daily basis
NO2.select <- NO2.data %>%
  dplyr::filter(time_local %in% c("9:00","10:00","11:00","12:00","13:00","14:00") & #select just midday data (9-14:59 data)
                month %in% c("05","06","07","08","09")) %>%
  dplyr::group_by(CBSA,Site,Date,year) %>%
  dplyr::summarise(NO2.midday = mean(NO2.1hr)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(weekday = format(Date, "%a"), day.type = ifelse(weekday %in% c("Tue","Wed","Thu"), "weekday", ifelse(weekday == "Sun", "weekend","other"))) %>%
  dplyr::filter(day.type != "other")

# #Look at which years the different monitors operated
# NO2.mons <- NO2.select %>%
#   dplyr::select(1,2,3,5) %>%
#   dplyr::distinct() %>%
#   dplyr::mutate(year = paste0("yr.",year), presence = "X") %>%
#   pivot_wider(names_from = year, values_from = presence)
# 
# write.csv(NO2.mons, "clipboard", row.names = FALSE)

#Combine with list of monitors to used (selected from above - records at least 8 years, in central part of city, not near road)
#Average 2 POCs at Northbrook
mons.use <- c("170310037","170310039","170310063","170310064","170310072","170310076","170313101","170313103","170314002","170314201","170314201","170318003",
              "171630010","291890001","291890004","291890006","291893001","291895001","291897002","295100072","295100080","295100085","295100086")

NO2.annual.mons.weekday <- NO2.select %>%
  dplyr::filter(day.type == "weekday" & Site %in% mons.use) %>%
  dplyr::group_by(CBSA,Site,year) %>%
  dplyr::summarise(annual.mean = mean(NO2.midday, na.rm = TRUE)) %>%
  dplyr::ungroup()

#make plots
CBSA.list <- c("Chicago","St. Louis")
    
    # for(i in CBSA.list)
    # {
    #         CBSA.subset <- dplyr::filter(NO2.annual.mons.weekday, CBSA == i)
    # 
    #   a <- ggplot(CBSA.subset, aes(x=year, y=annual.mean, color=Site, group=Site)) + geom_line() +
    #     xlab(NULL) + ylab("Annual mean NO2 (ppb)") + ggtitle(paste0("Annual Mean Summer Midday NO2: ", i)) +
    #     theme(axis.text = element_text(size = 10, color = "black"),
    #           axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
    #           axis.title.y = element_text(size = 12),
    #           legend.text = element_text(size = 10),
    #           legend.title = element_blank(),
    #           legend.key = element_blank(),
    #           panel.background = element_rect(fill = "white"),
    #           axis.line = element_line(color = "black"),
    #           panel.grid.major = element_line(color = "gray90"))
    #   ggsave(filename = paste0("Annual mean summer midday NO2 - ", i, ".png"), plot = a, width = 6, height = 5)
    # }
      

#Calculate averages for year groups for weekdays and weekend
#First calculate the annual mean by monitor, then calculate the year group mean by NAA
NO2.WW.yr.group.means <- NO2.select %>%
  dplyr::mutate(year = as.numeric(year),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::group_by(CBSA,Site,year,yr.bins,day.type) %>%
  dplyr::summarise(annual.mean = mean(NO2.midday, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(CBSA,yr.bins,day.type) %>%
  dplyr::summarise(area.mean = mean(annual.mean, na.rm = TRUE)) %>%
  dplyr::ungroup()

# write.csv(NO2.WW.yr.group.means, "Weekday-weekend NO2 concentrations - Chicago-St Louis.csv", row.names = FALSE)

#Make plot of weekday-weekend NO2 concentrations

    # a <- ggplot(NO2.WW.yr.group.means, aes(x=yr.bins, y=area.mean, color=CBSA, linetype=day.type, group=interaction(CBSA,day.type))) + geom_line(linewidth=1) +
    #   scale_color_brewer(palette = "Set1") + scale_linetype_manual(values = c(2,1)) +
    #   xlab(NULL) + labs(y = expression("Mean Area NO"[2]*" Concentrations (ppb)")) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Weekday-weekend NO2 trends - Chicago-St Louis.png", plot=a, width = 6, height = 5)


#Plot NO2 for ALL days (for Trends analysis)
#Calculate midday average NO2 by monitor for midday (9-14), May-September days
#Drop POC - only Northbrook had >1 POC - average these two on a daily basis
NO2.select.alldays <- NO2.data %>%
  dplyr::filter(time_local %in% c("9:00","10:00","11:00","12:00","13:00","14:00") & #select just midday data (9-14:59 data)
                  month %in% c("05","06","07","08","09")) %>%
  dplyr::group_by(CBSA,Site,Date,year) %>%
  dplyr::summarise(NO2.midday = mean(NO2.1hr)) %>%
  dplyr::ungroup() 


#Combine with list of monitors to used (selected from above - records at least 8 years, in central part of city, not near road)
#Average 2 POCs at Northbrook
mons.use <- c("170310037","170310039","170310063","170310064","170310072","170310076","170313101","170313103","170314002","170314201","170318003",
              "171630010","291890001","291890004","291890006","291893001","291895001","291897002","295100072","295100080","295100085","295100086")

# NO2.annual.mons.alldays <- NO2.select.alldays %>%
#   dplyr::filter(Site %in% mons.use) %>%
#   dplyr::group_by(CBSA,Site,year) %>%
#   dplyr::summarise(annual.mean = mean(NO2.midday, na.rm = TRUE)) %>%
#   dplyr::ungroup()

#Calculate averages for year groups
#First calculate the annual mean by monitor, then calculate the year group mean by NAA
NO2.yr.group.means.alldays <- NO2.select.alldays %>%
  dplyr::mutate(year = as.numeric(year),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::group_by(CBSA,Site,year,yr.bins) %>%
  dplyr::summarise(annual.mean = mean(NO2.midday, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(CBSA,yr.bins) %>%
  dplyr::summarise(area.mean = mean(annual.mean, na.rm = TRUE)) %>%
  dplyr::ungroup()

# write.csv(NO2.yr.group.means.alldays, "NO2 concentrations by year groups - Chicago-St Louis.csv", row.names = FALSE)

#Make plot of NO2 concentrations (average of all day types)

    # a <- ggplot(NO2.yr.group.means.alldays, aes(x=yr.bins, y=area.mean, color=CBSA, group=CBSA)) + geom_line(linewidth=1) +
    #   scale_color_brewer(palette = "Set1") +
    #   xlab(NULL) + labs(y = expression("Mean Area NO"[2]*" Concentrations (ppb)")) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "Average Midday Ozone Season NO2 trends - Chicago-St Louis.png", plot=a, width = 6, height = 5)


## COMBINE WEEKDAY-WEEKEND AND ALL DAYS NO2 INTO ONE PLOT
NO2.yr.group.means.alldays <- NO2.yr.group.means.alldays %>%
  dplyr::mutate(day.type = "all days")

NO2.WW.alldays <- bind_rows(NO2.yr.group.means.alldays, NO2.WW.yr.group.means)

## DROP 1987-91 DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
NO2.WW.alldays <- dplyr::filter(NO2.WW.alldays, yr.bins != "1987-91")

NO2.WW.alldays$yr.bins <- factor(NO2.WW.alldays$yr.bins, c("2017-21","2012-16","2007-11","2002-06","1997-01","1992-96"))

    a <- ggplot(NO2.WW.alldays, aes(x=yr.bins, y=area.mean, color=CBSA, linetype=day.type, group=interaction(CBSA,day.type))) + 
      geom_line(linewidth=1) +
      scale_color_brewer(palette = "Set1") + scale_linetype_manual(values = c(3,2,1)) + ylim(0,22) +
      xlab(NULL) + labs(y = expression("Mean Area NO"[2]*" Concentrations (ppb)")) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.background = element_rect(fill = "white"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray90"))
    ggsave(filename = "W-W and all day NO2 trends - Chicago-St Louis.png", plot=a, width = 4.5, height = 4.5)


## PLOT TROPOMI NO2 BY CLUSTER VERSUS DISTANCE FROM NO2 PEAK (FOR TRENDS PLOTS)

#Import distance & NO2 by cluster
Chicago.NO2.cluster <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/manuscript - W-W and trends/Cluster NO2 and distance - Chicago.csv", header=TRUE)
St.Louis.NO2.cluster <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/O3-NOx-VOC sens/manuscript - W-W and trends/Cluster NO2 and distance - St Louis.csv", header=TRUE)

both.NO2.cluster <- bind_rows(Chicago.NO2.cluster, St.Louis.NO2.cluster)
both.NO2.cluster <- dplyr::mutate(both.NO2.cluster, Cluster.name = ifelse(Area=="St. Louis" & Cluster.name=="22", "22.", Cluster.name)) #distinguish cluster 22 for Chicago and St. Louis
both.NO2.cluster$Cluster.name <- factor(both.NO2.cluster$Cluster.name, 
                                        c("13","19*","22","40SE*","40SW","47NW","66SE*","72N*","74SE","84SW","3","10W","22.","26N","44NW","55S*","64N","84S","91N"))

point.shapes <- c(seq(0,9,by=1),seq(0,8,by=1)) #use for manual shapes
# point.shapes <- c(7,6,0,3,1,4,2,5,9,8,6,8,1,2,3,4,0,5,7) #use for manual shapes
point.colors <- c(rep("#E41A1C",10),rep("#377EB8",9))

    b <- ggplot(data = both.NO2.cluster, aes(x=mean.km.peak, y=mean.TROPOMI.NO2, pch = Cluster.name, color = Cluster.name, group=Cluster.name)) +
    # b <- ggplot(data = both.NO2.cluster, aes(x=mean.km.peak, y=mean.TROPOMI.NO2, pch = Cluster.name)) +  #black scale to copy
      geom_point(size=3, stroke = 1) +
      geom_errorbar(aes(ymin=mean.TROPOMI.NO2-stdev.TROPOMI.NO2, ymax=mean.TROPOMI.NO2+stdev.TROPOMI.NO2), show.legend=FALSE) +
      geom_errorbar(aes(xmin=mean.km.peak-stdev.km.peak, xmax=mean.km.peak+stdev.km.peak), show.legend=FALSE) +
      scale_shape_manual(values = point.shapes) + scale_color_manual(values = point.colors) + scale_x_reverse(limits = c(100,0), breaks = seq(0,100,by=20)) + 
      scale_y_continuous(limits = c(0,7.5), minor_breaks = seq(0,7,by=1)) + #scale_x_continuous(breaks = seq(0,100,by=20)) +
      labs(x = expression("Mean distance from NO"[2]*" peak (km)"), y = expression("Mean TROPOMI NO"[2]*" (10"^15*" molec/cm"^2*")")) + 
      # xlab("Mean distance from NO2 peak (km)") + ylab("Mean TROPOMI NO2 (10^15 molec/cm2)") + ggtitle("TROPOMI NO2 vs cluster location") +
      guides(pch = guide_legend(ncol = 2)) +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, vjust=0.5),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.background = element_rect(fill = "white"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray90"))
    ggsave(filename = "TROPOMI NO2 vs distance from peak - Chicago-St Louis.png", plot=b, width = 5, height = 4.5)
    # ggsave(filename = "TROPOMI NO2 vs distance from peak - Chicago-St Louis-black scale.png", plot=b, width = 5, height = 4.5)

    
#Combine year group and distance plots
    NO2.plot <- plot_grid(a,b, nrow=1, align = "h", axis = "b", rel_widths = c(1,1.1), labels = c("(a)","(b)"), label_x = -0.025)
    ggsave(filename = "Combined NO2 vs time and space plot 1992-2021.png", plot=NO2.plot, width = 8, height = 4)
    


    



