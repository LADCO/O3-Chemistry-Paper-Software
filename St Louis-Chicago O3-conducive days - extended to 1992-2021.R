############################################################################################
#                                                                                          #
#             DETERMINING OZONE-CONDUCIVE DAYS FOR 1992-2021 - ST. LOUIS & CHICAGO         #
#                                 FOR WEEKDAY-WEEKEND ANALYSIS                             #
#           USING AIRPORT MET AND MET CONDITIONS DETERMINED FROM CART FOR 2001-2020        #
#                                                                                          #
############################################################################################

#Want to extend weekday-weekend analysis to cover the full extent of the trends analysis (1992-2021). To do this, need to identify the ozone-conducive days from the whole time period. Only have MetDat from EPA for 2001-2022 

#Download airport met data from Iowa State University's Iowa Environmental Mesonet site: https://mesonet.agron.iastate.edu/request/download.phtml?network=MO_ASOS


require("tidyverse")

# ST. LOUIS MET DATA

St.Louis.met <- read.csv("STL met 1987-2021.csv", header = TRUE)

#select just May-September data & calculate average parameters. Convert ws/wd to u and v component winds to average

St.Louis.hour.means <- St.Louis.met %>%
  dplyr::mutate(Time.CST.CDT = as.POSIXct(Time.CST.CDT, "%m/%d/%Y %H:%M", tz = "America/Chicago"), Date.time.CST = Time.CST.CDT - 3600,
                month = format(Date.time.CST, "%m")) %>% #May-September is all CDT so this conversion works for those months to CST
  dplyr::filter(month %in% c("05","06","07","08","09")) %>%
  pivot_longer(T.deg.F:sea.level.P.mb, names_to = "Param", values_to = "values") %>%
  dplyr::mutate(values = as.numeric(ifelse(values == "M", NA, values))) %>% #replace missing values ("M") with NAs
  pivot_wider(names_from = "Param", values_from = "values") %>%
  dplyr::mutate(hour = as.numeric(format(Date.time.CST, "%H")), date = as.Date(Date.time.CST, tz="America/Chicago"),
                u.wind.mph = -abs(ws.mph)*sin(wd*pi/180), v.wind.mph = -abs(ws.mph)*cos(wd*pi/180)) %>% #convert to u and v vector winds
  dplyr::select(1,3,12,11,6:10,13,14) %>%
  pivot_longer(T.deg.F:v.wind.mph, names_to = "Param", values_to = "values") %>%
  dplyr::group_by(station, elevation.m, date, hour, Param) %>%
  dplyr::summarize(hour.mean = mean(values, na.rm = TRUE)) %>% #calculate hourly means (since some hours have >1 value)
  dplyr::ungroup() %>%
  pivot_wider(names_from = "Param", values_from = "hour.mean") %>%
  dplyr::mutate(ws.m.s = ws.mph * 0.44704)


#Calculate equivalent parameters to MetDat parameters from EPA (for most important parameters in the CART analysis for ozone-conducive days)
St.Louis.MetDat <- St.Louis.hour.means %>%
  # dplyr::arrange(date,hour) %>%
  # dplyr::mutate(ws.m.s.lag = lag())
  dplyr::group_by(station, elevation.m, date) %>%
  dplyr::summarise(T.avgpm = mean(T.deg.F[hour %in% c(13,14,15,16)], na.rm = TRUE), RH.avgmid = mean(RH[hour %in% c(10,11,12,13,14,15,16)], na.rm = TRUE),
                   ws.m.s.day = mean(ws.m.s, na.rm = TRUE)) %>%
  dplyr::ungroup()
  
# #add blank data for missing days (to allow the lag function below to work) - DON'T HAVE TO DO THIS BECAUSE ALREADY IS COMPLETE DATE-WISE
# all.days <- data.frame(date = seq(as.Date("1987-01-01"), as.Date("2021-12-31"), by=1))
# all.days <- all.days %>%
#   dplyr::mutate(month = format(date, "%m")) %>%
#   dplyr::filter(month %in% c("05","06","07","08","09")) %>%
#   dplyr::select(-month)
# St.Louis.MetDat <- left_join(all.days, St.Louis.MetDat, by="date")

#Calculate 2-day wind speeds
St.Louis.MetDat <- St.Louis.MetDat %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(ws.m.s.lag = lag(ws.m.s.day, n=1)) %>%
  rowwise() %>%
  dplyr::mutate(ws.2day = mean(c(ws.m.s.day,ws.m.s.lag), na.rm = TRUE)) %>%
  ungroup()

# write.csv(St.Louis.MetDat, "St Louis MetDat 1987-2021.csv", row.names = FALSE)


#COMPARE WITH METDAT FROM EPA

EPA.MetDat <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/CART/MetDat/Ozone-met files 2005-20/Ozone-met data for CART 2005-20 -St Louis.csv", header=TRUE)

EPA.MetDat.narrow <- EPA.MetDat %>%
  dplyr::select(1,4,17,25,55) %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  dplyr::distinct()

Compare.MetDat <- inner_join(St.Louis.MetDat, EPA.MetDat.narrow, by = c("date"="Date"))
Compare.MetDat <- Compare.MetDat %>%
  dplyr::mutate(Tavgpm.diff = T.avgpm - tavgpm, RHavgmid.diff = RH.avgmid - rhavgmid, ws2day.diff = ws.2day - ws2day, 
                year = format(date, "%Y"), year.tag = ifelse(year == "2015", "2015", "other")) %>%
  pivot_longer(ends_with("diff"), names_to = "Param", values_to = "diff", names_pattern = "(.*)\\.diff")

#Plot boxplots of the differences between EPA's and my values

    # a <- ggplot(Compare.MetDat, aes(x = Param, y = diff)) + geom_boxplot() + facet_wrap(~year.tag, scales = "free_y") + 
    #   ggtitle("Difference between LADCO- and EPA-calculated met - STL") + ylab("Difference") + xlab(NULL) + 
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, vjust=0.5),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         strip.text = element_text(size = 12),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = "St Louis met select - LADCO vs EPA diffs.png", plot=a, width = 8, height = 6)

#Calculate summary statistics

Compare.MetDat.no.2015 <- Compare.MetDat %>%
  dplyr::filter(year != "2015") %>%
  dplyr::select(3,15,16) %>%
  pivot_wider(names_from = "Param", values_from = "diff")

# require("psych")

summary(Compare.MetDat.no.2015)

#calculate the 1st and 99th percentile values
Compare.MetDat.no.2015.stats <- Compare.MetDat.no.2015 %>%
  pivot_longer(2:4, names_to = "Param", values_to = "diff") %>%
  dplyr::group_by(Param) %>%
  dplyr::summarise(perc.2.5 = quantile(diff, probs = 0.025), perc.97.5 = quantile(diff, probs = 0.975)) %>%
  dplyr::ungroup()

# write.csv(Compare.MetDat.no.2015.stats, "St Louis - comparison stats for key MetDat params.csv", row.names = FALSE)


##DETERMINE OZONE-CONDUCIVE DAYS
Ozone.conduc.days <- St.Louis.MetDat %>%
  dplyr::filter(T.avgpm > 86.6, RH.avgmid <= 50.4, ws.2day <= 3.8) %>%
  dplyr::select(1,3)

# write.csv(Ozone.conduc.days, "St Louis - ozone conducive days - 1987-2021.csv", row.names = FALSE)


#Look at key met parameters on weekday and weekend ozone-conducive days - look at whether there are large differences in different year groups

met.ozone.conduc.days.St.Louis <- St.Louis.MetDat %>%
  dplyr::filter(T.avgpm > 86.6, RH.avgmid <= 50.4, ws.2day <= 3.8) %>%
  dplyr::mutate(year = as.numeric(format(date, "%Y")), day.of.week = format(date, "%a"), day.of.week = ifelse(day.of.week == "Sun", "weekend", 
                                                                       ifelse(day.of.week %in% c("Tue","Wed","Thu"), "weekday","other")),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::filter(day.of.week %in% c("weekend","weekday")) %>%
  dplyr::group_by(station, yr.bins) %>%  #show average of all nodes
  # dplyr::summarise(Tavgpm = mean(T.avgpm, na.rm=TRUE), RHavgmd = )
  dplyr::summarize(weekday.Tavgpm = mean(T.avgpm[day.of.week=="weekday"], na.rm=TRUE), weekend.Tavgpm = mean(T.avgpm[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.RHavgmid = mean(RH.avgmid[day.of.week=="weekday"]), weekend.RHavgmid = mean(RH.avgmid[day.of.week=="weekend"]),
  weekday.ws2d = mean(ws.2day[day.of.week=="weekday"], na.rm=TRUE), weekend.ws2d = mean(ws.2day[day.of.week=="weekend"], na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(weekday.Tavgpm:weekend.ws2d, names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)") %>%
  pivot_longer(Tavgpm:ws2d, names_to = "met.param", values_to = "values")


    a <- ggplot(data = met.ozone.conduc.days.St.Louis, aes(x=yr.bins, y=values, color = day.type, group = day.type)) + 
      facet_wrap(.~met.param, scales = "free_y") + geom_line() + 
      scale_color_brewer(palette = "Paired") + xlab(NULL) + ylab(NULL) + ggtitle("Important Meteorological Parameters \nSt. Louis O3-Conducive Days") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank())
    ggsave(filename = "St Louis Met params - O3-cond days.png", plot = a, width = 8, height = 4)


## CHICAGO #####################################################################

# CHICAGO AIRPORT MET DATA (Import HYSPLIT data separately below)

Chicago.met <- read.csv("ORD met 1987-2021.csv", header = TRUE)

#select just May-September data & calculate average parameters. Convert ws/wd to u and v component winds to average

Chicago.hour.means <- Chicago.met %>%
  dplyr::mutate(Time.CST.CDT = as.POSIXct(Time.CST.CDT, "%m/%d/%Y %H:%M", tz = "America/Chicago"), Date.time.CST = Time.CST.CDT - 3600,
                month = format(Date.time.CST, "%m")) %>% #May-September is all CDT so this conversion works for those months to CST
  dplyr::filter(month %in% c("04","05","06","07","08","09")) %>%
  pivot_longer(T.deg.F:sea.level.P.mb, names_to = "Param", values_to = "values") %>%
  dplyr::mutate(values = as.numeric(ifelse(values == "M", NA, values))) %>% #replace missing values ("M") with NAs
  pivot_wider(names_from = "Param", values_from = "values") %>%
  dplyr::mutate(hour = as.numeric(format(Date.time.CST, "%H")), date = as.Date(Date.time.CST, tz="America/Chicago"),
                u.wind.mph = -abs(ws.mph)*sin(wd*pi/180), v.wind.mph = -abs(ws.mph)*cos(wd*pi/180)) %>% #convert to u and v vector winds
  dplyr::select(1,3,12,11,6:10,13,14) %>% ##!!!!!!!!!!!!!!!!!!!!!!!!!!!ensure we keep u.wind and v.wind!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  pivot_longer(T.deg.F:v.wind.mph, names_to = "Param", values_to = "values") %>%
  dplyr::group_by(station, elevation.m, date, hour, Param) %>%
  dplyr::summarize(hour.mean = mean(values, na.rm = TRUE)) %>% #calculate hourly means (since some hours have >1 value)
  dplyr::ungroup() %>%
  pivot_wider(names_from = "Param", values_from = "hour.mean") %>%
  dplyr::mutate(ws.m.s = ws.mph * 0.44704, T.deg.K = (T.deg.F + 459.67)*5/9, StP.mb = sea.level.P.mb * exp(-elevation.m/(T.deg.K*29.263)))

#Calculate equivalent parameters to MetDat parameters from EPA (for most important parameters in the CART analysis for ozone-conducive days)
Chicago.MetDat <- Chicago.hour.means %>%

  dplyr::group_by(station, elevation.m, date) %>%
  dplyr::summarise(T.avgpm = mean(T.deg.F[hour %in% c(13,14,15,16)], na.rm = TRUE), Tmax = max(T.deg.F, na.rm = TRUE),
                   RH.avgmid = mean(RH[hour %in% c(10,11,12,13,14,15,16)], na.rm = TRUE),
                   v.wind.m.s.day = mean(v.wind.mph*0.44704, na.rm = TRUE), ws.m.s.day = mean(ws.m.s, na.rm = TRUE), 
                   #u.wind.m.s.am = mean(u.wind.mph[hour %in% c(7,8,9,10)]*0.44704, na.rm = TRUE)) %>%
                   StP.avg = mean(StP.mb, na.rm = TRUE)) %>%
  dplyr::ungroup()

#Merge with complete list of days (in case missing data for days - need to correctly lag data below)
complete.days <- data.frame(date = seq(as.Date("1987-01-01"), as.Date("2021-12-31"), by="1 day"))
complete.days <- complete.days %>%
  dplyr::mutate(month = format(date, "%m")) %>%
  dplyr::filter(month %in% c("04","05","06","07","08","09"))

Chicago.MetDat <- full_join(complete.days, Chicago.MetDat, by = c("date"))

Chicago.MetDat <- Chicago.MetDat %>%
  dplyr::mutate(year = format(date, "%Y")) %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(station, elevation.m, year) %>%
  dplyr::mutate(lag.S.wn = lag(v.wind.m.s.day, n=1), lag.StP.avg = lag(StP.avg, n=1), lag.ws = lag(ws.m.s.day, n=1), 
                ws.2day = (ws.m.s.day + lag.ws)/2) %>%
  dplyr::ungroup() %>%
  dplyr::filter(month %in% c("05","06","07","08","09")) %>% #drop April now that the lag calculation is done
  dplyr::select(3,1,2,11,5:7,12:13,15)


#Pull in transport parameters from HYSPLIT analysis. Pull in 1987-2000 and then 2021 data in one pull and 2001-2020 data in a separate pull
#I ran the 1987-2000 and 2021 HYSPLIT model and post-processed the data myself.
#EPA ran the HYSPLIT model for 2001-2019, and Tsengel ran it for 2000. They provided the post-processed results.

transport.87.00 <- read.csv("C:/Users/afdic/OneDrive/Documents/HYSPLIT/HYSPLIT transport terms - ORD 1987-2000.csv", header = TRUE)
transport.87.00 <- dplyr::mutate(transport.87.00, Date = as.Date(Date))

#IMPORT EPA PARAMETERS

transport.01.20 <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/CART/MetDat/Ozone-met files 2001-20/Ozone-met data for CART 2001-20 -Chicago- Cook Co.csv", header = TRUE)
transport.01.20 <- transport.01.20 %>%
  dplyr::select(4,27:31) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Date = as.Date(Date))

#Import 2015 separately - had dropped from CART analysis because temperature data was off, but HYSPLIT data should be fine

transport.2015 <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/CART/MetDat/Ozone-met files 2005-19/Ozone-met data for CART 2005-19 -Chicago- Cook Co.csv", header = TRUE)
transport.2015 <- transport.2015 %>%
  dplyr::filter(year == 2015) %>%
  dplyr::select(4,47:51) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Date = as.Date(Date))

#Import 2021 HYSPLIT data
transport.2021 <- read.csv("C:/Users/afdic/OneDrive/Documents/ozone/CART/MetDat/2021 MetDat from EPA/daily-wide-HYSPLIT-only-2021-LADCO.csv", header = TRUE)
transport.2021.ORD <- transport.2021 %>%
  dplyr::filter(site_id == 895 & month %in% c(5,6,7,8,9)) %>%
  dplyr::mutate(Date = as.Date(paste(year,month,day, sep="-"))) %>%
  dplyr::select(16,5:9) %>%
  dplyr::distinct() %>%
  dplyr::rename(transouth = tranv, tranwest = tranu)

#Combine LADCO- and EPA-calculated transport parameters

transport.87.21 <- bind_rows(transport.87.00, transport.01.20, transport.2015, transport.2021.ORD)

#Add on to other met data

Chicago.MetDat <- full_join(Chicago.MetDat, transport.87.21, by=c("date"="Date"))
# write.csv(Chicago.MetDat, "Chicago MetDat 1987-2021.csv", row.names = FALSE)


##DETERMINE OZONE-CONDUCIVE DAYS - for each of three areas of Chicago
#North
Ozone.conduc.days.Chicago.North <- Chicago.MetDat %>%
  dplyr::filter((T.avgpm > 84.9 & tranwest <= 219.6) |
                  T.avgpm > 84.9 & tranwest > 219.6 & ws.2day <= 3.5) %>%
  dplyr::select(1,2)

# write.csv(Ozone.conduc.days.Chicago.North, "Chicago North - ozone conducive days - 1987-2021.csv", row.names = FALSE)

#Central
Ozone.conduc.days.Chicago.Central <- Chicago.MetDat %>%
  dplyr::filter((T.avgpm > 85.6 & trandis <= 618.2)) %>%
  dplyr::select(1,2)

# write.csv(Ozone.conduc.days.Chicago.Central, "Chicago Central - ozone conducive days - 1987-2021.csv", row.names = FALSE)

#Indiana
Ozone.conduc.days.Chicago.IN <- Chicago.MetDat %>%
  dplyr::filter((Tmax > 79.1 & Tmax <= 85.0 & lag.StP.avg > 994.4 & lag.S.wn > -0.1) |
                  Tmax > 85.0 & RH.avgmid <= 59.8) %>%
  dplyr::select(1,2)

# write.csv(Ozone.conduc.days.Chicago.IN, "Chicago Indiana - ozone conducive days - 1987-2021.csv", row.names = FALSE)


#Look at key met parameters on weekday and weekend ozone-conducive days - look at whether there are large differences in different year groups

#Chicago North
met.ozone.conduc.days.Chicago.North <- Chicago.MetDat %>%
  dplyr::filter((T.avgpm > 84.9 & tranwest <= 219.6) |
                  T.avgpm > 84.9 & tranwest > 219.6 & ws.2day <= 3.5) %>%
  dplyr::mutate(day.of.week = format(date, "%a"), day.of.week = ifelse(day.of.week == "Sun", "weekend", 
                                                                       ifelse(day.of.week %in% c("Tue","Wed","Thu"), "weekday","other")),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::filter(day.of.week %in% c("weekend","weekday")) %>%
  dplyr::group_by(station, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.Tavgpm = mean(T.avgpm[day.of.week=="weekday"], na.rm=TRUE), weekend.Tavgpm = mean(T.avgpm[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.Tmax = mean(Tmax[day.of.week=="weekday"], na.rm=TRUE), weekend.Tmax = mean(Tmax[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.RHavgmid = mean(RH.avgmid[day.of.week=="weekday"]), weekend.RHavgmid = mean(RH.avgmid[day.of.week=="weekend"]),
                   weekday.lagSwn = mean(lag.S.wn[day.of.week=="weekday"], na.rm=TRUE), weekend.lagSwn = mean(lag.S.wn[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.lagStP = mean(lag.StP.avg[day.of.week=="weekday"], na.rm=TRUE), weekend.lagStP = mean(lag.StP.avg[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.ws2d = mean(ws.2day[day.of.week=="weekday"], na.rm=TRUE), weekend.ws2d = mean(ws.2day[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.trandis = mean(trandis[day.of.week=="weekday"], na.rm=TRUE), weekend.trandis = mean(trandis[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.tranwest = mean(tranwest[day.of.week=="weekday"], na.rm=TRUE), weekend.tranwest = mean(tranwest[day.of.week=="weekend"], na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(weekday.Tavgpm:weekend.tranwest, names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)") %>%
  pivot_longer(Tavgpm:tranwest, names_to = "met.param", values_to = "values")


    a <- ggplot(data = met.ozone.conduc.days.Chicago.North, aes(x=yr.bins, y=values, color = day.type, group = day.type)) + 
      facet_wrap(.~met.param, scales = "free_y") + geom_line() + 
      scale_color_brewer(palette = "Paired") + xlab(NULL) + ylab(NULL) + ggtitle("Important Meteorological Parameters \nChicago North O3-Conducive Days") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank())
    ggsave(filename = "Chicago North Met params - O3-cond days.png", plot = a, width = 8, height = 6)
    
    
#Chicago Central
met.ozone.conduc.days.Chicago.Central <- Chicago.MetDat %>%
  dplyr::filter(T.avgpm > 85.6 & trandis <= 618.2) %>%
  dplyr::mutate(day.of.week = format(date, "%a"), day.of.week = ifelse(day.of.week == "Sun", "weekend", 
                                                                       ifelse(day.of.week %in% c("Tue","Wed","Thu"), "weekday","other")),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::filter(day.of.week %in% c("weekend","weekday")) %>%
  dplyr::group_by(station, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.Tavgpm = mean(T.avgpm[day.of.week=="weekday"], na.rm=TRUE), weekend.Tavgpm = mean(T.avgpm[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.Tmax = mean(Tmax[day.of.week=="weekday"], na.rm=TRUE), weekend.Tmax = mean(Tmax[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.RHavgmid = mean(RH.avgmid[day.of.week=="weekday"]), weekend.RHavgmid = mean(RH.avgmid[day.of.week=="weekend"]),
                   weekday.lagSwn = mean(lag.S.wn[day.of.week=="weekday"], na.rm=TRUE), weekend.lagSwn = mean(lag.S.wn[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.lagStP = mean(lag.StP.avg[day.of.week=="weekday"], na.rm=TRUE), weekend.lagStP = mean(lag.StP.avg[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.ws2d = mean(ws.2day[day.of.week=="weekday"], na.rm=TRUE), weekend.ws2d = mean(ws.2day[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.trandis = mean(trandis[day.of.week=="weekday"], na.rm=TRUE), weekend.trandis = mean(trandis[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.tranwest = mean(tranwest[day.of.week=="weekday"], na.rm=TRUE), weekend.tranwest = mean(tranwest[day.of.week=="weekend"], na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(weekday.Tavgpm:weekend.tranwest, names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)") %>%
  pivot_longer(Tavgpm:tranwest, names_to = "met.param", values_to = "values")
    
    
    a <- ggplot(data = met.ozone.conduc.days.Chicago.Central, aes(x=yr.bins, y=values, color = day.type, group = day.type)) + 
      facet_wrap(.~met.param, scales = "free_y") + geom_line() + 
      scale_color_brewer(palette = "Paired") + xlab(NULL) + ylab(NULL) + ggtitle("Important Meteorological Parameters \nChicago Central O3-Conducive Days") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank())
    ggsave(filename = "Chicago Central Met params - O3-cond days.png", plot = a, width = 8, height = 6)
    
#Chicago Indiana
met.ozone.conduc.days.Chicago.Indiana <- Chicago.MetDat %>%
  dplyr::filter((Tmax > 79.1 & Tmax <= 85.0 & lag.StP.avg > 994.4 & lag.S.wn > -0.1) |
                  Tmax > 85.0 & RH.avgmid <= 59.8) %>%
  dplyr::mutate(day.of.week = format(date, "%a"), day.of.week = ifelse(day.of.week == "Sun", "weekend", 
                                                                       ifelse(day.of.week %in% c("Tue","Wed","Thu"), "weekday","other")),
                yr.bins = ifelse(year <= 1991, "1987-91", ifelse(year <= 1996, "1992-96", ifelse(year <= 2001, "1997-01",
                                                                                                 ifelse(year <= 2006, "2002-06", ifelse(year <= 2011, "2007-11", ifelse(year <= 2016, "2012-16", "2017-21"))))))) %>%
  dplyr::filter(day.of.week %in% c("weekend","weekday")) %>%
  dplyr::group_by(station, yr.bins) %>%  #show average of all nodes
  dplyr::summarize(weekday.Tavgpm = mean(T.avgpm[day.of.week=="weekday"], na.rm=TRUE), weekend.Tavgpm = mean(T.avgpm[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.Tmax = mean(Tmax[day.of.week=="weekday"], na.rm=TRUE), weekend.Tmax = mean(Tmax[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.RHavgmid = mean(RH.avgmid[day.of.week=="weekday"]), weekend.RHavgmid = mean(RH.avgmid[day.of.week=="weekend"]),
                   weekday.lagSwn = mean(lag.S.wn[day.of.week=="weekday"], na.rm=TRUE), weekend.lagSwn = mean(lag.S.wn[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.lagStP = mean(lag.StP.avg[day.of.week=="weekday"], na.rm=TRUE), weekend.lagStP = mean(lag.StP.avg[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.ws2d = mean(ws.2day[day.of.week=="weekday"], na.rm=TRUE), weekend.ws2d = mean(ws.2day[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.trandis = mean(trandis[day.of.week=="weekday"], na.rm=TRUE), weekend.trandis = mean(trandis[day.of.week=="weekend"], na.rm=TRUE),
                   weekday.tranwest = mean(tranwest[day.of.week=="weekday"], na.rm=TRUE), weekend.tranwest = mean(tranwest[day.of.week=="weekend"], na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(weekday.Tavgpm:weekend.tranwest, names_to = c("day.type",".value"), names_pattern = "(.*)\\.(.*)") %>%
  pivot_longer(Tavgpm:tranwest, names_to = "met.param", values_to = "values")
    
    
    a <- ggplot(data = met.ozone.conduc.days.Chicago.Indiana, aes(x=yr.bins, y=values, color = day.type, group = day.type)) + 
      facet_wrap(.~met.param, scales = "free_y") + geom_line() + 
      scale_color_brewer(palette = "Paired") + xlab(NULL) + ylab(NULL) + ggtitle("Important Meteorological Parameters \nChicago Indiana O3-Conducive Days") +
      theme(axis.text = element_text(size = 10, color = "black"),
            axis.text.x = element_text(size = 10, angle=90, vjust=0.5),
            axis.title.y = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_blank())
    ggsave(filename = "Chicago Indiana Met params - O3-cond days.png", plot = a, width = 8, height = 6)
    
    
    






