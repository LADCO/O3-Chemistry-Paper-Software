############################################################################################
#                                                                                          #
#         RUNNING CART FOR KENOSA & LAKE COs.  - FOR DATA 2001 - 2020 (May-Sept)           #
#              USING MET DATA FROM CHICAGO O'HARE AND FROM MILWAUKEE AIRPORT               #
#                   FOR WEEKDAY-WEEKEND ANALYSIS - FEBRUARY 2022                           #
#                                                                                          #
############################################################################################


require("scales")
require("lubridate")
require("tidyverse")
require("rattle")
require("partykit")
require("RColorBrewer")
require("caret")

## IMPORT DATA - USING CHICAGO O'HARE MET DATA

O3.met <- read.csv("./MetDat/Ozone-met files 2001-20/Ozone-met data for CART 2001-20 -Chicago- Kenosha-Lake.csv", header=TRUE)
O3.met$weekday <- as.numeric(factor(O3.met$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), labels=1:7)) #converts weekday to numeric (1=Monday, 7=Sunday)


## DO A FEW EXPLORATORY PLOTS 

    O3.Tmax.plot <- ggplot(O3.met, aes(x=tmax, y=MDA8)) + geom_point() + ggtitle("Ozone-Tmax plot - Kenosha-Lake, 2001-2020") +
      xlab("Maximum daily Temperature (F)") + ylab("MDA8 ozone (ppb)")
    ggsave(filename = "./2001-2020 CART - for W-W/Ozone-Tmax plot - Kenosha-Lake v OHare 2001-2020.png", plot = O3.Tmax.plot, width = 6, height =4)

    O3.hist <- ggplot(O3.met, aes(x=MDA8)) + geom_histogram(binwidth=2, color="black", fill="white") +
      xlab("MDA8 ozone (ppb)") + ylab("Count") + ggtitle("MDA8 Ozone - Kenosha-Lake, 2001-2020")
    ggsave(filename = "./2001-2020 CART - for W-W/Ozone histogram - Kenosha-Lake v OHare 2001-2020.png", plot = O3.hist, width = 6, height =4)

# already removed 2015 data because temperatures look really off - way too high. 
    T.hist.by.yr <- ggplot(O3.met, aes(x=tavgpm)) + geom_histogram(binwidth=2, color="black", fill="white") + facet_wrap(. ~ year) +
      xlab("pm T") + ylab("Count") + ggtitle("Avg pm temp - Kenosha-Lake, 2001-2020")
    ggsave(filename = "./2001-2020 CART - for W-W/pm temp histogram by yr - Kenosha-Lake v OHare 2001-2020.png", plot = T.hist.by.yr, width = 6, height =4)
  
    
#Get list of ozone monitors used
unique(O3.met$Site)
unique(O3.met$Site.Name.new)
    

# Run CART analysis - using Conditional Inference methods (ctree), which avoids some of the biases in the variable choice in the traditional method

## ANALYSIS ON FIRST 10 YEARS OF DATA THEN APPLY MODEL TO SUBSEQUENT 10 YEARS OF DATA. 20 years is getting to be a long time, with ozone changing a lot over that time. It's probably better to train using one decade and then apply to the other decade.
    
O3.met.2001.10 <- filter(O3.met, year <= 2010)
O3.met.2011.20 <- filter(O3.met, year > 2010)
    
set.seed(100)
tree2 <- ctree(MDA8 ~ dpavg + dpmax + foghrs + hazehrs + mrmax + precip + rainhrs + rhavg + rhavgmid + rhavgnight + slpavg + stpavg + 
                 taavg + tamax + tamin + tavgam + tavgpm + tmax + trandir + trandis + tranwest + transouth + tranw + avg_W_win + avg_W_am + avg_W_pm +
                 avg_S_win + avg_S_am + avg_S_pm + wdavg + wdavgam + wdavgpm + wndrun + wsavg + wsavgam + wsavgpm + lagtmax + lagstpavg + lagwsavg + lag_W_wn +
                 lag_S_wn + tempchange + presschange + weekday + tem2day + tem3day + ws2day + ws3day,
                 data = O3.met.2001.10,
               # control = ctree_control(maxdepth=4, minsplit = 400, minbucket=200, maxsurrogate=3))
                 control = ctree_control(maxdepth=4, minsplit=200, minbucket=100, maxsurrogate=3))


cor(predict(tree2, newdata=O3.met),O3.met$MDA8)^2 #get R^2 value


plot(tree2)
print(tree2)
    
    plot.new()
    png(file="./2001-2020 CART - for W-W/Cond-Inf CART ctree - Kenosha-Lake 01-10 OHare met-minbuck 100.png", width=2800, height=1500, res=200)
    # png(file="./2001-2020 CART - for W-W/Cond-Inf CART ctree - Kenosha-Lake 01-20 OHare met-minbuck 150.png", width=2800, height=1500, res=200)
    plot(tree2)
    dev.off()
    
partykit:::.list.rules.party(tree2) #gives the splits for each node
    
    
# info_node(node_party(tree2)) #prints test statistics for each variable

O3.met.node <- predict(tree2, type="node") #gives list of nodes each observation belongs to
O3.met.2001.10 <- mutate(O3.met.2001.10, node=O3.met.node, yrs = "2001-2010") #adds node list on to main data frame
    

##APPLY MODEL (from 2001-2010) TO 2011-20 DATA

nodes.11.20 <- predict(tree2, newdata = O3.met.2011.20, type = "node")
O3.met.2011.20 <- mutate(O3.met.2011.20, node=nodes.11.20, yrs = "2011-2020") #adds node list on to main data frame    

#Combine all data again for plotting

O3.met.all <- rbind(O3.met.2001.10, O3.met.2011.20)

#export data
node.day <- dplyr::select(O3.met.all, 1:7,57:61)
# write.csv(node.day, "Kenosha-Lake CART data - 2001-2020.csv", row.names = FALSE)


#export met data - just the parameters that were relevant to high-ozone nodes across all CART analyses
select.met.data <- dplyr::select(O3.met.all, 1,2,4,6,7,61,37,35,33,34,50,47,16,17,20,24,25,26,28,31,29,30,41,55)
select.met.data <- dplyr::distinct(select.met.data) #get rid of duplicates (for multiple ozone sites)
# write.csv(select.met.data, "Kenosha-Lake CART data - select met - 2001-2020.csv", row.names = FALSE)



#Plot boxplots of ozone in each node for each set of years

    # a <- ggplot(data=O3.met.all) + facet_grid(.~node) +
    #   geom_hline(aes(yintercept=70), color="gray") + 
    #   geom_boxplot(aes(x=yrs, y=MDA8, fill=yrs, group = yrs), alpha=0.7) +
    #   xlab(NULL) + labs(y = expression("MDA8 Ozone (ppb)")) +
    #   ggtitle("MDA8 Ozone in CART Nodes \nCART run using 2001-2010 data") +
    #   # coord_cartesian(ylim=c(1.7,8)) + scale_y_continuous(breaks = seq(0,8,by=1)) +
    #   scale_fill_brewer(palette="Set1") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_blank(),
    #         # axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
    #         axis.title = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 16, hjust = 0))
    # ggsave(filename = "CART boxplots by nodes and year set - 2001-20 - Kenosha-Lake.png", plot = a, width = 8, height =5)


#Determine which nodes have >=10% of MDA8 values >70 ppb. (Use these in the weekday/weekend analysis)
nodes.gt70 <- O3.met.all %>%
  group_by(yrs, node) %>%
  dplyr::summarize(gt70.perc = length(MDA8[MDA8>70]) / length(MDA8) * 100, count.days = length(MDA8), MDA8.mean = mean(MDA8, na.rm=TRUE)) %>%
  ungroup()
write.csv(nodes.gt70, "CART 2001-20 - days gt70 in each node - Kenosha-Lake.csv", row.names=FALSE)

#Plot histogram
    # a <- ggplot(data = O3.met.all) + facet_wrap(~ node) +
    #   geom_histogram(aes(MDA8), binwidth = 2) + geom_vline(aes(xintercept = 70), color="darkgray") +
    #   xlab("MDA8 ozone (ppb)") + ylab("Frequency") + ggtitle(paste0("Histogram of MDA8 ozone by CART node-Kenosha-Lake 2001-20")) +
    #   # scale_y_continuous(limits = c(-12.2,7.9), breaks = seq(-12,8,by=2), minor_breaks = seq(-12,8,by=0.5)) +
    #   # scale_color_brewer(palette="Set1") +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #         axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
    #         axis.title.y = element_text(size = 16),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_blank(),
    #         plot.title = element_text(size = 18, hjust = 0))
    # ggsave(filename = paste0("Histogram of MDA8 ozone by node - Kenosha-Lake 2001-20.png"), plot=a, width = 9, height = 6.5)


#Plot average ozone in node by year

## FIRST MAKE SURE NO NODES HAVE <2 DAYS IN THEM!!!!! If they do, eliminate that node for that year because it may not be representative (Used 3 day cut before, but try 2-day cut here)
# count.by.year <- data.frame(table(predict(tree2), O3.met$year)) #gives a table with what the number of counts in each node each year
count.by.year <- O3.met.all %>%
  dplyr::group_by(year,node) %>%
  dplyr::summarize(count = length(MDA8)) %>%
  dplyr::ungroup()

# #Need to eliminate three years with just two or three days
O3.met.filter <- O3.met.all
# O3.met.filter <- filter(O3.met, (year != 2007 | node != 9) & (year != 2014 | node != 24))
# # O3.met.filter <- filter(O3.met, (year != 2007 | node != 9) & (year != 2006 | node != 25) & (year != 2014 | node !=28)) #minbucket=150


O3.avg.node <- O3.met.filter %>%
  group_by(node,year) %>%
  dplyr::summarize(mean.O3 = mean(MDA8)) %>%
  ungroup()

high.O3.avg.node <- filter(O3.avg.node, node %in% c(23,24,27,28,30,31)) #selects just nodes with >10% of days with MDA8 >70 ppb
high.O3.avg.node$node <- as.factor(high.O3.avg.node$node)

#Mean ozone plot
    # O3.node.trend.plot <- ggplot(high.O3.avg.node, aes(x=year,y=mean.O3, color=node)) + geom_point() + geom_smooth(method=lm, se = FALSE , size=0.75) +
    #   # scale_y_continuous(breaks=seq(0,90,by=5)) + # coord_cartesian(ylim = c(40,90)) +
    #   scale_x_continuous(breaks=seq(2002,2020,by=2)) + ylab("Mean Ozone (ppb)") + xlab(NULL) +
    #   scale_color_brewer(palette="Set1") + ggtitle("2001-2020 Trends by CART Node: Kenosha-Lake Counties") +
    #   annotate("text", x= 2008, y=83, label="Only nodes with >10% of MDA8 values >70 ppb", size = 4, hjust=0, vjust=1) +
    #   theme(axis.text = element_text(size = 14, color = "black"),
    #                 axis.title = element_text(size = 16),
    #                 legend.text = element_text(size = 12),
    #                 legend.title = element_text(size = 12),
    #                 plot.title = element_text(size = 15, hjust = 0))
    # ggsave(filename="./2001-2020 CART - for W-W/O3 trends 2001-20 Kenosha-Lake means - OHare met.png", plot=O3.node.trend.plot, width=7, height=4)
    # # ggsave(filename="./2001-2020 CART - for W-W/O3 trends 2001-20 Kenosha-Lake means - OHare met-minbuck 150.png", plot=O3.node.trend.plot, width=7, height=4)


#Determine variable importance using the caret package

#For 2001-2010
tree.caret <- caret::train(MDA8 ~ dpavg + dpmax + foghrs + hazehrs + mrmax + precip + rainhrs + rhavg + rhavgmid + rhavgnight + slpavg + stpavg +
                             taavg + tamax + tamin + tavgam + tavgpm + tmax + trandir + trandis + tranwest + transouth + tranw + avg_W_win + avg_W_am + avg_W_pm +
                             avg_S_win + avg_S_am + avg_S_pm + wdavg + wdavgam + wdavgpm + wndrun + wsavg + wsavgam + wsavgpm + lagtmax + lagstpavg + lagwsavg + lag_W_wn +
                             lag_S_wn + tempchange + presschange + weekday + tem2day + tem3day + ws2day + ws3day,
                            data = O3.met.2001.10, method="ctree", na.action=na.exclude)
tree.caret.var.imp <- varImp(tree.caret)

#Plot results
    # png("./2001-2020 CART - for W-W/variable importance 2001-10 Kenosha-Lake OHare met.png", height=500, width=300)
    # plot(tree.caret.var.imp, top=20)
    # dev.off()

#For 2001-2020
tree.caret <- caret::train(MDA8 ~ dpavg + dpmax + foghrs + hazehrs + mrmax + precip + rainhrs + rhavg + rhavgmid + rhavgnight + slpavg + stpavg +
                             taavg + tamax + tamin + tavgam + tavgpm + tmax + trandir + trandis + tranwest + transouth + tranw + avg_W_win + avg_W_am + avg_W_pm +
                             avg_S_win + avg_S_am + avg_S_pm + wdavg + wdavgam + wdavgpm + wndrun + wsavg + wsavgam + wsavgpm + lagtmax + lagstpavg + lagwsavg + lag_W_wn +
                             lag_S_wn + tempchange + presschange + weekday + tem2day + tem3day + ws2day + ws3day,
                           data = O3.met.all, method="ctree", na.action=na.exclude)
tree.caret.var.imp <- varImp(tree.caret)

#Plot results
    # png("./2001-2020 CART - for W-W/variable importance 2001-20 Kenosha-Lake OHare met.png", height=500, width=300)
    # plot(tree.caret.var.imp, top=20)
    # dev.off()

#For 2011-2020
tree.caret <- caret::train(MDA8 ~ dpavg + dpmax + foghrs + hazehrs + mrmax + precip + rainhrs + rhavg + rhavgmid + rhavgnight + slpavg + stpavg +
                             taavg + tamax + tamin + tavgam + tavgpm + tmax + trandir + trandis + tranwest + transouth + tranw + avg_W_win + avg_W_am + avg_W_pm +
                             avg_S_win + avg_S_am + avg_S_pm + wdavg + wdavgam + wdavgpm + wndrun + wsavg + wsavgam + wsavgpm + lagtmax + lagstpavg + lagwsavg + lag_W_wn +
                             lag_S_wn + tempchange + presschange + weekday + tem2day + tem3day + ws2day + ws3day,
                           data = O3.met.2011.20, method="ctree", na.action=na.exclude)
tree.caret.var.imp <- varImp(tree.caret)

#Plot results
    # png("./2001-2020 CART - for W-W/variable importance 2011-20 Kenosha-Lake OHare met.png", height=500, width=300)
    # plot(tree.caret.var.imp, top=20)
    # dev.off()



