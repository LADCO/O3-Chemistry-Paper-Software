############################################################################################
#                                                                                          #
#            CURVE FITTING FOR PUSEDE-STYLE PLOTS (WEEKDAY-WEEKEND MDA8 vs NO2)            #
#                  BASED ON SCRIPT FROM SALLY PUSEDE AND OLIVIA SALMON (WDNR)              #
#                                                                                          #
#                       FOR WEEKDAY-WEEKEND ANALYSIS PROJECT                               #
#                 FOR ST. LOUIS AND CHICAGO CLUSTERS - FOR PUBLICATION                     #
#                                                                                          #
############################################################################################

require("grid")
require("gridExtra")
require("plyr")
require("tidyverse")
require("stats")
require("minpack.lm")
require("RColorBrewer")


## IMPORT WEEKDAY-WEEKEND MDA8 AND NO2 VALUES FOR PLOTTING/ANALYSIS

#MDA8 data
St.Louis.WW.MDA8 <- read.csv("St Louis cluster W-W diffs-means-by cluster - 1987-2021.csv", header=TRUE)

St.Louis.WW.MDA8 <- St.Louis.WW.MDA8 %>%
  dplyr::filter(weekend.count >= 9) %>% #remove clusters/year groups with <9 days
  dplyr::select(2:6) %>%
  pivot_longer(4:5, names_to = "day.type", values_to = "MDA8") %>%
  dplyr::mutate(day.type = ifelse(day.type == "weekday.mean", "weekday", "weekend"))

#NO2 data
NO2.data <- read.csv("Weekday-weekend NO2 concentrations - Chicago-St Louis.csv", header = TRUE)

STL.NO2.data <- NO2.data %>%
  dplyr::filter(CBSA == "St. Louis")

#Combine
STL.WW.MDA8.NO2 <- full_join(St.Louis.WW.MDA8, STL.NO2.data, by=c("yr.bins","day.type"))

STL.WW.MDA8.NO2 <- STL.WW.MDA8.NO2 %>%
  dplyr::select(6,1:5,7) %>%
  dplyr::rename(NO2 = area.mean) %>%
  dplyr::mutate(Cluster.name = ifelse(Cluster.name == "55S*", "55S", Cluster.name)) #dropping * because the figure won't save with that in the filename


## DROP 1987-91 DATA 
STL.WW.MDA8.NO2 <- dplyr::filter(STL.WW.MDA8.NO2, yr.bins != "1987-91")



################################ Analytical O3 model from S. Pusede #######################################
### Rates ###
R <-8.3144621 
T <- 300               # K
M <- 2.44E19           # molec/cm3/s

# calculate rate of HO2 + HO2 -> H2O2 + O2
kho2ho2 <- (2.2*(10^-13))*exp(4989/(R*T))

# calculate rate of HO2 +NO -> OH + NO2
kho2no <- (3.6e-12)*exp(2245/(R*T))

# calculate rate of ro2 + ho2 --> rooh + o2
kho2ro2 <- 2.9e-13*exp(1300./T)

# calculate rate of OH + NO2 + M -> HNO3 + M
k9o <- (1.48e-30*((T/300)^(-3))*M)
k9oo <- 2.58e-11*((T/300)^(0)) 
kohno2new <- (k9o/(1+(k9o/k9oo)))*(0.6^((1+((log10(k9o/k9oo))^2))^(-1.0)))

# calculate rate of OH + NO2 + M -> HNO3 + M
kro2no <- (2.8e-12)*exp(285/T)

# calculate rate of RO2 + ISOPO2 -> isoprene aldehyde + O2 + alcohol?
kro2ro2 <- 2.4e-12

#  includes alkyl nitrate production - turn off by setting alpha = 0
k1 <- kohno2new
k2 <- kho2no     # effective ro2 or ho2 +no
k3 <- kro2no
ka <- kho2ho2
kb <- kho2ro2
kc <- kro2ro2
ks <- 2*(ka + kb + kc)

phox <- 0.2*M*1e-12   # ppt/s converted to molec/cm3/s    # INPUT; originally 0.2
#vocr = 130          # Hz (VOCR = k_OH*VOC)              # TUNE; originally 10 (for 1999-2002) GOING TO CALC THIS############

#noxppb <- seq(from = 0.001, to = 50, by = 0.01)  # ppb     #NOT NEEDED FOR AUTO-CALC
#nox <- (1E-9)*noxppb*M                                     #NOT NEEDED FOR AUTO-CALC
#no <- 0.25*nox             # INPUT; originally 0.25NOx     #NOT NEEDED FOR AUTO-CALC
#no2 <- 0.75*nox            # INPUT; originally 0.75NOx     #NOT NEEDED FOR AUTO-CALC

alpha <- 0.01               # branching ratio for alkyl nitrate formation


## NONLINEAR FITTING OF DATA

curve.eqn <- function(NO2,vocr,S)
  {3600*1E9*(k2+k3)*(vocr*((-(k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))+sqrt((k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))^2-4*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2)*-phox))/(2*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2))))/(k2*((1E-9)*NO2*M/3))*((1E-9)*NO2*M/3)/M/S + 30}



## ST. LOUIS #####################

#Apply to data by (1) NAA (do one at a time), (2) cluster, and (3) set of years
# NAA.list <- unique(WW.MDA8.new.NO2$NAA.broad)
# yr.list <- unique(STL.WW.MDA8.NO2$yr.bins)
cluster.list <- unique(STL.WW.MDA8.NO2$Cluster.name)


STL.Pusede.curves <- data.frame() #makes a blank dataframe I'll fill below
STL.Pusede.params <- data.frame() #makes a blank dataframe for the vocr and S values

# for(i in NAA.list)
# {
#   NAA.subset <- subset(WW.MDA8.new.NO2, NAA.broad == i) #subset data for NAA
#   group.list <- unique(NAA.subset$group)
#   
  for(j in cluster.list)
  {
    cluster.subset <- subset(STL.WW.MDA8.NO2, Cluster.name == j) #subset for group
    yr.list <- unique(cluster.subset$yr.bins)
   
    for(k in yr.list)
    {
      cluster.yr.subset <- subset(cluster.subset, yr.bins == k) #subset for year bin
      
      #run nonlinear model fit
      curve.fit <- nlsLM(MDA8 ~ 3600*1E9*(k2+k3)*
                           (vocr*((-(k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))+sqrt((k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))^2-
                                                                                              4*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2)*-phox))/
                                    (2*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2))))/(k2*((1E-9)*NO2*M/3))*((1E-9)*NO2*M/3)/M/S + 30, 
               data = cluster.yr.subset, 
               start = c(vocr=130, S=80))
      
      #apply model
      curve.points <- curve.eqn(NO2 = seq(0,26,by=0.1),
                          vocr = coef(curve.fit)["vocr"],
                          S = coef(curve.fit)["S"])
      
      #Make dataframe with results of applying model
      curve.df <- data.frame(cluster = j, yr.bins = k, NO2 = seq(0,26,by=0.1), MDA8 = curve.points)
      curve.stats.dv <- data.frame(cluster = j, yr.bins = k, vocr = coef(curve.fit)["vocr"], S = coef(curve.fit)["S"])
      
      #append to larger dataframes
      STL.Pusede.curves <- rbind(STL.Pusede.curves, curve.df)
      STL.Pusede.params <- rbind(STL.Pusede.params, curve.stats.dv)
      
    }
  }
# }


#Make Pusede-style plots

# # #Plot cluster 10W (with only four year sets) separately
# i <- "Louisville"
# j <- "Central"
# # # j <- "Outlying"

# viridis.colors <- c("#440154FF","#443A83FF","#31688EFF","#21908CFF","#35B779FF","#8FD744FF","#FDE725FF")
viridis.colors <- c("#440154FF","#414487FF","#2A788EFF","#22A884FF","#7AD151FF","#FDE725FF")
all.year.list <- c("1992-96","1997-01","2002-06","2007-11","2012-16","2017-21")

    # for (i in NAA.list)
    # {
    #   NAA.subset <- subset(WW.MDA8.new.NO2, NAA.broad == i) #subset by broad NAA
    #   NAA.model.subset <- subset(Pusede.curves, NAA.broad == i) #subset model df
    #   group.list <- unique(NAA.subset$group)

      for (j in cluster.list)
      {
        cluster.subset  <-  subset(STL.WW.MDA8.NO2, Cluster.name == j) #subset by cluster
        cluster.model.subset <- subset(STL.Pusede.curves, cluster == j)

        a <- ggplot() + geom_line(data=cluster.subset, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
          geom_point(data=cluster.subset, aes(x=NO2, y=MDA8, cluster=yr.bins, color=yr.bins, shape=day.type), size=2) +
          geom_line(data = cluster.model.subset, aes(x=NO2, y=MDA8, cluster=yr.bins, color=yr.bins), size=0.5, linetype = 2) +
          geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
          scale_y_continuous(limits = c(45,90), breaks= seq(45,90,by=5)) +
          scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
          labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
          ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (St. Louis)")) +
          scale_shape_manual(values=c(16,1)) +
          # scale_color_brewer(palette = "Spectral") +
          scale_color_manual(values = viridis.colors, breaks = all.year.list) +
          # scale_color_manual(values = c("#FDAE61", "#2B83BA")) + #use just for Louisville Central (with two year bins - second and fourth years)
          # scale_color_manual(values = c("#FDAE61", "#ABDDA4", "#2B83BA")) + #use just for Louisville Central (with three year bins - last three years)
          # scale_color_manual(values = c("#D7191C", "#FDAE61", "#2B83BA")) + #use just for Louisville Outlying (with three year bins - missing 2011-15)
          theme(axis.text = element_text(size = 14, color = "black"),
                axis.text.x = element_text(size = 14, vjust=0.5),
                axis.title = element_text(size = 16),
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                plot.title = element_text(size = 18, hjust = 0),
                legend.key = element_blank(),
                # legend.background = element_rect(color = "black"),
                legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
                panel.background = element_rect(fill = "white"),
                axis.line = element_line(color = "black"),
                panel.grid.major = element_line(color = "gray90"))

        ggsave(filename = paste0("weekday-weekend MDA8 high-O3 nodes-Pusede-style - St Louis CLUSTERS", j, ".png"), plot = a, width = 8, height =5)
      }
    # }


#Pusede-style plots formatted for the report:
    
    # for (i in NAA.list)
    # {
    #   NAA.subset <- subset(WW.MDA8.new.NO2, NAA.broad == i) #subset by broad NAA
    #   NAA.model.subset <- subset(Pusede.curves, NAA.broad == i) #subset model df
    #   group.list <- unique(NAA.subset$group)
    # 
    #   for (j in group.list)
    #   {
    #     group.subset  <-  subset(NAA.subset, group == j) #subset by group
    #     group.model.subset <- subset(NAA.model.subset, group == j)
    # 
    #     a <- ggplot(data=group.subset) + geom_line(aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1.5) +
    #       geom_point(aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins, shape=day), size=3) +
    #       geom_line(data = group.model.subset, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=0.5, linetype = 2) +
    #       geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
    #       scale_y_continuous(limits = c(45,80), breaks= seq(45,80,by=5)) + scale_x_continuous(limits = c(0,26), breaks = seq(0,25,by=5)) +
    #       ylab(NULL) + xlab(NULL) + ggtitle(NULL) +
    #       # ylab("Exceedance Probability (%)") + ggtitle(NULL) + labs(x = expression("Nonattainment Area NO"[2]*" (ppb)")) +
    #       annotate("text", x= 0.6, y=77, label=paste0(i, ": ", j), size = 6, hjust=0, vjust=0) +
    #       scale_shape_manual(values=c(16,1)) +
    #       scale_color_brewer(palette = "Spectral") +
    #       # scale_color_manual(values = c("#FDAE61", "#2B83BA")) + #use just for Louisville Central (with two year bins - second and fourth years)
    #       # scale_color_manual(values = c("#FDAE61", "#ABDDA4", "#2B83BA")) + #use just for Louisville Central (with three year bins - last three years)
    #       # scale_color_manual(values = c("#D7191C", "#FDAE61", "#2B83BA")) + #use just for Louisville Outlying (with three year bins - missing 2011-15)
    #       guides(color=FALSE, shape=FALSE) +
    #       theme(axis.text = element_text(size = 14, color = "black"),
    #             axis.text.x = element_text(size = 14, vjust=0.5),
    #             axis.title = element_text(size = 16),
    #             legend.text = element_text(size = 10),
    #             legend.title = element_text(size = 12),
    #             plot.title = element_text(size = 18, hjust = 0))
    #     ggsave(filename = paste0("weekday-weekend MDA8 highO3 nodes-Pusede-style- ", i, j, "-report-with fits-NEW NO2.png"), plot = a, width = 5, height =3.5)
    #   }
    # }


#Plot curve parameters (vocr and S)

    # for(i in NAA.list)
    # {
    #   NAA.subset <- subset(STL.Pusede.params, NAA.broad == i)

      #vocr plot
      a <- ggplot() + geom_line(data=STL.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=1) +
        geom_point(data=STL.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=2) +
        geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
        scale_y_continuous(limits = c(0,200), breaks= seq(0,200,by=50)) +
        xlab(NULL) + ylab("VOCR") +
        ggtitle(paste0("VOCR from the nonlinear model fit - \nSt. Louis")) +
        scale_color_brewer(palette = "Spectral") +
        theme(axis.text = element_text(size = 14, color = "black"),
              axis.text.x = element_text(size = 14, vjust=0.5),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 12),
              plot.title = element_text(size = 18, hjust = 0))
      ggsave(filename = paste0("weekday-weekend curve fit vocr - St Louis CLUSTERS.png"), plot = a, width = 8, height =5)

      #S plot
      a <- ggplot() + geom_line(data=STL.Pusede.params, aes(x=yr.bins, y=S, group=cluster, color=cluster), size=1) +
        geom_point(data=STL.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=2) +
        geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
        scale_y_continuous(limits = c(0,1.5), breaks= seq(0,1.5,by=0.25)) +
        xlab(NULL) + ylab("S") +
        ggtitle(paste0("S from the nonlinear model fit - \nSt. Louis")) +
        scale_color_brewer(palette = "Spectral") +
        theme(axis.text = element_text(size = 14, color = "black"),
              axis.text.x = element_text(size = 14, vjust=0.5),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 12),
              plot.title = element_text(size = 18, hjust = 0))
      ggsave(filename = paste0("weekday-weekend curve fit S - St Louis CLUSTERS.png"), plot = a, width = 8, height =5)

    # }



## CHICAGO ############################################################################

## IMPORT WEEKDAY-WEEKEND MDA8 AND NO2 VALUES FOR PLOTTING/ANALYSIS

#MDA8 data
Chicago.WW.MDA8 <- read.csv("Chicago cluster W-W diffs-means-by cluster - 1987-2021.csv", header=TRUE)

Chicago.WW.MDA8 <- Chicago.WW.MDA8 %>%
  dplyr::filter(weekend.count >= 9) %>% #remove clusters/year groups with <9 days
  dplyr::select(2:6) %>%
  pivot_longer(4:5, names_to = "day.type", values_to = "MDA8") %>%
  dplyr::mutate(day.type = ifelse(day.type == "weekday.mean", "weekday", "weekend"))

#NO2 data
NO2.data <- read.csv("Weekday-weekend NO2 concentrations - Chicago-St Louis.csv", header = TRUE)

CHI.NO2.data <- NO2.data %>%
  dplyr::filter(CBSA == "Chicago")

#Combine
Chicago.WW.MDA8.NO2 <- full_join(Chicago.WW.MDA8, CHI.NO2.data, by=c("yr.bins","day.type"))

Chicago.WW.MDA8.NO2 <- Chicago.WW.MDA8.NO2 %>%
  dplyr::select(6,1:5,7) %>%
  dplyr::rename(NO2 = area.mean) %>%
  dplyr::mutate(Cluster.name = ifelse(Cluster.name == "19*", "19", ifelse(Cluster.name == "40SE*", "40SE", 
                                      ifelse(Cluster.name == "66SE*", "66SE", ifelse(Cluster.name == "72N*", "72N", Cluster.name))))) #dropping * because the figure won't save with that in the filename

## DROP 1987-91 DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Chicago.WW.MDA8.NO2 <- dplyr::filter(Chicago.WW.MDA8.NO2, yr.bins != "1987-91")

      


## CHICAGO #####################

## !!!!!! FIRST RUN CODE ABOVE IN ST. LOUIS SECTION FOR ANALYTICAL MODEL !!!!!!!!!!!

#Apply to data by (1) NAA (do one at a time), (2) cluster, and (3) set of years
# NAA.list <- unique(WW.MDA8.new.NO2$NAA.broad)
# yr.list <- unique(STL.WW.MDA8.NO2$yr.bins)
cluster.list <- unique(Chicago.WW.MDA8.NO2$Cluster.name)


Chicago.Pusede.curves <- data.frame() #makes a blank dataframe I'll fill below
Chicago.Pusede.params <- data.frame() #makes a blank dataframe for the vocr and S values

# for(i in NAA.list)
# {
#   NAA.subset <- subset(WW.MDA8.new.NO2, NAA.broad == i) #subset data for NAA
#   group.list <- unique(NAA.subset$group)
#   
for(j in cluster.list)
{
  cluster.subset <- subset(Chicago.WW.MDA8.NO2, Cluster.name == j) #subset for group
  yr.list <- unique(cluster.subset$yr.bins)
  
  for(k in yr.list)
  {
    cluster.yr.subset <- subset(cluster.subset, yr.bins == k) #subset for year bin
    
    #run nonlinear model fit
    curve.fit <- nlsLM(MDA8 ~ 3600*1E9*(k2+k3)*
                         (vocr*((-(k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))+sqrt((k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))^2-
                                                                                            4*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2)*-phox))/
                                  (2*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2))))/(k2*((1E-9)*NO2*M/3))*((1E-9)*NO2*M/3)/M/S + 30, 
                       data = cluster.yr.subset, 
                       start = c(vocr=130, S=80))
    
    #apply model
    curve.points <- curve.eqn(NO2 = seq(0,26,by=0.1),
                              vocr = coef(curve.fit)["vocr"],
                              S = coef(curve.fit)["S"])
    
    #Make dataframe with results of applying model
    curve.df <- data.frame(cluster = j, yr.bins = k, NO2 = seq(0,26,by=0.1), MDA8 = curve.points)
    curve.stats.dv <- data.frame(cluster = j, yr.bins = k, vocr = coef(curve.fit)["vocr"], S = coef(curve.fit)["S"])
    
    #append to larger dataframes
    Chicago.Pusede.curves <- rbind(Chicago.Pusede.curves, curve.df)
    Chicago.Pusede.params <- rbind(Chicago.Pusede.params, curve.stats.dv)
    
  }
}
# }


#Make Pusede-style plots

# # #Plot cluster 10W (with only four year sets) separately
# i <- "Louisville"
# j <- "Central"
# # # j <- "Outlying"

# viridis.colors <- c("#440154FF","#443A83FF","#31688EFF","#21908CFF","#35B779FF","#8FD744FF","#FDE725FF")
viridis.colors <- c("#440154FF","#414487FF","#2A788EFF","#22A884FF","#7AD151FF","#FDE725FF")
all.year.list <- c("1992-96","1997-01","2002-06","2007-11","2012-16","2017-21")

    # for (i in NAA.list)
    # {
    #   NAA.subset <- subset(WW.MDA8.new.NO2, NAA.broad == i) #subset by broad NAA
    #   NAA.model.subset <- subset(Pusede.curves, NAA.broad == i) #subset model df
    #   group.list <- unique(NAA.subset$group)
    
    for (j in cluster.list)
    {
      cluster.subset  <-  subset(Chicago.WW.MDA8.NO2, Cluster.name == j) #subset by cluster
      cluster.model.subset <- subset(Chicago.Pusede.curves, cluster == j)
      
      a <- ggplot() + geom_line(data=cluster.subset, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
        geom_point(data=cluster.subset, aes(x=NO2, y=MDA8, cluster=yr.bins, color=yr.bins, shape=day.type), size=2) +
        geom_line(data = cluster.model.subset, aes(x=NO2, y=MDA8, cluster=yr.bins, color=yr.bins), size=0.5, linetype = 2) +
        geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
        scale_y_continuous(limits = c(45,80), breaks= seq(45,80,by=5)) +
        # scale_y_continuous(limits = c(45,94.5), breaks= seq(45,90,by=5)) +
        scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
        labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
        ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
        scale_shape_manual(values=c(16,1)) +
        # scale_color_brewer(palette = "Spectral") +
        scale_color_manual(values = viridis.colors, breaks = all.year.list) +
        # scale_color_manual(values = c("#FDAE61", "#2B83BA")) + #use just for Louisville Central (with two year bins - second and fourth years)
        # scale_color_manual(values = c("#FDAE61", "#ABDDA4", "#2B83BA")) + #use just for Louisville Central (with three year bins - last three years)
        # scale_color_manual(values = c("#D7191C", "#FDAE61", "#2B83BA")) + #use just for Louisville Outlying (with three year bins - missing 2011-15)
        theme(axis.text = element_text(size = 14, color = "black"),
              axis.text.x = element_text(size = 14, vjust=0.5),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 10),
              legend.title = element_blank(),
              plot.title = element_text(size = 18, hjust = 0),
              legend.key = element_blank(),
              # legend.background = element_rect(color = "black"),
              legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(color = "black"),
              panel.grid.major = element_line(color = "gray90"))
      
      ggsave(filename = paste0("weekday-weekend MDA8 high-O3 nodes-Pusede-style - Chicago CLUSTERS", j, ".png"), plot = a, width = 8, height =5)
      # ggsave(filename = paste0("weekday-weekend MDA8 high-O3 nodes-Pusede-style - Chicago CLUSTERS-full scale", j, ".png"), plot = a, width = 8, height =5)
    }
    # }


#Plot curve parameters (vocr and S)

# for(i in NAA.list)
# {
#   NAA.subset <- subset(STL.Pusede.params, NAA.broad == i)

#vocr plot
    a <- ggplot() + geom_line(data=Chicago.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=1) +
      geom_point(data=Chicago.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(0,200), breaks= seq(0,200,by=50)) +
      xlab(NULL) + ylab("VOCR") +
      ggtitle(paste0("VOCR from the nonlinear model fit - \nChicago")) +
      scale_color_brewer(palette = "Spectral") +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            plot.title = element_text(size = 18, hjust = 0))
    ggsave(filename = paste0("weekday-weekend curve fit vocr - Chicago CLUSTERS.png"), plot = a, width = 8, height =5)

#S plot
    a <- ggplot() + geom_line(data=Chicago.Pusede.params, aes(x=yr.bins, y=S, group=cluster, color=cluster), size=1) +
      geom_point(data=Chicago.Pusede.params, aes(x=yr.bins, y=vocr, group=cluster, color=cluster), size=2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(0,1.5), breaks= seq(0,1.5,by=0.25)) +
      xlab(NULL) + ylab("S") +
      ggtitle(paste0("S from the nonlinear model fit - \nChicago")) +
      scale_color_brewer(palette = "Spectral") +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            plot.title = element_text(size = 18, hjust = 0))
    ggsave(filename = paste0("weekday-weekend curve fit S - Chicago CLUSTERS.png"), plot = a, width = 8, height =5)

# }

    
## MAKE COMBINED ST. LOUIS-CHICAGO PLOT FOR PAPER
    
Stl.3.data <- dplyr::filter(STL.WW.MDA8.NO2, Cluster.name == "3")
Stl.3.model <- dplyr::filter(STL.Pusede.curves, cluster == "3")

    a <- ggplot() + geom_line(data=Stl.3.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Stl.3.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Stl.3.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="St. Louis: 3", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            # legend.position="none",
            legend.text = element_text(size = 14),
            legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            # legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
Stl.44NW.data <- dplyr::filter(STL.WW.MDA8.NO2, Cluster.name == "44NW")
Stl.44NW.model <- dplyr::filter(STL.Pusede.curves, cluster == "44NW")
    
    b <- ggplot() + geom_line(data=Stl.44NW.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Stl.44NW.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Stl.44NW.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="44NW", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.position="none",
            # legend.text = element_text(size = 10),
            # legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            # legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
Stl.91N.data <- dplyr::filter(STL.WW.MDA8.NO2, Cluster.name == "91N")
Stl.91N.model <- dplyr::filter(STL.Pusede.curves, cluster == "91N")
    
    c <- ggplot() + geom_line(data=Stl.91N.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Stl.91N.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Stl.91N.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="91N", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.position="none",
            # legend.text = element_text(size = 10),
            # legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            # legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
Chi.13.data <- dplyr::filter(Chicago.WW.MDA8.NO2, Cluster.name == "13")
Chi.13.model <- dplyr::filter(Chicago.Pusede.curves, cluster == "13")
    
    d <- ggplot() + geom_line(data=Chi.13.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Chi.13.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Chi.13.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="Chicago: 13", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.position="none",
            # legend.text = element_text(size = 10),
            # legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            # legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
Chi.40SW.data <- dplyr::filter(Chicago.WW.MDA8.NO2, Cluster.name == "40SW")
Chi.40SW.model <- dplyr::filter(Chicago.Pusede.curves, cluster == "40SW")
    
    e <- ggplot() + geom_line(data=Chi.40SW.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Chi.40SW.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Chi.40SW.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="40SW", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            legend.position="none",
            # legend.text = element_text(size = 10),
            # legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            # legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))
    
Chi.74SE.data <- dplyr::filter(Chicago.WW.MDA8.NO2, Cluster.name == "74SE")
Chi.74SE.model <- dplyr::filter(Chicago.Pusede.curves, cluster == "74SE")
    
    f <- ggplot() + geom_line(data=Chi.74SE.data, aes(x=NO2, y=MDA8, group=yr.bins, color=yr.bins), size=1) +
      geom_point(data=Chi.74SE.data, aes(x=NO2, y=MDA8, color=yr.bins, shape=day.type), size=2) +
      geom_line(data = Chi.74SE.model, aes(x=NO2, y=MDA8, color=yr.bins), size=0.5, linetype = 2) +
      geom_hline(yintercept=0, color="darkgray") + geom_vline(xintercept=0, color="darkgray") +
      scale_y_continuous(limits = c(45,79), breaks= seq(45,75,by=5)) +
      scale_x_continuous(limits = c(0,26), breaks= seq(0,25,by=5)) +
      xlab(NULL) + ylab(NULL) + annotate("text", x=0, y=78, label="74SE", size=6, hjust=0) +
      # labs(x = expression("Mean Area NO"[2]*" (ppb)")) + ylab("Mean MDA8 (ppb)") +
      # ggtitle(paste0("Mean MDA8 on Ozone-Conducive Days vs NO2 \n", j, " (Chicago)")) +
      scale_shape_manual(values=c(16,1)) +
      scale_color_manual(values = viridis.colors, breaks = all.year.list) +
      theme(axis.text = element_text(size = 14, color = "black"),
            axis.text.x = element_text(size = 14, vjust=0.5),
            axis.title = element_text(size = 16),
            # legend.position="none",
            legend.text = element_text(size = 14),
            legend.title = element_blank(),
            plot.title = element_text(size = 18, hjust = 0),
            legend.key = element_blank(),
            # legend.background = element_rect(color = "black"),
            # legend.margin=margin(t=-0.1, r=0.1, b=0, l=0.1, unit="cm"),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray90"))


#Extract legend
    legend <- cowplot::get_legend(a + theme(legend.box.margin = margin(0,0,0,12), return_all = TRUE))
    
#remove legends from plots
    a <- a + ggplot2::theme(legend.position="none")
    b <- b + ggplot2::theme(legend.position="none")
    c <- c + ggplot2::theme(legend.position="none")
    d <- d + ggplot2::theme(legend.position="none")
    e <- e + ggplot2::theme(legend.position="none")
    f <- f + ggplot2::theme(legend.position="none")
    
#Combine plots    
    paper.plot <- cowplot::plot_grid(a,b,c,d,e,f, nrow=2, ncol=3, labels = c("(a)","(b)","(c)","(d)","(e)","(f)"), label_x = -0.025, align = "hv",
                                     axis = "tb") 
#Add in legend
    paper.plot <- cowplot::plot_grid(paper.plot, legend, rel_widths = c(3,0.5))
    
#Define common axes and add in
    x.axis <- textGrob(expression("Mean Area NO"[2]*" (ppb)"), gp=gpar(fontsize = 16), hjust=0.75)
    y.axis <- textGrob("Mean MDA8 Value (ppb)", gp=gpar(fontsize = 16), rot = 90, hjust=0.37)
    
    paper.plot <- grid.arrange(paper.plot, left = y.axis, bottom = x.axis)
    
    # ggsave(filename = "Pusede plots - St Louis-Chicago 1987-2021.png", plot = paper.plot, width=10, height = 6)
    ggsave(filename = "Pusede plots - St Louis-Chicago 1992-2021.png", plot = paper.plot, width=10, height = 6)
    
    

    





## DRAW EXAMPLE CURVES FOR OZONE VERSUS NO2 AT THREE LEVELS OF VOCr

# curve.NO2.vocr <- function(NO2,vocr) {3600*1E9*(k2+k3)*
#                      (vocr*((-(k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))+sqrt((k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))^2-
#                                                                                         4*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2)*-phox))/
#                               (2*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2))))/(k2*((1E-9)*NO2*M/3))*((1E-9)*NO2*M/3)/M/S + 30 }

curve.eqn <- function(NO2,vocr,S)
{3600*1E9*(k2+k3)*(vocr*((-(k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))+sqrt((k1*((1E-9)*NO2*M)+(alpha*k2*vocr)/((1-alpha)*k2))^2-4*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2)*-phox))/(2*((2*ka+2*kb+2*kc)*(vocr/((1-alpha)*k2*((1E-9)*NO2*M/3)))^2))))/(k2*((1E-9)*NO2*M/3))*((1E-9)*NO2*M/3)/M/S + 30}

#apply model
curve.points.vocr.120 <- curve.eqn(NO2 = seq(0,35,by=0.1),
                          vocr = 120,
                          S = 1)
curve.vocr.120 <- data.frame(ozone = curve.points.vocr.120, NO2 = seq(0,35,by=0.1), vocr = "120")

curve.points.vocr.75 <- curve.eqn(NO2 = seq(0,35,by=0.1),
                                   vocr = 75,
                                   S = 1)
curve.vocr.75 <- data.frame(ozone = curve.points.vocr.75, NO2 = seq(0,35,by=0.1), vocr = "75")

curve.points.vocr.40 <- curve.eqn(NO2 = seq(0,35,by=0.1),
                                   vocr = 40,
                                   S = 1)
curve.vocr.40 <- data.frame(ozone = curve.points.vocr.40, NO2 = seq(0,35,by=0.1), vocr = "40")

three.curves <- bind_rows(curve.vocr.120, curve.vocr.75, curve.vocr.40)
                   
    a <- ggplot(three.curves, aes(x=NO2,y=ozone, color = vocr, group = vocr)) + geom_line(linewidth = 0.75) +
      scale_color_manual(values = c("red3","blue3","purple3")) + xlab("NOx") + ylab("Ozone") +
      annotate("text", x=26, y=54, label="high VOC", size=5, color="red3", hjust=0) +
      annotate("text", x=26, y=46.5, label="mod. VOC", size=5, color="purple3", hjust=0) +
      annotate("text", x=26, y=40, label="low VOC", size=5, color="blue3", hjust=0) +
      annotate("text", x=11, y=65, label="transitional", size=5, color="gray30") +
      annotate("text", x=28, y=60, label="VOC-sensitive\n(NOx-suppressed)", size=5, color="gray30") +
      annotate("text", x=2, y=60, label="NOx-\nsensitive", size=5, color="gray30") +
      guides(color = "none") +
      theme(axis.text = element_blank(),
            axis.title = element_text(size = 16),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(color = "black"))
    ggsave(filename = "Example ozone-NOx curves for paper-3 VOCr levels.png", plot = a, width = 5, height = 3)
    


    
 
    
    