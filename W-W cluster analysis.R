################################################################################
#                                                                              #
#               CLUSTER ANALYSIS for WEEKDAY-WEEKEND ANALYSIS                  #
#          2002-2020 (BECAUSE OF METDAT AVAILABILITY) - FOR PUBLICATION        #
#                                                                              #
#                       CLUSTER ANALYSIS OF W-W RESULTS                        #
#                                                                              #
################################################################################


require("factoextra")
require("tidyverse")
require("gridExtra")

#FIRST TRY: INPUT VARIABLES: MEAN WEEKDAY MDA8 VALUES FOR EACH YEAR SET & WEEKDAY-WEEKEND MDA8 DIFFERENCES FOR EACH YEAR SET (no location information)

WW.data <- read.csv("Weekday-weekend results.csv", header=TRUE)

#select and prep just weekday MDA8s and W-W diffs. Shorten labels
wdMDA8s.WWdiffs <- WW.data %>%
  dplyr::select(21,20,1:4,5,9) %>%
  dplyr::rename(wd.MDA8 = weekday.mean, W.W = wd.we.diff) %>%
  dplyr::mutate(yr.bins = ifelse(yr.bins=="2002-06","02.06", ifelse(yr.bins=="2007-11","07.11", ifelse(yr.bins=="2012-16","12.16","17.20"))))

#Chicago

Chicago <- dplyr::filter(wdMDA8s.WWdiffs, city == "Chicago")

#Scales data (using z-scores)
# Chicago.scaled <- dplyr::mutate_at(Chicago, c(7,8), funs(c(scale(.)))) #don't use dplyr because you lose the attributes so can't convert back to concentration units
Chicago.scaled <- Chicago
Chicago.scaled$wd.MDA8 <- scale(Chicago.scaled$wd.MDA8, center = TRUE, scale = TRUE)
Chicago.scaled$W.W <- scale(Chicago.scaled$W.W, center = TRUE, scale = TRUE)
Chicago.scaled.wide <- pivot_wider(Chicago.scaled, names_from = yr.bins, values_from = c(wd.MDA8, W.W))
Chicago.scaled.wide <- dplyr::select(Chicago.scaled.wide, 4:13)
Chicago.scaled.wide <- na.omit(Chicago.scaled.wide)
Chicago.for.kmeans <- dplyr::select(Chicago.scaled.wide, 3:10) #removes data labels for clustering
# Chicago.for.kmeans <- na.omit(Chicago.for.kmeans) #remove sites with NAs for clustering
# Chicago.for.kmeans <- dplyr::ungroup(Chicago.for.kmeans)

#check optimal number of clusters in the data 
    # png("Chicago kmeans cluster tests-elbow method.png", width = 500, height = 500)
    # factoextra::fviz_nbclust(Chicago.for.kmeans, kmeans, method = "wss", k.max=18) #using the elbow method
    # dev.off()
    # 
    # png("Chicago kmeans cluster tests-silhouette method.png", width = 500, height = 500)
    # factoextra::fviz_nbclust(Chicago.for.kmeans, kmeans, method = "silhouette", k.max=18) #using the silhouette method (high is better)
    # dev.off()


#Compare PCA plots with different numbers of clusters (from 5 to 9)
set.seed(10)
km.Chicago.5 <- kmeans(Chicago.for.kmeans, 5, nstart=25)
km.Chicago.6 <- kmeans(Chicago.for.kmeans, 6, nstart=25)
km.Chicago.7 <- kmeans(Chicago.for.kmeans, 7, nstart=25)
km.Chicago.8 <- kmeans(Chicago.for.kmeans, 8, nstart=25)
km.Chicago.9 <- kmeans(Chicago.for.kmeans, 9, nstart=25)
# print(km.Chicago)

    plot5 <- fviz_cluster(km.Chicago.5, data=Chicago.for.kmeans, geom="point")
    plot6 <- fviz_cluster(km.Chicago.6, data=Chicago.for.kmeans, geom="point")
    plot7 <- fviz_cluster(km.Chicago.7, data=Chicago.for.kmeans, geom="point")
    plot8 <- fviz_cluster(km.Chicago.8, data=Chicago.for.kmeans, geom="point")
    plot9 <- fviz_cluster(km.Chicago.9, data=Chicago.for.kmeans, geom="point")

    # png("Chicago kmeans clusters compare 5-9 clusters.png", width = 900, height = 900)
    # grid.arrange(plot5,plot6,plot7,plot8,plot9)
    # dev.off()

#Compute mean of variables by cluster
cluster.means.scaled.5 <- aggregate(Chicago.for.kmeans, by=list(cluster=km.Chicago.5$cluster), mean)
cluster.means.scaled.6 <- aggregate(Chicago.for.kmeans, by=list(cluster=km.Chicago.6$cluster), mean)
cluster.means.scaled.7 <- aggregate(Chicago.for.kmeans, by=list(cluster=km.Chicago.7$cluster), mean)
cluster.means.scaled.8 <- aggregate(Chicago.for.kmeans, by=list(cluster=km.Chicago.8$cluster), mean)
cluster.means.scaled.9 <- aggregate(Chicago.for.kmeans, by=list(cluster=km.Chicago.9$cluster), mean)

#Determine the within-cluster sum of squares
cluster.means.scaled.5 <- cluster.means.scaled.5 %>%
  dplyr::mutate(withinss = km.Chicago.5$withinss, n.clusters = "5 clusters")
cluster.means.scaled.6 <- cluster.means.scaled.6 %>%
  dplyr::mutate(withinss = km.Chicago.6$withinss, n.clusters = "6 clusters")
cluster.means.scaled.7 <- cluster.means.scaled.7 %>%
  dplyr::mutate(withinss = km.Chicago.7$withinss, n.clusters = "7 clusters")
cluster.means.scaled.8 <- cluster.means.scaled.8 %>%
  dplyr::mutate(withinss = km.Chicago.8$withinss, n.clusters = "8 clusters")
cluster.means.scaled.9 <- cluster.means.scaled.9 %>%
  dplyr::mutate(withinss = km.Chicago.9$withinss, n.clusters = "9 clusters")
  
Chicago.cluster.means <- rbind(cluster.means.scaled.5, cluster.means.scaled.6, cluster.means.scaled.7, cluster.means.scaled.8, cluster.means.scaled.9)
Chicago.cluster.means.long <- pivot_longer(Chicago.cluster.means, wd.MDA8_02.06:W.W_17.20, names_to = c(".value","years"), names_pattern = "(.*)_(.*)")
#Convert back to units of ppb from scaled values
Chicago.cluster.means.long <- Chicago.cluster.means.long %>%
  dplyr::mutate(wd.MDA8.orig = Chicago.cluster.means.long$wd.MDA8 * attr(Chicago.scaled$wd.MDA8, "scaled:scale") + attr(Chicago.scaled$wd.MDA8, "scaled:center"),
                W.W.orig = Chicago.cluster.means.long$W.W * attr(Chicago.scaled$W.W, "scaled:scale") + attr(Chicago.scaled$W.W, "scaled:center"))

#Add cluster labels on to original data frame with site names/IDs
Chicago.kmeans <- cbind(Chicago.scaled.wide, cluster.5 = km.Chicago.5$cluster, cluster.6 = km.Chicago.6$cluster, cluster.7 = km.Chicago.7$cluster,
                        cluster.8 = km.Chicago.8$cluster, cluster.9 = km.Chicago.9$cluster)
Chicago.kmeans.9.labels <- dplyr::select(Chicago.kmeans, 1,15) #just selects cluster labels

#Export to use to group monitors
# write.csv(Chicago.kmeans.9.labels, "Chicago cluster labels - 9 clusters.csv", row.names = FALSE)


#Make plots comparing cluster means to individual monitors in cluster - for mean MDA8 & W-W differences
#Do for 9-cluster analysis
Chicago.monitor.kmeans.9 <- left_join(Chicago, Chicago.kmeans.9.labels, by="Site") #add cluster labels onto monitor means
Chicago.monitor.kmeans.9 <- dplyr::rename(Chicago.monitor.kmeans.9, cluster = cluster.9)

Chicago.cluster.means.9 <- Chicago.cluster.means.long %>%
  dplyr::filter(n.clusters == "9 clusters") %>%
  dplyr::mutate(Site.Name.new = "cluster")

# #MDA8 plot
#     a <- ggplot(Chicago.monitor.kmeans.9, aes(x=yr.bins, y=wd.MDA8, color=Site.Name.new, group=Site.Name.new)) + geom_line() + facet_wrap(~cluster) +
#       xlab("Years") + ylab("Mean Weekday MDA8 Ozone (ppb)") + ggtitle("Chicago Monitor Mean MDA8 by Cluster (9 Clusters)") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title = element_text(size = 16),
#             # legend.text = element_text(size = 12),
#             # legend.title = element_blank(),
#             plot.title = element_text(size = 19, hjust = 0),
#             strip.text = element_text(size=16))
#     ggsave(filename = "Chicago monitor mean MDA8 by cluster.png", plot = a, width = 8, height =6)
#     # a <- a + geom_line(data = Chicago.cluster.means.9, aes(x=years, y=wd.MDA8.orig)) + facet_wrap(~cluster)
#     
# #Weekday-Weekend plot
#     a <- ggplot(Chicago.monitor.kmeans.9, aes(x=yr.bins, y=W.W, color=Site.Name.new, group=Site.Name.new)) + geom_line() + facet_wrap(~cluster) +
#       xlab("Years") + ylab("Weekday-Weekend MDA8 Difference (ppb)") + ggtitle("Chicago Weekday-Weekend Diff. by Cluster (9 Clusters)") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title = element_text(size = 16),
#             # legend.text = element_text(size = 12),
#             # legend.title = element_blank(),
#             plot.title = element_text(size = 19, hjust = 0),
#             strip.text = element_text(size=16))
#     ggsave(filename = "Chicago monitor mean weekday-weekend diff by cluster.png", plot = a, width = 8, height =6)




## ST. LOUIS #######################################################################

St.Louis <- dplyr::filter(wdMDA8s.WWdiffs, city == "St. Louis")

#Scales data (using z-scores)
# St.Louis.scaled <- dplyr::mutate_at(St.Louis, c(7,8), funs(c(scale(.)))) #don't use dplyr because you lose the attributes so can't convert back to concentration units
St.Louis.scaled <- St.Louis
St.Louis.scaled$wd.MDA8 <- scale(St.Louis.scaled$wd.MDA8, center = TRUE, scale = TRUE)
St.Louis.scaled$W.W <- scale(St.Louis.scaled$W.W, center = TRUE, scale = TRUE)
St.Louis.scaled.wide <- pivot_wider(St.Louis.scaled, names_from = yr.bins, values_from = c(wd.MDA8, W.W))
St.Louis.scaled.wide <- dplyr::select(St.Louis.scaled.wide, 4:13)
St.Louis.scaled.wide <- na.omit(St.Louis.scaled.wide)
St.Louis.for.kmeans <- dplyr::select(St.Louis.scaled.wide, 3:10) #removes data labels for clustering
# St.Louis.for.kmeans <- na.omit(St.Louis.for.kmeans) #remove sites with NAs for clustering
# St.Louis.for.kmeans <- dplyr::ungroup(St.Louis.for.kmeans)

#check optimal number of clusters in the data 
    # png("St Louis kmeans cluster tests-elbow method.png", width = 500, height = 500)
    # factoextra::fviz_nbclust(St.Louis.for.kmeans, kmeans, method = "wss", k.max=12) #using the elbow method
    # dev.off()
    # 
    # png("St Louis kmeans cluster tests-silhouette method.png", width = 500, height = 500)
    # factoextra::fviz_nbclust(St.Louis.for.kmeans, kmeans, method = "silhouette", k.max=12) #using the silhouette method (high is better)
    # dev.off()

#Compare PCA plots with different numbers of clusters (from 2 to 7)
set.seed(10)
# km.St.Louis.2 <- kmeans(St.Louis.for.kmeans, 2, nstart=5)
# km.St.Louis.3 <- kmeans(St.Louis.for.kmeans, 3, nstart=5)
km.St.Louis.4 <- kmeans(St.Louis.for.kmeans, 4, nstart=5)
km.St.Louis.5 <- kmeans(St.Louis.for.kmeans, 5, nstart=5)
km.St.Louis.6 <- kmeans(St.Louis.for.kmeans, 6, nstart=5)
km.St.Louis.7 <- kmeans(St.Louis.for.kmeans, 7, nstart=5)
km.St.Louis.8 <- kmeans(St.Louis.for.kmeans, 8, nstart=5)
km.St.Louis.9 <- kmeans(St.Louis.for.kmeans, 9, nstart=5)
# print(km.St.Louis)

# plot2 <- fviz_cluster(km.St.Louis.2, data=St.Louis.for.kmeans, geom="point")
# plot3 <- fviz_cluster(km.St.Louis.3, data=St.Louis.for.kmeans, geom="point")
plot4 <- fviz_cluster(km.St.Louis.4, data=St.Louis.for.kmeans, geom="point")
plot5 <- fviz_cluster(km.St.Louis.5, data=St.Louis.for.kmeans, geom="point")
plot6 <- fviz_cluster(km.St.Louis.6, data=St.Louis.for.kmeans, geom="point")
plot7 <- fviz_cluster(km.St.Louis.7, data=St.Louis.for.kmeans, geom="point")
plot8 <- fviz_cluster(km.St.Louis.8, data=St.Louis.for.kmeans, geom="point")
plot9 <- fviz_cluster(km.St.Louis.9, data=St.Louis.for.kmeans, geom="point")

# png("St Louis kmeans clusters compare 2-7 clusters.png", width = 900, height = 900)
# grid.arrange(plot2,plot3,plot4,plot5,plot6,plot7)
# dev.off()

# png("St Louis kmeans clusters compare 4-9 clusters.png", width = 900, height = 900)
# grid.arrange(plot4,plot5,plot6,plot7,plot8,plot9)
# dev.off()


#Compute mean of variables by cluster
# cluster.means.scaled.2 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.2$cluster), mean)
# cluster.means.scaled.3 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.3$cluster), mean)
cluster.means.scaled.4 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.4$cluster), mean)
cluster.means.scaled.5 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.5$cluster), mean)
cluster.means.scaled.6 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.6$cluster), mean)
cluster.means.scaled.7 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.7$cluster), mean)
cluster.means.scaled.8 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.8$cluster), mean)
cluster.means.scaled.9 <- aggregate(St.Louis.for.kmeans, by=list(cluster=km.St.Louis.9$cluster), mean)

#Determine the within-cluster sum of squares
# cluster.means.scaled.2 <- cluster.means.scaled.2 %>%
#   dplyr::mutate(withinss = km.St.Louis.2$withinss, n.clusters = "2 clusters")
# cluster.means.scaled.3 <- cluster.means.scaled.3 %>%
#   dplyr::mutate(withinss = km.St.Louis.3$withinss, n.clusters = "3 clusters")
cluster.means.scaled.4 <- cluster.means.scaled.4 %>%
  dplyr::mutate(withinss = km.St.Louis.4$withinss, n.clusters = "4 clusters")
cluster.means.scaled.5 <- cluster.means.scaled.5 %>%
  dplyr::mutate(withinss = km.St.Louis.5$withinss, n.clusters = "5 clusters")
cluster.means.scaled.6 <- cluster.means.scaled.6 %>%
  dplyr::mutate(withinss = km.St.Louis.6$withinss, n.clusters = "6 clusters")
cluster.means.scaled.7 <- cluster.means.scaled.7 %>%
  dplyr::mutate(withinss = km.St.Louis.7$withinss, n.clusters = "7 clusters")
cluster.means.scaled.8 <- cluster.means.scaled.8 %>%
  dplyr::mutate(withinss = km.St.Louis.8$withinss, n.clusters = "8 clusters")
cluster.means.scaled.9 <- cluster.means.scaled.9 %>%
  dplyr::mutate(withinss = km.St.Louis.9$withinss, n.clusters = "9 clusters")

St.Louis.cluster.means <- rbind(cluster.means.scaled.4, cluster.means.scaled.5, cluster.means.scaled.6, cluster.means.scaled.7,
                                cluster.means.scaled.8, cluster.means.scaled.9)
St.Louis.cluster.means.long <- pivot_longer(St.Louis.cluster.means, wd.MDA8_02.06:W.W_17.20, names_to = c(".value","years"), names_pattern = "(.*)_(.*)")
#Convert back to units of ppb from scaled values
St.Louis.cluster.means.long <- St.Louis.cluster.means.long %>%
  dplyr::mutate(wd.MDA8.orig = St.Louis.cluster.means.long$wd.MDA8 * attr(St.Louis.scaled$wd.MDA8, "scaled:scale") + attr(St.Louis.scaled$wd.MDA8, "scaled:center"),
                W.W.orig = St.Louis.cluster.means.long$W.W * attr(St.Louis.scaled$W.W, "scaled:scale") + attr(St.Louis.scaled$W.W, "scaled:center"))

#Add cluster labels on to original data frame with site names/IDs
St.Louis.kmeans <- cbind(St.Louis.scaled.wide, cluster.4 = km.St.Louis.4$cluster, cluster.5 = km.St.Louis.5$cluster, cluster.6 = km.St.Louis.6$cluster, 
                         cluster.7 = km.St.Louis.7$cluster, cluster.8 = km.St.Louis.8$cluster, cluster.9 = km.St.Louis.9$cluster)

# #Including 8 clusters
# St.Louis.cluster.means <- rbind(cluster.means.scaled.2, cluster.means.scaled.3, cluster.means.scaled.4, cluster.means.scaled.5, cluster.means.scaled.6, 
#                                  cluster.means.scaled.7, cluster.means.scaled.8)
# St.Louis.cluster.means.long <- pivot_longer(St.Louis.cluster.means, wd.MDA8_02.06:W.W_17.20, names_to = c(".value","years"), names_pattern = "(.*)_(.*)")
# #Convert back to units of ppb from scaled values
# St.Louis.cluster.means.long <- St.Louis.cluster.means.long %>%
#   dplyr::mutate(wd.MDA8.orig = St.Louis.cluster.means.long$wd.MDA8 * attr(St.Louis.scaled$wd.MDA8, "scaled:scale") + attr(St.Louis.scaled$wd.MDA8, "scaled:center"),
#                 W.W.orig = St.Louis.cluster.means.long$W.W * attr(St.Louis.scaled$W.W, "scaled:scale") + attr(St.Louis.scaled$W.W, "scaled:center"))
# 
# #Add cluster labels on to original data frame with site names/IDs
# St.Louis.kmeans <- cbind(St.Louis.scaled.wide, cluster.2 = km.St.Louis.2$cluster, cluster.3 = km.St.Louis.3$cluster, cluster.4 = km.St.Louis.4$cluster, 
#                           cluster.5 = km.St.Louis.5$cluster, cluster.6 = km.St.Louis.6$cluster, cluster.7 = km.St.Louis.7$cluster, cluster.8 = km.St.Louis.8$cluster)

# #Including 9 clusters
# St.Louis.cluster.means <- rbind(cluster.means.scaled.2, cluster.means.scaled.3, cluster.means.scaled.4, cluster.means.scaled.5, cluster.means.scaled.6,
#                                  cluster.means.scaled.7, cluster.means.scaled.8, cluster.means.scaled.9)
# St.Louis.cluster.means.long <- pivot_longer(St.Louis.cluster.means, wd.MDA8_02.06:W.W_17.20, names_to = c(".value","years"), names_pattern = "(.*)_(.*)")
# #Convert back to units of ppb from scaled values
# St.Louis.cluster.means.long <- St.Louis.cluster.means.long %>%
#   dplyr::mutate(wd.MDA8.orig = St.Louis.cluster.means.long$wd.MDA8 * attr(St.Louis.scaled$wd.MDA8, "scaled:scale") + attr(St.Louis.scaled$wd.MDA8, "scaled:center"),
#                 W.W.orig = St.Louis.cluster.means.long$W.W * attr(St.Louis.scaled$W.W, "scaled:scale") + attr(St.Louis.scaled$W.W, "scaled:center"))
# 
# #Add cluster labels on to original data frame with site names/IDs
# St.Louis.kmeans <- cbind(St.Louis.scaled.wide, cluster.2 = km.St.Louis.2$cluster, cluster.3 = km.St.Louis.3$cluster, cluster.4 = km.St.Louis.4$cluster,
#                           cluster.5 = km.St.Louis.5$cluster, cluster.6 = km.St.Louis.6$cluster, cluster.7 = km.St.Louis.7$cluster, cluster.8 = km.St.Louis.8$cluster,
#                           cluster.9 = km.St.Louis.9$cluster)



St.Louis.kmeans.8.labels <- dplyr::select(St.Louis.kmeans, 1,15) #just selects cluster labels

#Export to use to group monitors
write.csv(St.Louis.kmeans.8.labels, "St Louis cluster labels - 8 clusters.csv", row.names = FALSE)


#Make plots comparing cluster means to individual monitors in cluster - for mean MDA8 & W-W differences
#Do for 8-cluster analysis
St.Louis.monitor.kmeans.8 <- left_join(St.Louis, St.Louis.kmeans.8.labels, by="Site") #add cluster labels onto monitor means
St.Louis.monitor.kmeans.8 <- dplyr::rename(St.Louis.monitor.kmeans.8, cluster = cluster.8)

St.Louis.cluster.means.8 <- St.Louis.cluster.means.long %>%
  dplyr::filter(n.clusters == "8 clusters") %>%
  dplyr::mutate(Site.Name.new = "cluster")

#MDA8 plot
#     a <- ggplot(St.Louis.monitor.kmeans.8, aes(x=yr.bins, y=wd.MDA8, color=Site.Name.new, group=Site.Name.new)) + geom_line() + facet_wrap(~cluster) +
#       xlab("Years") + ylab("Mean Weekday MDA8 Ozone (ppb)") + ggtitle("St. Louis Monitor Mean MDA8 by Cluster (8 Clusters)") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title = element_text(size = 16),
#             # legend.text = element_text(size = 12),
#             # legend.title = element_blank(),
#             plot.title = element_text(size = 19, hjust = 0),
#             strip.text = element_text(size=16))
#     ggsave(filename = "St Louis monitor mean MDA8 by cluster.png", plot = a, width = 8, height =6)
#     # a <- a + geom_line(data = St.Louis.cluster.means.7, aes(x=years, y=wd.MDA8.orig)) + facet_wrap(~cluster)
# 
# #Weekday-Weekend plot
#     a <- ggplot(St.Louis.monitor.kmeans.8, aes(x=yr.bins, y=W.W, color=Site.Name.new, group=Site.Name.new)) + geom_line() + facet_wrap(~cluster) +
#       xlab("Years") + ylab("Weekday-Weekend MDA8 Difference (ppb)") + ggtitle("St. Louis Weekday-Weekend Diff. by Cluster (8 Clusters)") +
#       theme(axis.text = element_text(size = 14, color = "black"),
#             axis.text.x = element_text(size = 14, angle=90, vjust=0.5),
#             axis.title = element_text(size = 16),
#             # legend.text = element_text(size = 12),
#             # legend.title = element_blank(),
#             plot.title = element_text(size = 19, hjust = 0),
#             strip.text = element_text(size=16))
#     ggsave(filename = "St Louis monitor mean weekday-weekend diff by cluster.png", plot = a, width = 8, height =6)




