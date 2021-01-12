
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(rattle)

crimes <- read.csv("crimes_tidy.csv")

# Principal Component Analysis
crimes.quant <- select(crimes, Latitude, longitude)
pr.out <- prcomp(crimes.quant, scale=TRUE)

#Multiple Correspondence Analysis
crimes.cat <- select(crimes, year_rept:dow1, DIST:downtown)



# Hierarchical Clustering of reportedas data using Euclidean distance
set.seed(1)

# Summary of data so that we have quantitative variables
test <- crimes %>%
  select(Latitude:longitude, year_rept:minute_occur, reportedas) %>%
  group_by(reportedas) %>%
  summarise(counts = n(), mean_lat = mean(Latitude), sd_lat = sd(Latitude),
            mean_long = mean(longitude), sd_long = sd(longitude),
            mean_hr = mean(hour_occur), sd_hr = sd(hour_occur)) %>% 
  filter(counts > 150 & reportedas != "")

# Making the row names "reported as"
row.names(test) <- test$reportedas
test <- select(test, counts:sd_hr)

# Clustering for scaled average, single, complete
xsc <- scale(test)
hc.average.xsc <- hclust(dist(xsc), method="average")
plot(hc.average.xsc, main="Average Linkage for Scaled Variables",
     xlab="", sub="", cex=0.7)

xsc <- scale(test)
hc.single.xsc <- hclust(dist(xsc), method="single")
plot(hc.single.xsc, main="Single Linkage for Scaled Variables",
     xlab="", sub="", cex=0.7)

xsc <- scale(test)
hc.complete.xsc <- hclust(dist(xsc), method="complete")
plot(hc.complete.xsc, main="Complete Linkage for Scaled Variables",
     xlab="", sub="", cex=0.7)

xsc <- scale(test)
hc.complete.xsc <- hclust(dist(xsc), method="complete")
plot(hc.complete.xsc, main="Complete Linkage for Scaled Variables",
     xlab="", sub="", cex=0.7)

hc.groups <- cutree(hc.complete.xsc, 3)
rect.hclust(hc.complete.xsc, 3, border="red")

# K-Means Clustering of reportedas data, then comparing two clustering methods
set.seed(1)

# K-means
km.out <- kmeans(xsc, 3, nstart=20)

# Making K-mean cluster names same as hierarchical cluster names
for(i in 1:length(km.out$cluster)){
  if(km.out$cluster[i] == 1){
    km.out$cluster[i] <-  3
  }
  else if(km.out$cluster[i] == 2){
    km.out$cluster[i] <-  1
  }
  else if(km.out$cluster[i] == 3){
    km.out$cluster[i] <-  2
  }
}

# Percentage of observations that are classified into same groups: hierarchical vs. k-means

t.f <- km.out$cluster == hc.groups

count <- 0
for(i in 1:length(t.f)){
  if(t.f[i] == TRUE){
    count <- count + 1
  }
}

# Percentage value
count / length(t.f)

# K-Means Clustering of charged as data
set.seed(1)

# Summary of data so that we have quantitative variables
test2 <- crimes %>%
  select(Latitude:longitude, year_rept:minute_occur, chrgdesc) %>%
  group_by(chrgdesc) %>%
  summarise(mean_lat = mean(Latitude), mean_long = mean(longitude),
            mean_hour = mean(hour_occur), counts = n())

test2$chrgdesc <- c("ASSAULT", "DISORDERLY CONDUCT", "DRUGS/ALCOHOL", "FRAUD",
                    "LARCENY", "MISSING PERSON", "OTHER/WARNING",
                    "PROPERTY OFFENSES", "VEHICLE CRIME")

# K-means clustering
km.out <- kmeans(test2[,c("mean_lat", "mean_long")], 3, nstart=20)
o <- order(km.out$cluster)
plot(test2$mean_lat, test2$mean_long, type="n", xlab="Latitude", ylab="Longitude", 
     main="K-Means Clustering of 'charged as'")
text(test2$mean_lat, test2$mean_long, labels=test2$chrgdesc, col=km.out$cluster+1, cex=0.7)


# Hierarchical Clustering of charged as data
set.seed(1)

# Making row names "charged as"
row.names(test2) <- test2$chrgdesc
test2 <- select(test2, mean_lat, mean_long)

# Hierarchical Clustering
xsc <- scale(test2)
hc.complete.xsc <- hclust(dist(xsc), method="complete")
plot(hc.complete.xsc, main="Complete Linkage for Scaled Variables",
     xlab="", sub="", cex=0.7)
hc.groups <- cutree(hc.complete.xsc, 3)
rect.hclust(hc.complete.xsc, 3, border="red")
o <- order(hc.groups)

# Making plots
test2 <- crimes %>%
  select(Latitude:longitude, year_rept:minute_occur, chrgdesc) %>%
  group_by(chrgdesc) %>%
  summarise(mean_lat = mean(Latitude), mean_long = mean(longitude))
test2$chrgdesc <- c("ASSAULT", "DISORDERLY CONDUCT", "DRUGS/ALCOHOL", "FRAUD",
                    "LARCENY", "MISSING PERSON", "OTHER/WARNING",
                    "PROPERTY OFFENSES", "VEHICLE CRIME")

plot(test2$mean_lat, test2$mean_long, type="n", xlab="Latitude", ylab="Longitude",
     main="Hierarchical Clustering of 'charged as'")
text(test2$mean_lat, test2$mean_long, labels=test2$chrgdesc, col=hc.groups+1, cex=0.7)
# Create vector with crime names
