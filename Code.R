library("sf")
library("sp")
library("rgdal")
library("rgeos")
library("dplyr")
library(ggplot2)
library("lattice")

cat("\014") 
rm(list = ls())
graphics.off()

social <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean.csv", header = TRUE)
social  <- na.omit(social) 
social$ymd <- paste(social$year, social$month, social$day, sep="-")
social$wday <- weekdays(as.Date(social$ymd))
social$ymd <- NULL
#write.csv (social, "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean_Francisco.csv", row.names = FALSE)

### Buffers
cat("\014") 
rm(list = ls())
graphics.off()

s <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean_Francisco.csv", header = TRUE)
s <- mutate(s, id = rownames(s))
s$wday <- ordered(s$wday , levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                    "Friday", "Saturday", "Sunday"))


shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\buffer_de_identified_places25m.shp"
x25 = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
x25_1 <- subset(x25, x25$OID==1)
x25_2 <- subset(x25, x25$OID==2)
x25_3 <- subset(x25, x25$OID==3)
x25_4 <- subset(x25, x25$OID==4)
x25_5 <- subset(x25, x25$OID==5)
x25_6 <- subset(x25, x25$OID==6)
x25_7 <- subset(x25, x25$OID==7)

p <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean_Francisco.csv", header = TRUE)
coordinates(p)<-~longitude+latitude
proj4string(p)<-CRS("+proj=longlat +datum=NAD83")
p <- spTransform(p, CRS(proj4string(x25)))
##p1
res <- over(p,x25_1)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res1 <-res
res1 <- merge(x=res1, y=s, by="id", all.x=T)
##p2
res <- over(p,x25_2)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res2 <-res
res2 <- merge(x=res2, y=s, by="id", all.x=T)
##p3
res <- over(p,x25_3)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res3 <-res
res3 <- merge(x=res3, y=s, by="id", all.x=T)
##p4
res <- over(p,x25_4)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res4 <-res
res4 <- merge(x=res4, y=s, by="id", all.x=T)
##p5
res <- over(p,x25_5)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res5 <-res
res5 <- merge(x=res5, y=s, by="id", all.x=T)
##p6
res <- over(p,x25_6)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res6 <-res
res6 <- merge(x=res6, y=s, by="id", all.x=T)
##p7
res <- over(p,x25_7)
res <- mutate(res, id = rownames(res))
res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
res <- na.omit(res) 
res7 <-res
res7 <- merge(x=res7, y=s, by="id", all.x=T)
### Lucky Plaza
shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\buffer_de_Lucky_plaza150m.shp"
x = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
resl <- over(p,x)
resl <- mutate(resl, id = rownames(resl))
resl <- resl[,c("id","DISTDESDE","DISTHASTA","OID","id")]
resl <- na.omit(resl) 
resl <- merge(x=resl, y=s, by="id", all.x=T)
resl$OID <- 8

res <- rbind(res1,res2,res3,res4,res5,res6,res7,resl)
res <- subset(res, (res$hour>=16 & res$hour<=20 & 
                      (res$wday=="Friday" | res$wday=="Saturday" | res$wday=="Sunday") ))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Results\\Figures")
png("hotspots_25mr.png", width = 10, height =10, units = 'in', res = 300)
a <- ggplot(data=res, aes(wday))+
     geom_bar(aes(fill=as.factor(hour)), position="fill") + 
     facet_grid(rows = vars(as.factor(OID))) +
     labs(x="Day", y="% of Tweets",
       fill="Day of the week", title="Places 1-7 = Tweets in hotspots (25m radius)",
       subtitle="Place 8 = Lucky Plaza 150m radius") + 
     theme_bw(base_size = 20)+ theme(legend.position="bottom")
a
print(a)
dev.off()
graphics.off()

##########################Grid Orchard
shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Grid_150x150.shp"
g = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
g$id <- as.numeric( as.character(g$id) )
g_o <- subset(g, ( (g$id >= 44230 & g$id <= (44230+13) |
               (g$id >= 44588 & g$id <= (44588+13)) |
               (g$id >= 44946 & g$id <= (44946+13)) |
               (g$id >= 45304 & g$id <= (45304+13)) |
               (g$id >= 45662 & g$id <= (45662+13)) |
               (g$id >= 46020 & g$id <= (46020+13)) |
               (g$id >= 46378 & g$id <= (46378+13)) |
               (g$id >= 46736 & g$id <= (46736+13)) ) ) ) 
plot(g_o)

p <- spTransform(p, CRS(proj4string(g)))
resg <- over(p,g_o)
resg <- mutate(resg, id_tweets = rownames(resg))
resg <- resg[,c("id","xmin","xmax","ymin","ymax","id_tweets")]
resg <- na.omit(resg) 
s$id_tweets <- s$id
resg <- merge(x=resg, y=s, by="id_tweets", all.x=T)
resg <- subset(resg, (resg$hour>=16 & resg$hour<=20 & 
                        (resg$wday=="Friday" | resg$wday=="Saturday" | resg$wday=="Sunday") ))
resg$ones <- 1
resg <- resg[,c("id.x","ones")]
resg <-  aggregate(resg, by = list(resg$id.x), FUN = sum)
resg$id.x <- NULL
names(resg) <- c("id","ones")
g_o <- merge(x=g_o, y=resg, by="id", all.x=TRUE)
g_o$ones[is.na(g_o$ones)] <- 0
g_o_df <- as.data.frame(g_o)

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes")
writeOGR(g_o, dsn = '.', layer = 'Orchard_grid', driver = "ESRI Shapefile")


##########################Grid Clementi
shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Grid_150x150.shp"
g = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
g$id <- as.numeric( as.character(g$id) )
g_c <- subset(g, ( (g$id >= 42035 & g$id <= (42035+3) |
                      (g$id >= 42393 & g$id <= (42393+3)) |
                      (g$id >= 42751 & g$id <= (42751+3)) |
                      (g$id >= 43109 & g$id <= (43109+3)) ) ) ) 
plot(g_c)

resc <- over(p,g_c)
resc <- mutate(resc, id_tweets = rownames(resc))
resc <- resc[,c("id","xmin","xmax","ymin","ymax","id_tweets")]
resc <- na.omit(resc) 
#s$id_tweets <- s$id
resc <- merge(x=resc, y=s, by="id_tweets", all.x=T)
resc <- subset(resc, (resc$hour>=16 & resc$hour<=20 & 
                        (resc$wday=="Friday" | resc$wday=="Saturday" | resc$wday=="Sunday") ))
resc$ones <- 1
resc <- resc[,c("id.x","ones")]
resc <-  aggregate(resc, by = list(resc$id.x), FUN = sum)
resc$id.x <- NULL
names(resc) <- c("id","ones")
g_c <- merge(x=g_c, y=resc, by="id", all.x=TRUE)
g_c$ones[is.na(g_c$ones)] <- 0
g_c_df <- as.data.frame(g_c)

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes")
writeOGR(g_c, dsn = '.', layer = 'Clementi', driver = "ESRI Shapefile")


###################### Bars from grids
shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Orchard.shp"
g = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
g_df <- as.data.frame(g)
g_df$Place <- ifelse( (g_df$id>=45308 & g_df$id<=45311), "Lucky Plaza", "Other")

shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Clementi.shp"
h = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
h_df <- as.data.frame(h)
h_df$Place <- ifelse( (h_df$id==42394 ), "Clementi", "Other")

grid <- rbind(g_df, h_df)
grid$grid_id <- rownames(grid)

g_df$grid_id <- rownames(g_df)
h_df$grid_id <- rownames(h_df)

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Results\\Figures")
png("No_Tweets_Orchard_grid.png", width = 8, height =6, units = 'in', res = 300)
a <- ggplot(g_df, aes(x=grid_id, y=ones)) + geom_point(aes(colour = factor(Place))) +
      labs(x="Grid id", y="# of Tweets",
        colour="Place", title="Number of Tweets in Orchard") + 
      theme_bw(base_size = 22)
print(a)
dev.off()
graphics.off()

png("No_Tweets_Clementi_grid.png", width = 8, height =6, units = 'in', res = 300)
a <- ggplot(h_df, aes(x=grid_id, y=ones)) + geom_point(aes(colour = factor(Place))) +
      labs(x="Grid id", y="# of Tweets",
        colour="Place", title="Number of Tweets in Clementi") + 
      theme_bw(base_size = 22)
print(a)
dev.off()
graphics.off()





#############################Twitts in Orchard block analysis
cat("\014") 
rm(list = ls())
graphics.off()


s <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean_Francisco.csv", header = TRUE)
s <- mutate(s, id = rownames(s))
s$wday <- ordered(s$wday , levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                      "Friday", "Saturday", "Sunday"))
s <- subset(s, (s$hour>=16 & s$hour<=20 & 
                    (s$wday=="Friday" | s$wday=="Saturday" | s$wday=="Sunday") ))

p <- s
coordinates(p)<-~longitude+latitude
proj4string(p)<-CRS("+proj=longlat +datum=NAD83")


shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Orchard_areas_of_interest.shp"
g = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
p <- spTransform(p, CRS(proj4string(g)))

res <- over(p,g)
res <- mutate(res, id = rownames(res))
#res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
#res <- na.omit(res) 
res <- merge(x=res, y=s, by="id", all.x=T)
res <- subset(res, !is.na(res$XMIN))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Results\\Figures")
png("Tweets_Orchard_paper.png", width = 8, height =6, units = 'in', res = 300)
a <- ggplot(data=res, aes(wday))+
  geom_bar(aes(fill=as.factor(hour)), position="fill") + 
  labs(x="Day", y="% of Tweets",
       fill="Hour of the day", subtitle="Total of 1,410 tweets") + 
  theme_bw(base_size = 20)+ theme(legend.position="bottom")
print(a)
dev.off()
graphics.off()

#############################Twitts in Clementiblock analysis
cat("\014") 
rm(list = ls())
graphics.off()


s <- read.csv("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Datasets\\Twitter\\social_clean_Francisco.csv", header = TRUE)
s <- mutate(s, id = rownames(s))
s$wday <- ordered(s$wday , levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                      "Friday", "Saturday", "Sunday"))
s <- subset(s, (s$hour>=16 & s$hour<=20 & 
                  (s$wday=="Friday" | s$wday=="Saturday" | s$wday=="Sunday") ))

p <- s
coordinates(p)<-~longitude+latitude
proj4string(p)<-CRS("+proj=longlat +datum=NAD83")


shp = "C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Shapes\\Clementi_area_of_interest.shp"
g = readOGR(shp, layer = basename(strsplit(shp, "\\.")[[1]])[1]) 
p <- spTransform(p, CRS(proj4string(g)))

res <- over(p,g)
res <- mutate(res, id = rownames(res))
#res <- res[,c("id","DISTDESDE","DISTHASTA","OID","id")]
#res <- na.omit(res) 
res <- merge(x=res, y=s, by="id", all.x=T)
res <- subset(res, !is.na(res$XMIN))

setwd("C:\\Users\\franc\\Dropbox\\Francisco\\Papers 2018\\Rafa\\Results\\Figures")
png("Tweets_Clementi_paper.png", width = 8, height =6, units = 'in', res = 300)
a <- ggplot(data=res, aes(wday))+
  geom_bar(aes(fill=as.factor(hour)), position="fill") + 
  labs(x="Day", y="% of Tweets",
       fill="Hour of the day", subtitle="Total of 153 tweets") + 
  theme_bw(base_size = 20)+ theme(legend.position="bottom")
print(a)
dev.off()
graphics.off()
