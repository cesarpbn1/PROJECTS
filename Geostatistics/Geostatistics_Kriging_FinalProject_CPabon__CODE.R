setwd("~/Desktop/Hunter_College_Docs/FALL2019/STAT787/PROJECT/FINAL")


#########  HUNTER COLLEGE: DEPARTMENT OF MATHEMATICS & STATISTICS 
#########  STAT 787 
#########  FALL 2019       
#########  CESAR R. PABON BERNAL
########   
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(zoo) 
library(chron) 
library(plyr)
library(xts)
library(lattice)
library(rugarch)
library(readxl)
library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(data.table)
library(maps)
library(scatterplot3d)
library(geoXY)
library(usmap)
library(plotly)
library(concaveman)
library(sf)
library(spatstat)
library(maptools)
library(sp)
library(rgdal)
library(officer)
library(rvg)
library(smoothr)
library(USAboundaries) # STATES/counties data
library(Imap)
library(maps)
library(tools)
library(geosphere)
library(lattice)
library(sp)
library(gstat)
library(phylin)

####### 1. LOAD FILE
######
df = read_csv("torn2.csv");head(df)
df2= copy(df)


df2$year = as.integer(df2$yr)
df2= df2[which(df2$year > 2006), ];df2
df2= df2[which(df2$mag > 2), ];df2
df3 = df2 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];df2
#df3 = df3[ !(df3$slat == 35.9342),]
st_all= count(df3$st)
st_all= st_all[order(-st_all$freq),];st_all

######## a. Get some outputs about # of tornadoes 
df2_EF1= df[which(df$mag == 1), ]
count_df2_EF1= count(df2_EF1$mag)
print("Number of Tornadoes EF 1 in USA");print(count_df2_EF1$freq)
df2_EF2= df[which(df$mag == 2), ]
count_df2_EF2= count(df2_EF2$mag)
print("Number of Tornadoes EF 2 in USA");print(count_df2_EF2$freq)
df2_EF3= df2[which(df2$mag == 3), ]
count_df2_EF3= count(df2_EF3$mag)
print("Number of Tornadoes EF 3 in USA");print(count_df2_EF3$freq)
df2_EF4= df2[which(df2$mag == 4), ]
count_df2_EF4= count(df2_EF4$mag)
print("Number of Tornadoes EF 4 in USA");print(count_df2_EF4$freq)
df2_EF5= df2[which(df2$mag == 5), ]
count_df2_EF5= count(df2_EF5$mag)
print("Number of Tornadoes EF 5 in USA");print(count_df2_EF5$freq)


########### 2. MAP OF USA W/ TORNADOES EF> 3
###########
usa = map_data("usa")
stat_all_plot = usa  %>%
        group_by(group) %>% 
        plot_ly(x = ~long, y =~lat, title="Tornadoes") %>%
        add_polygons(
                fillcolor = 'white', opacity =0.4,
                line = list(color ='black', width = 0.9),
                showlegend = FALSE, hoverinfo ="none") %>%
        layout(title = 'Tornadoes in the USA 2007-2018') %>%
        add_markers(
                x = df2_EF3$slon, y = df2_EF3$slat, showlegend= TRUE , marker = list(color = 'green', 
                                                                               symbol = 'circle'), name = 'EF 3')%>%
        add_markers(
                x = df2_EF4$slon, y = df2_EF4$slat, showlegend= TRUE , marker = list(color = 'red', 
                                                                                   symbol = 'cross'),name = 'EF 4')%>%
        add_markers(
                x = df2_EF5$slon, y = df2_EF5$slat, showlegend= TRUE , marker = list(color = 'blue', 
                                                                                   symbol = 'square'), name = 'EF 5')
stat_all_plot # nice plot with all points





# SECOND MAP
library(ggplot2)
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

boundary <- st_read("1950-2018-torn-aspath.shp")
st_geometry_type(boundary)
st_bbox(boundary)
boundary

df_2= boundary[which(boundary$yr > 2006), ];df_2
df_2= df_2[which(df_2$mag > 2), ];dim(df2)
df_3 = df_2 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(df_3)
df_4 = df_3 [, -c(1,3)]

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

EF3= df_3[df_3$mag == 3,]
EF4= df_3[df_3$mag == 4,]
EF5= df_3[df_3$mag == 5,]

(sites <- data.frame(longitude = c(EF3$slon), latitude = c(EF3$slat)))
(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

(sites2 <- data.frame(longitude = c(EF4$slon), latitude = c(EF4$slat)))
(sites2 <- st_as_sf(sites2, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

(sites3 <- data.frame(longitude = c(EF5$slon), latitude = c(EF5$slat)))
(sites3 <- st_as_sf(sites3, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


(sites4 <- st_as_sf(df_4, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
coordinates(states)
states2 <- cbind(states, st_coordinates(st_centroid(states)))
states2$ID <- toTitleCase(states2$ID)
head(states2)

us_tor_plot = ggplot(data = world) +
        geom_sf() +
        geom_sf(data = sites, size = 2, shape = 23, fill = "red", show.legend = "line") +
        geom_sf(data = sites2, size = 2, shape = 23, fill = "green",show.legend = "line") +
        geom_sf(data = sites3, size = 2, shape = 23, fill = "blue",show.legend = "line") +  geom_sf(data = states2, fill = NA) +
        coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE)
     
#annotate_figure(us_tor_plot,
#                top = text_grob("Tornadoes in the USA  w/ EF>2 (2007-2018)", color = "red", face = "bold", size = 14))


        
        








########### 3. POLYGON MAP  W/ TORNADOES EF> 3
###########
boundary <- st_read("1950-2018-torn-aspath.shp")
st_geometry_type(boundary)
st_bbox(boundary)
boundary


########## A. CLEAN SHAPEFILE
df_2= boundary[which(boundary$yr > 2006), ];df_2
df_2= df_2[which(df_2$mag > 2), ];dim(df2)
df_3 = df_2 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(df_3)
df_3 = df_3 [df_3$slon > -105,];dim(df_3)
df_4 = df_3 [, -c(1,3)]


######### B. CREATE POLYGON & POINTS & PLOT OF POLYGON
poly = concaveman(df_3)
pt_df_3   = st_cast(df_4, "POINT")
(sites_pt = st_as_sf(pt_df_3, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


EF_all_plot = sites_pt%>%
        ggplot() + 
        geom_sf(data = poly,col= "red") +
        geom_sf(aes(col = sites_pt$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("Tornadoes in USA w/ EF rating > 2")






#2007
EF_total= aggregate(df_2$mag, by=list(yr=df_2$yr), FUN=sum);EF_total
EF_2007= df_2[which(df_2$yr == 2007), ];dim(EF_2007)
EF_2007 = EF_2007 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(EF_2007)
EF_2007 = EF_2007 [EF_2007$slon > -105,];dim(EF_2007)
EF_2007 = EF_2007 [, -c(1,3)]
count(EF_2007$mag=="3")
count(EF_2007$mag=="4")
count(EF_2007$mag=="5")


poly_EF_2007 = concaveman(EF_2007)
pt_df_EF_2007  = st_cast(EF_2007, "POINT")
(sites_pt_EF_2007 = st_as_sf(pt_df_EF_2007, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2007_plot = sites_pt_EF_2007%>%
        ggplot() + 
        geom_sf(data = poly_EF_2007,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2007$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2007: EF3=27, EF4=4, EF5=1")


#2008
EF_2008= df_2[which(df_2$yr == 2008), ];
EF_2008 = EF_2008 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2008 = EF_2008 [EF_2008$slon > -105,]
EF_2008 = EF_2008 [, -c(1,3)]
count(EF_2008$mag=="3")
count(EF_2008$mag=="4")
count(EF_2008$mag=="5")

poly_EF_2008 = concaveman(EF_2008)
pt_df_EF_2008  = st_cast(EF_2008, "POINT")
(sites_pt_EF_2008 = st_as_sf(pt_df_EF_2008, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2008_plot = sites_pt_EF_2008%>%
        ggplot() + 
        geom_sf(data = poly_EF_2008,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2008$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2008:EF3=49, EF4=9, EF5=1")

#2009
EF_2009= df_2[which(df_2$yr == 2009), ];
EF_2009 = EF_2009 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2009 = EF_2009 [EF_2009$slon > -105,]
EF_2009 = EF_2009 [, -c(1,3)]
count(EF_2009$mag=="3")
count(EF_2009$mag=="4")
count(EF_2009$mag=="5")

poly_EF_2009 = concaveman(EF_2009)
pt_df_EF_2009  = st_cast(EF_2009, "POINT")
(sites_pt_EF_2009 = st_as_sf(pt_df_EF_2009, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2009_plot = sites_pt_EF_2009%>%
        ggplot() + 
        geom_sf(data = poly_EF_2009,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2009$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2009:EF3=20, EF4=2, EF5=0")

#2010
EF_2010= df_2[which(df_2$yr == 2010), ];
EF_2010 = EF_2010 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2010 = EF_2010 [EF_2010$slon > -105,]
EF_2010 = EF_2010 [, -c(1,3)]
count(EF_2010$mag=="3")
count(EF_2010$mag=="4")
count(EF_2010$mag=="5")

poly_EF_2010 = concaveman(EF_2010)
pt_df_EF_2010  = st_cast(EF_2010, "POINT")
(sites_pt_EF_2010 = st_as_sf(pt_df_EF_2010, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2010_plot = sites_pt_EF_2010%>%
        ggplot() + 
        geom_sf(data = poly_EF_2010,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2010$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2010:EF3=30, EF4=13, EF5=0")

#2011
EF_2011= df_2[which(df_2$yr == 2011), ];
EF_2011 = EF_2011 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2011 = EF_2011 [EF_2011$slon > -105,]
EF_2011 = EF_2011 [, -c(1,3)]
count(EF_2011$mag=="3")
count(EF_2011$mag=="4")
count(EF_2011$mag=="5")

poly_EF_2011 = concaveman(EF_2011)
pt_df_EF_2011  = st_cast(EF_2011, "POINT")
(sites_pt_EF_2011 = st_as_sf(pt_df_EF_2011, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2011_plot = sites_pt_EF_2011%>%
        ggplot() + 
        geom_sf(data = poly_EF_2011,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2011$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2011:EF3=61, EF4=17, EF5=6")

#2012
EF_2012= df_2[which(df_2$yr == 2012), ];
EF_2012 = EF_2012 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2012 = EF_2012 [EF_2012$slon > -105,]
EF_2012 = EF_2012 [, -c(1,3)]
count(EF_2012$mag=="3")
count(EF_2012$mag=="4")
count(EF_2012$mag=="5")

poly_EF_2012 = concaveman(EF_2012)
pt_df_EF_2012  = st_cast(EF_2012, "POINT")
(sites_pt_EF_2012 = st_as_sf(pt_df_EF_2012, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2012_plot = sites_pt_EF_2012%>%
        ggplot() + 
        geom_sf(data = poly_EF_2012,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2012$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2012:EF3=24, EF4=4, EF5=0")

#2013
EF_2013= df_2[which(df_2$yr == 2013), ];
EF_2013 = EF_2013 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2013 = EF_2013 [EF_2013$slon > -105,]
EF_2013 = EF_2013 [, -c(1,3)]
count(EF_2013$mag=="3")
count(EF_2013$mag=="4")
count(EF_2013$mag=="5")

poly_EF_2013 = concaveman(EF_2013)
pt_df_EF_2013  = st_cast(EF_2013, "POINT")
(sites_pt_EF_2013 = st_as_sf(pt_df_EF_2013, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2013_plot = sites_pt_EF_2013%>%
        ggplot() + 
        geom_sf(data = poly_EF_2013,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2013$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2013:EF3=19, EF4=8, EF5=1")

#2014
EF_2014= df_2[which(df_2$yr == 2014), ];
EF_2014 = EF_2014 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2014 = EF_2014 [EF_2014$slon > -105,]
EF_2014 = EF_2014 [, -c(1,3)]
count(EF_2014$mag=="3")
count(EF_2014$mag=="4")
count(EF_2014$mag=="5")

poly_EF_2014 = concaveman(EF_2014)
pt_df_EF_2014  = st_cast(EF_2014, "POINT")
(sites_pt_EF_2014 = st_as_sf(pt_df_EF_2014, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2014_plot = sites_pt_EF_2014%>%
        ggplot() + 
        geom_sf(data = poly_EF_2014,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2014$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2014:EF3=20, EF4=7, EF5=0")

#2015
EF_2015= df_2[which(df_2$yr == 2015), ];
EF_2015 = EF_2015 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2015 = EF_2015 [EF_2015$slon > -105,]
EF_2015 = EF_2015 [, -c(1,3)]
count(EF_2015$mag=="3")
count(EF_2015$mag=="4")
count(EF_2015$mag=="5")

poly_EF_2015 = concaveman(EF_2015)
pt_df_EF_2015  = st_cast(EF_2015, "POINT")
(sites_pt_EF_2015 = st_as_sf(pt_df_EF_2015, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2015_plot = sites_pt_EF_2015%>%
        ggplot() + 
        geom_sf(data = poly_EF_2015,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2015$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2015:EF3=17, EF4=3, EF5=20")


#2016
EF_2016= df_2[which(df_2$yr == 2016), ];
EF_2016 = EF_2016 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2016 = EF_2016 [EF_2016$slon > -105,]
EF_2016 = EF_2016 [, -c(1,3)]
count(EF_2016$mag=="3")
count(EF_2016$mag=="4")
count(EF_2016$mag=="5")

poly_EF_2016 = concaveman(EF_2016)
pt_df_EF_2016  = st_cast(EF_2016, "POINT")
(sites_pt_EF_2016 = st_as_sf(pt_df_EF_2016, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2016_plot = sites_pt_EF_2016%>%
        ggplot() + 
        geom_sf(data = poly_EF_2016,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2016$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2016:EF3=26, EF4=2, EF5=0")

#2017
EF_2017= df_2[which(df_2$yr == 2017), ];
EF_2017 = EF_2017 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2017 = EF_2017 [EF_2017$slon > -105,]
EF_2017 = EF_2017 [, -c(1,3)]
count(EF_2017$mag=="3")
count(EF_2017$mag=="4")
count(EF_2017$mag=="5")

poly_EF_2017 = concaveman(EF_2017)
pt_df_EF_2017  = st_cast(EF_2017, "POINT")
(sites_pt_EF_2017 = st_as_sf(pt_df_EF_2017, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2017_plot = sites_pt_EF_2017%>%
        ggplot() + 
        geom_sf(data = poly_EF_2017,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2017$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2017:EF3=13, EF4=2, EF5=0")

#2017
EF_2018= df_2[which(df_2$yr == 2018), ];
EF_2018 = EF_2018 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
EF_2018 = EF_2018 [EF_2018$slon > -105,]
EF_2018 = EF_2018 [, -c(1,3)]
count(EF_2018$mag=="3")
count(EF_2018$mag=="4")
count(EF_2018$mag=="5")

poly_EF_2018 = concaveman(EF_2018)
pt_df_EF_2018  = st_cast(EF_2018, "POINT")
(sites_pt_EF_2018 = st_as_sf(pt_df_EF_2018, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))

EF_2018_plot = sites_pt_EF_2018%>%
        ggplot() + 
        geom_sf(data = poly_EF_2018,col= "red") +
        geom_sf(aes(col = sites_pt_EF_2018$mag))+
        scale_color_continuous(name = 'Magnitude')+
        ggtitle("2018:EF3=17, EF4=0, EF5=0")


# PLOTS
year_EF_plot = ggarrange(EF_2007_plot, 
                             EF_2008_plot,
                             EF_2009_plot,
                             EF_2010_plot,
                             EF_2011_plot,
                             EF_2012_plot,
                             EF_2013_plot,
                             EF_2014_plot,
                             EF_2015_plot,
                             EF_2016_plot,
                             EF_2017_plot,
                             EF_2018_plot,
                             labels = c("A", "B", "C", "D", "E", "F", "G","H","I","J","K","L"),
                             ncol = 3, nrow = 4)
#annotate_figure(year_EF_plot,
 #               top = text_grob("Tornados in the USA  w/ EF>2 (2007-2018)", color = "blue", face = "bold", size = 14))


year_EF_plot2 = ggarrange(EF_2007_plot, 
                         EF_2008_plot,
                         EF_2009_plot,
                         EF_2010_plot,
                         EF_2011_plot,
                         EF_2012_plot,
                         EF_2013_plot,
                         EF_2014_plot,
                         EF_2015_plot,
                         EF_2016_plot,
                         EF_2017_plot,
                         EF_2018_plot,
                         ncol = 3, nrow = 4)
#annotate_figure(year_EF_plot2,
#                top = text_grob("Tornados in the USA  w/ EF>2 (2007-2018)", color = "blue", face = "bold", size = 14))



##################################################################################################################
##################################################################################################################
##################################################################################################################
########### 3. POLYGON MAP  W/ FATALATIES PER YEAR 2006-20018
###### fatalities
#2007
deaths= aggregate(df_2$fat, by=list(yr=df_2$yr), FUN=sum);deaths
DEATH_2007 = df_2[which(df_2$yr == 2007), ];dim(DEATH_2007)
DEATH_2007= DEATH_2007 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(DEATH_2007)
DEATH_2007 = DEATH_2007 [DEATH_2007$slon > -105& DEATH_2007$slat>20,];dim(DEATH_2007)
DEATH_2007= DEATH_2007 [, -c(1,2)]

poly_2007 = concaveman(DEATH_2007)
pt_df_2007   = st_cast(DEATH_2007, "POINT")
(sites_pt_2007 = st_as_sf(pt_df_2007, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2007_plot= sites_pt_2007%>%
        ggplot() + 
        geom_sf(data = poly_2007)+
        geom_sf(aes(col = sites_pt_2007$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2007:32")

DEATH_2007_plot
#2008
deaths= aggregate(df_2$fat, by=list(yr=df_2$yr), FUN=sum);deaths
DEATH_2008 = df_2[which(df_2$yr == 2008), ];dim(DEATH_2008)
DEATH_2008= DEATH_2008 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(DEATH_2008)
DEATH_2008 = DEATH_2008 [DEATH_2008$slon > -105& DEATH_2008$slat>20,];dim(DEATH_2008)
DEATH_2008= DEATH_2008 [, -c(1,2)]



poly_2008 = concaveman(DEATH_2008)
pt_df_2008   = st_cast(DEATH_2008, "POINT")
(sites_pt_2008 = st_as_sf(pt_df_2008, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2008_plot=sites_pt_2008%>%
        ggplot() + 
        geom_sf(data = poly_2008)+
        geom_sf(aes(col = sites_pt_2008$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2008:59")
#2009
deaths= aggregate(df_2$fat, by=list(yr=df_2$yr), FUN=sum);deaths
DEATH_2009 = df_2[which(df_2$yr == 2009), ];dim(DEATH_2009)
DEATH_2009= DEATH_2009 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)];dim(DEATH_2009)
DEATH_2009 = DEATH_2009 [DEATH_2009$slon > -105& DEATH_2009$slat>20,];dim(DEATH_2009)
DEATH_2009= DEATH_2009 [, -c(1,2)]


poly_2009 = concaveman(DEATH_2009)
pt_df_2009   = st_cast(DEATH_2009, "POINT")
(sites_pt_2009 = st_as_sf(pt_df_2009, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2009_plot=sites_pt_2009%>%
        ggplot() + 
        geom_sf(data = poly_2009)+
        geom_sf(aes(col = sites_pt_2009$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2009:22")

#2010
DEATH_2010 = df_2[which(df_2$yr == 2010), ]
DEATH_2010= DEATH_2010 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2010 = DEATH_2010 [DEATH_2010$slon > -105& DEATH_2010$slat>20,]
DEATH_2010= DEATH_2010 [, -c(1,2)]


poly_2010 = concaveman(DEATH_2010)
pt_df_2010   = st_cast(DEATH_2010, "POINT")
(sites_pt_2010 = st_as_sf(pt_df_2010, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2010_plot=sites_pt_2010%>%
        ggplot() + 
        geom_sf(data = poly_2010)+
        geom_sf(aes(col = sites_pt_2010$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2010:43")
#2011
DEATH_2011 = df_2[which(df_2$yr == 2011), ]
DEATH_2011= DEATH_2011 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2011 = DEATH_2011 [DEATH_2011$slon > -105& DEATH_2011$slat>20,]
DEATH_2011= DEATH_2011 [, -c(1,2)]


poly_2011 = concaveman(DEATH_2011)
pt_df_2011   = st_cast(DEATH_2011, "POINT")
(sites_pt_2011 = st_as_sf(pt_df_2011, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2011_plot=sites_pt_2011%>%
        ggplot() + 
        geom_sf(data = poly_2011)+
        geom_sf(aes(col = sites_pt_2011$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2011:84")

#2012
DEATH_2012 = df_2[which(df_2$yr == 2012), ]
DEATH_2012= DEATH_2012 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2012 = DEATH_2012 [DEATH_2012$slon > -105& DEATH_2012$slat>20,]
DEATH_2012= DEATH_2012 [, -c(1,2)]


poly_2012 = concaveman(DEATH_2012)
pt_df_2012   = st_cast(DEATH_2012, "POINT")
(sites_pt_2012 = st_as_sf(pt_df_2012, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2012_plot=sites_pt_2012%>%
        ggplot() + 
        geom_sf(data = poly_2012)+
        geom_sf(aes(col = sites_pt_2012$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2012:28")
#2013
DEATH_2013 = df_2[which(df_2$yr == 2013), ]
DEATH_2013= DEATH_2013 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2013 = DEATH_2013 [DEATH_2013$slon > -105& DEATH_2013$slat>20,]
DEATH_2013= DEATH_2013 [, -c(1,2)]


poly_2013 = concaveman(DEATH_2013)
pt_df_2013   = st_cast(DEATH_2013, "POINT")
(sites_pt_2013 = st_as_sf(pt_df_2013, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2013_plot=sites_pt_2013%>%
        ggplot() + 
        geom_sf(data = poly_2013)+
        geom_sf(aes(col = sites_pt_2013$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2013:28")
#2014
DEATH_2014 = df_2[which(df_2$yr == 2014), ]
DEATH_2014= DEATH_2014 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2014 = DEATH_2014 [DEATH_2014$slon > -105& DEATH_2014$slat>20,]
DEATH_2014= DEATH_2014 [, -c(1,2)]


poly_2014 = concaveman(DEATH_2014)
pt_df_2014   = st_cast(DEATH_2014, "POINT")
(sites_pt_2014 = st_as_sf(pt_df_2014, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2014_plot=sites_pt_2014%>%
        ggplot() + 
        geom_sf(data = poly_2014)+
        geom_sf(aes(col = sites_pt_2014$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2014:27")

#2015
DEATH_2015 = df_2[which(df_2$yr == 2015), ]
DEATH_2015= DEATH_2015 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2015 = DEATH_2015 [DEATH_2015$slon > -105& DEATH_2015$slat>20,]
DEATH_2015= DEATH_2015 [, -c(1,2)]


poly_2015 = concaveman(DEATH_2015)
pt_df_2015   = st_cast(DEATH_2015, "POINT")
(sites_pt_2015 = st_as_sf(pt_df_2015, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2015_plot=sites_pt_2015%>%
        ggplot() + 
        geom_sf(data = poly_2015)+
        geom_sf(aes(col = sites_pt_2015$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2015:20")

#2016
DEATH_2016 = df_2[which(df_2$yr == 2016), ]
DEATH_2016= DEATH_2016 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2016 = DEATH_2016 [DEATH_2016$slon > -105& DEATH_2016$slat>20,]
DEATH_2016= DEATH_2016 [, -c(1,2)]


poly_2016 = concaveman(DEATH_2016)
pt_df_2016   = st_cast(DEATH_2016, "POINT")
(sites_pt_2016 = st_as_sf(pt_df_2016, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2016_plot=sites_pt_2016%>%
        ggplot() + 
        geom_sf(data = poly_2016)+
        geom_sf(aes(col = sites_pt_2016$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange", name = 'Fatalities')+
        ggtitle("2016:28")

#2017
DEATH_2017 = df_2[which(df_2$yr == 2017), ]
DEATH_2017= DEATH_2017 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2017 = DEATH_2017 [DEATH_2017$slon > -105& DEATH_2017$slat>20,]
DEATH_2017= DEATH_2017 [, -c(1,2)]

poly_2017 = concaveman(DEATH_2017)
pt_df_2017   = st_cast(DEATH_2017, "POINT")
(sites_pt_2017 = st_as_sf(pt_df_2017, coords = c("longitude", "latitude",name = 'Fatalities'), crs = 4326, agr = "constant"))


DEATH_2017_plot=sites_pt_2017%>%
        ggplot() + 
        geom_sf(data = poly_2017)+
        geom_sf(aes(col = sites_pt_2017$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2017:15")


#2018
DEATH_2018 = df_2[which(df_2$yr == 2018), ]
DEATH_2018= DEATH_2018 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2018 = DEATH_2018 [DEATH_2018$slon > -105& DEATH_2018$slat>20,]
DEATH_2018= DEATH_2018 [, -c(1,2)]


poly_2018 = concaveman(DEATH_2018)
pt_df_2018   = st_cast(DEATH_2018, "POINT")
(sites_pt_2018 = st_as_sf(pt_df_2018, coords = c("longitude", "latitude"), crs = 4326, agr = "constant"))


DEATH_2018_plot= sites_pt_2018%>%
        ggplot() + 
        geom_sf(data = poly_2018)+
        geom_sf(aes(col = sites_pt_2018$fat))+
        scale_color_continuous(name = 'Fatalities')+
        scale_colour_gradient(low = "red", high = "orange",name = 'Fatalities')+
        ggtitle("2018:9")

year_deaths_plot = ggarrange(DEATH_2007_plot, 
          DEATH_2008_plot,
          DEATH_2009_plot,
          DEATH_2010_plot,
          DEATH_2011_plot,
          DEATH_2012_plot,
          DEATH_2013_plot,
          DEATH_2014_plot,
          DEATH_2015_plot,
          DEATH_2016_plot,
          DEATH_2017_plot,
          labels = c("A", "B", "C", "D", "E", "F", "G","H","I","J","K"),
          ncol = 3, nrow = 4)
#annotate_figure(year_deaths_plot,
 #               top = text_grob("Tornado Fatalities in the USA--Year:Fatalities", color = "red", face = "bold", size = 14))


year_deaths_plot2 = ggarrange(DEATH_2007_plot, 
                             DEATH_2008_plot,
                             DEATH_2009_plot,
                             DEATH_2010_plot,
                             DEATH_2011_plot,
                             DEATH_2012_plot,
                             DEATH_2013_plot,
                             DEATH_2014_plot,
                             DEATH_2015_plot,
                             DEATH_2016_plot,
                             DEATH_2017_plot,
                             ncol = 3, nrow = 4)


#year_deaths_plot2
#annotate_figure(year_deaths_plot2,
#                top = text_grob("Tornado Fatalities in the USA--Year:Fatalities", color = "red", face = "bold", size = 14))

#ggsave("map.pdf")
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
########### 5. POLYGON MAP  W/ FATALATIES PER YEAR 2006-20018
##########     MODEL SELECTION: 2008, 2011, 2014, 2017


year_deaths_plot_4 = ggarrange(DEATH_2007_plot, 
                             DEATH_2008_plot,
                             DEATH_2010_plot,
                             DEATH_2011_plot,
                             labels = c("A", "B", "C", "D"),
                             ncol = 2, nrow = 2)

annotate_figure(year_deaths_plot_4,
                top = text_grob("Tornado Fatalities in the USA--Year:Fatalities", color = "red", face = "bold", size = 14))


year_mag_plot_5 = ggarrange(EF_2008_plot, 
                            EF_2011_plot,
                            EF_2014_plot,
                            EF_2017_plot,
                               labels = c("A", "B", "C", "D"),
                               ncol = 2, nrow = 2)
year_mag_plot_5


#annotate_figure(year_mag_plot_5,
#                top = text_grob("Tornados in the USA  w/ EF>2 (2007-2018)", color = "red", face = "bold", size = 14))


########### 5. A. 2011 DEATHS
########## 
DEATH_2011 = df_2[which(df_2$yr == 2011), ]
DEATH_2011= DEATH_2011 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2011 = DEATH_2011 [DEATH_2011$slon > -105& DEATH_2011$slat>20,]
DEATH_2011= DEATH_2011 [, -c(1,2)]

library(maps)
xmon_fat2011=DEATH_2011$slon
ymon_fat2011=DEATH_2011$slat

#plot(xmon_fat2011,ymon_fat2011,type="n",xlab="Longitude W", ylab="Latitude N",main="2011 fatalities EF>2 ")
#text(xmon_fat2011,ymon_fat2011,1:11,col="red",cex=1.5)
#map("state", add=TRUE)

#onverts lat-long coordinates into carthesian coordinates 

convert = function(lat, lon) {
        lat = lat/180*pi
        lon = lon/180*pi
        m.lat = (max(lat)+min(lat))/2
        m.lon = (max(lon)+min(lon))/2
        x = cos(m.lat)*sin(m.lon-lon)
        y = sin(lat-m.lat)
        cbind(x,y)
}


o.cart_fat2011 = convert(xmon_fat2011,ymon_fat2011);o.cart_fat2011
plot(o.cart_fat2011)



my.dist=function(x,y)
{
        n=length(x) # should be equal to length(y)
        dist=matrix(nrow=n,ncol=n)
        
        for(i in 1:n)
        {
                x1=x[i];y1=y[i]
                for(j in 1:i) # matrix is symmetric
                {
                        x2=x[j];y2=y[j] 
                        
                        dist[i,j]=dist[j,i]=sqrt((x1-x2)^2+(y1-y2)^2)
                        
                        #cat("distance between location",i,"and location",j,"is",dist[i,j],fill=T)
                        
                }}
        drop(dist)
} 

distances_fat2011=my.dist(o.cart_fat2011[,1],o.cart_fat2011[,2]);distances_fat2011

###########   B. 2007 DEATHS
########## 


DEATH_2007 = df_2[which(df_2$yr == 2007), ]
DEATH_2007= DEATH_2007 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2007 = DEATH_2007 [DEATH_2007$slon > -105& DEATH_2007$slat>20,]
DEATH_2007= DEATH_2007 [, -c(1,2)]

xmon_fat2007=DEATH_2007$slon
ymon_fat2007=DEATH_2007$slat

par(mfrow=c(1,1))
plot(xmon_fat2008,ymon_fat2008,col="blue", pch=20, cex=2,xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
text(xmon_fat2008,ymon_fat2008,1:11,col="red",cex=.5, pch=20)
map("state", add=TRUE)



### this add the labels to the points using values in dist

text(dist~speed, labels=dist,data=cars, cex=0.9, font=2)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2007 = convert(xmon_fat2007,ymon_fat2007)
#plot(o.cart_fat2008)

distances_fat2007=my.dist(o.cart_fat2007[,1],o.cart_fat2007[,2])


###########   c. 2008 DEATHS
########## 

DEATH_2008 = df_2[which(df_2$yr == 2008), ]
DEATH_2008= DEATH_2008 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2008 = DEATH_2008 [DEATH_2008$slon > -105& DEATH_2008$slat>20,]
DEATH_2008= DEATH_2008 [, -c(1,2)]

xmon_fat2008=DEATH_2008$slon
ymon_fat2008=DEATH_2008$slat

#plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
#text(xmon_fat2008,ymon_fat2008,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2008 = convert(xmon_fat2008,ymon_fat2008)
#plot(o.cart_fat2008)

distances_fat2008=my.dist(o.cart_fat2008[,1],o.cart_fat2008[,2])

###########   c. 2009 DEATHS
########## 

DEATH_2009 = df_2[which(df_2$yr == 2009), ]
DEATH_2009= DEATH_2009 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2009 = DEATH_2009 [DEATH_2009$slon > -105& DEATH_2009$slat>20,]
DEATH_2009= DEATH_2009 [, -c(1,2)]

xmon_fat2009=DEATH_2009$slon
ymon_fat2009=DEATH_2009$slat

#plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
#text(xmon_fat2008,ymon_fat2008,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2009 = convert(xmon_fat2009,ymon_fat2009)
#plot(o.cart_fat2008)

distances_fat2009=my.dist(o.cart_fat2009[,1],o.cart_fat2009[,2])

###########   c. 2010 DEATHS
########## 

DEATH_2010 = df_2[which(df_2$yr == 2010), ]
DEATH_2010= DEATH_2010 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2010 = DEATH_2010 [DEATH_2010$slon > -105& DEATH_2010$slat>20,]
DEATH_2010= DEATH_2010 [, -c(1,2)]

xmon_fat2010=DEATH_2010$slon
ymon_fat2010=DEATH_2010$slat

#plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
#text(xmon_fat2008,ymon_fat2008,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2010 = convert(xmon_fat2010,ymon_fat2010)
#plot(o.cart_fat2008)

distances_fat2010=my.dist(o.cart_fat2010[,1],o.cart_fat2010[,2])

###########   c. 2012 DEATHS
########## 

DEATH_2012 = df_2[which(df_2$yr == 2012), ]
DEATH_2012= DEATH_2012 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2012 = DEATH_2012 [DEATH_2012$slon > -105& DEATH_2012$slat>20,]
DEATH_2012= DEATH_2012 [, -c(1,2)]

xmon_fat2012=DEATH_2012$slon
ymon_fat2012=DEATH_2012$slat

#plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
#text(xmon_fat2008,ymon_fat2008,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2012 = convert(xmon_fat2012,ymon_fat2012)
#plot(o.cart_fat2008)

distances_fat2012=my.dist(o.cart_fat2012[,1],o.cart_fat2012[,2])


###########   c. 2013 DEATHS
########## 

DEATH_2013 = df_2[which(df_2$yr == 2013), ]
DEATH_2013= DEATH_2013 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2013 = DEATH_2013 [DEATH_2013$slon > -105& DEATH_2013$slat>20,]
DEATH_2013= DEATH_2013 [, -c(1,2)]

xmon_fat2013=DEATH_2013$slon
ymon_fat2013=DEATH_2013$slat

#plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
#text(xmon_fat2008,ymon_fat2008,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2013 = convert(xmon_fat2013,ymon_fat2013)
#plot(o.cart_fat2008)

distances_fat2013=my.dist(o.cart_fat2013[,1],o.cart_fat2013[,2])


###########   c. 2014 DEATHS
########## 


DEATH_2014 = df_2[which(df_2$yr == 2014), ]
DEATH_2014= DEATH_2014 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2014 = DEATH_2014 [DEATH_2014$slon > -105& DEATH_2014$slat>20,]
DEATH_2014= DEATH_2014 [, -c(1,2)]

xmon_fat2014=DEATH_2014$slon
ymon_fat2014=DEATH_2014$slat

#plot(xmon_fat2014,ymon_fat2014,type="n",xlab="Longitude W", ylab="Latitude N",main="2014 fatalities EF>2 ")
#text(xmon_fat2014,ymon_fat2014,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2014 = convert(xmon_fat2014,ymon_fat2014)
#plot(o.cart_fat2008)

distances_fat2014=my.dist(o.cart_fat2014[,1],o.cart_fat2014[,2])


###########   c. 2015 DEATHS
########## 


DEATH_2015 = df_2[which(df_2$yr == 2015), ]
DEATH_2015= DEATH_2015 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2015 = DEATH_2015 [DEATH_2015$slon > -105& DEATH_2015$slat>20,]
DEATH_2015= DEATH_2015 [, -c(1,2)]

xmon_fat2015=DEATH_2015$slon
ymon_fat2015=DEATH_2015$slat

#plot(xmon_fat2014,ymon_fat2014,type="n",xlab="Longitude W", ylab="Latitude N",main="2014 fatalities EF>2 ")
#text(xmon_fat2014,ymon_fat2014,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2015 = convert(xmon_fat2015,ymon_fat2015)
#plot(o.cart_fat2008)

distances_fat2015=my.dist(o.cart_fat2015[,1],o.cart_fat2015[,2])


###########   c. 2016 DEATHS
########## 


DEATH_2016 = df_2[which(df_2$yr == 2016), ]
DEATH_2016= DEATH_2016 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2016 = DEATH_2016 [DEATH_2016$slon > -105& DEATH_2016$slat>20,]
DEATH_2016= DEATH_2016 [, -c(1,2)]

xmon_fat2016=DEATH_2016$slon
ymon_fat2016=DEATH_2016$slat

#plot(xmon_fat2014,ymon_fat2014,type="n",xlab="Longitude W", ylab="Latitude N",main="2014 fatalities EF>2 ")
#text(xmon_fat2014,ymon_fat2014,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2016 = convert(xmon_fat2016,ymon_fat2016)
#plot(o.cart_fat2008)

distances_fat2016=my.dist(o.cart_fat2016[,1],o.cart_fat2016[,2])


###########   c. 2017 DEATHS
########## 


DEATH_2017 = df_2[which(df_2$yr == 2017), ]
DEATH_2017= DEATH_2017 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2017 = DEATH_2017 [DEATH_2017$slon > -105& DEATH_2017$slat>20,]
DEATH_2017= DEATH_2017 [, -c(1,2)]

xmon_fat2017=DEATH_2017$slon
ymon_fat2017=DEATH_2017$slat

#plot(xmon_fat2014,ymon_fat2014,type="n",xlab="Longitude W", ylab="Latitude N",main="2014 fatalities EF>2 ")
#text(xmon_fat2014,ymon_fat2014,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2017 = convert(xmon_fat2017,ymon_fat2017)
#plot(o.cart_fat2008)

distances_fat2017=my.dist(o.cart_fat2017[,1],o.cart_fat2017[,2])

###########   D. 2018 DEATHS
########## 


DEATH_2018 = df_2[which(df_2$yr == 2018), ]
DEATH_2018= DEATH_2018 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
DEATH_2018 = DEATH_2018 [DEATH_2018$slon > -105& DEATH_2018$slat>20,]
DEATH_2018= DEATH_2018 [, -c(1,2)]

xmon_fat2018=DEATH_2018$slon
ymon_fat2018=DEATH_2018$slat

#plot(xmon_fat2018,ymon_fat2018,type="n",xlab="Longitude W", ylab="Latitude N",main="2018 fatalities EF>2 ")
#text(xmon_fat2018,ymon_fat2018,1:11,col="blue",cex=1.5)
#map("state", add=TRUE)

#Converts lat-long coordinates into carthesian coordinates 

o.cart_fat2018 = convert(xmon_fat2018,ymon_fat2018)
#plot(o.cart_fat2008)
distances_fat2018=my.dist(o.cart_fat2018[,1],o.cart_fat2018[,2])


###########   E. PLOTS FOR A-D
########## 

install.packages("maps")
library(maps)
par(mfrow=c(1,1))
par(mfrow=c(2,2))


plot(xmon_fat2007,ymon_fat2007,type="n",xlab="Longitude W", ylab="Latitude N",main="2007 fatalities EF>2 ")
text(xmon_fat2007,ymon_fat2007,1:11,col="blue",cex=1.5)
map("state", add=TRUE)

plot1= plot(xmon_fat2008,ymon_fat2008,type="n",xlab="Longitude W", ylab="Latitude N",main="2008 fatalities EF>2 ")
text(xmon_fat2008,ymon_fat2008,1:11,col="red",cex=1.5)
map("state", add=TRUE)


plot(xmon_fat2010,ymon_fat2010,type="n",xlab="Longitude W", ylab="Latitude N",main="2010 fatalities EF>2 ")
text(xmon_fat2010,ymon_fat2010,1:11,col="darkgreen",cex=1.5)
map("state", add=TRUE)

plot(xmon_fat2011,ymon_fat2011,type="n",xlab="Longitude W", ylab="Latitude N",main="2011 fatalities EF>2 ")
text(xmon_fat2011,ymon_fat2011,1:11,col="purple",cex=1.5)
map("state", add=TRUE)

par(mfrow=c(1,1))

###########  F. DIAGNOSTICS 
########## 

#lag_2008 = df2[which(df2$yr == 2008), ]
#lag_2008= lag_2008 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
#lag_2008 = lag_2008 [lag_2008$slon > -105& lag_2008$slat>20,]
#lag_2008= lag_2008 [, -c(1,2)]

#library(harrietr)
#melt_dist2008= melt_dist(distances_fat2008)
#coordinates(lag_2008) = c("slon", "slat")
#variogram(log(lag_2008$fat) ~ 1, lag_2008, cloud = TRUE)
#hscat(log(fat) ~ 1, lag_2008, (0:9) * 100)
#v= variogram(log(lag_2008$fat) ~ 1, lag_2008)
#plot(v)
#sel <- plot(variogram(lag_2008$fat ~ 1, lag_2008, cloud = TRUE), digitize = TRUE)
#plot(sel, lag_2008)

###########  F. 2007
library(harrietr)
library(phylin)
par("mar")
par(mfrow=c(1,1))
#par(mar=c(1,1,1,1))
lag_2007 = df2[which(df2$yr == 2007), ]
lag_2007= lag_2007 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2007 = lag_2007 [lag_2007$slon > -105& lag_2007$slat>20,]
lag_2007= lag_2007 [, -c(1,2)]



hc <- hclust(as.dist(distances_fat2007))
plot(hc)

r.dist <- dist(lag_2007[,1:2])
gv <- gen.variogram(r.dist, distances_fat2007)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)


###########  F. 2008
lag_2008 = df2[which(df2$yr == 2008), ]
lag_2008= lag_2008 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2008 = lag_2008 [lag_2008$slon > -105& lag_2008$slat>20,]
lag_2008= lag_2008 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2008))
plot(hc, hang = -1)

r.dist <- dist(lag_2008[,1:2])
gv <- gen.variogram(r.dist, distances_fat2008)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2_ <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2_)
plot(gv.linear)


###########  F. 2009
lag_2009 = df2[which(df2$yr == 2009), ]
lag_2009= lag_2009 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2009 = lag_2009 [lag_2009$slon > -105& lag_2009$slat>20,]
lag_2009= lag_2009 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2009))
plot(hc, hang = -1)

r.dist <- dist(lag_2009[,1:2])
gv <- gen.variogram(r.dist, distances_fat2009)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2010
lag_2010 = df2[which(df2$yr == 2010), ]
lag_2010= lag_2010 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2010 = lag_2010 [lag_2010$slon > -105& lag_2010$slat>20,]
lag_2010= lag_2010 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2010))
plot(hc)

r.dist <- dist(lag_2010[,1:2])
gv <- gen.variogram(r.dist, distances_fat2010)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2011
lag_2011 = df2[which(df2$yr == 2011), ]
lag_2011= lag_2011 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2011 = lag_2011 [lag_2011$slon > -105& lag_2011$slat>20,]
lag_2011= lag_2011 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2011))
plot(hc)

r.dist <- dist(lag_2011[,1:2])
gv <- gen.variogram(r.dist, distances_fat2011)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2012
lag_2012 = df2[which(df2$yr == 2012), ]
lag_2012= lag_2012 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2012 = lag_2012 [lag_2012$slon > -105& lag_2012$slat>20,]
lag_2012= lag_2012 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2012))
plot(hc)

r.dist <- dist(lag_2012[,1:2])
gv <- gen.variogram(r.dist, distances_fat2012)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2013
lag_2013 = df2[which(df2$yr == 2013), ]
lag_2013= lag_2013 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2013 = lag_2013 [lag_2013$slon > -105& lag_2013$slat>20,]
lag_2013= lag_2013 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2013))
plot(hc)

r.dist <- dist(lag_2013[,1:2])
gv <- gen.variogram(r.dist, distances_fat2013)
plot(gv)
gv <- gv.model(gv, model='linear',)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=8)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)


###########  F. 2014
lag_2014 = df2[which(df2$yr == 2014), ]
lag_2014= lag_2014 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2014 = lag_2014 [lag_2014$slon > -105& lag_2014$slat>20,]
lag_2014= lag_2014 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2014))
plot(hc)

r.dist <- dist(lag_2014[,1:2])
gv <- gen.variogram(r.dist, distances_fat2014)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=.28)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2015
lag_2015 = df2[which(df2$yr == 2015), ]
lag_2015= lag_2015 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2015 = lag_2015 [lag_2015$slon > -105& lag_2015$slat>20,]
lag_2015= lag_2015 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2015))
plot(hc)

r.dist <- dist(lag_2015[,1:2])
gv <- gen.variogram(r.dist, distances_fat2015)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=.28)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)


###########  F. 2016
lag_2016 = df2[which(df2$yr == 2016), ]
lag_2016= lag_2016 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2016 = lag_2016 [lag_2016$slon > -105& lag_2016$slat>20,]
lag_2016= lag_2016 [, -c(1,2)]

hc <- hclust(as.dist(distances_fat2016))
plot(hc)

r.dist <- dist(lag_2016[,1:2])
gv <- gen.variogram(r.dist, distances_fat2016)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='linear', range=.28)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

###########  F. 2017
lag_2017 = df2[which(df2$yr == 2017), ]
lag_2017= lag_2017 [, -c(1,2,3,4,5,6,7,9,10,12,14,15,18,19,20,21,22,23,24,25,26,27,28,29)]
lag_2017 = lag_2017 [lag_2017$slon > -105& lag_2017$slat>20,]
lag_2017= lag_2017 [, -c(1,2)]


hc <- hclust(as.dist(distances_fat2017))
plot(hc)

r.dist <- dist(lag_2017[,1:2])
gv <- gen.variogram(r.dist, distances_fat2017)
plot(gv)
gv <- gv.model(gv)
plot(gv)
gv2 <- gv.model(gv, range=8)
gv.linear <- gv.model(gv, model='exponential', range=.28)
layout(matrix(1:2, 1, 2))
plot(gv2)
plot(gv.linear)

############## VARIOGRAMS 2
#############

#2017
library(geoR)
coordinates(lag_2017) <- c("slon", "slat")

plot(variogram(((lag_2017$fat)^(1/3)) ~ 1, lag_2017, alpha = c(0, 45, 90, 135)))
v= variogram((lag_2017$fat)^1/2 ~ 1, lag_2017, cutoff=50)
v.fit= fit.variogram(v, vgm(1,"Log",5,1))
plot(v,v.fit,col="red", pch= 16, lty = 2, cex = 1.25)
attr(v.fit, "SSErr")

#2007
library(geoR)
library(data.table)
lag_2007_cp= copy(lag_2007)
coordinates(lag_2007_cp) <- c("slon", "slat")

#plot(variogram(((lag_2007_cp$fat)^(1/3)) ~ 1, lag_2007_cp, alpha = c(0, 45, 90, 135)))
v.2007= variogram((lag_2007_cp$fat)^1/2 ~ 1, lag_2007_cp, cutoff=55)
v.2007_= variogram((lag_2007_cp$fat)^1/2 ~ 1, lag_2007_cp)
v.fit.2007= fit.variogram(v.2007, vgm(1,"Log",5,.1))
plot(v.2007,v.fit.2007,col="red", pch= 16, lty = 2, cex = 1.25, main="2007,Nugget=")
attr(v.fit.2007, "SSErr")


sel <- plot(variogram(zinc ~ 1, meuse, cloud = TRUE), + digitize = TRUE)

par("mar")
#par(mar=c(1,1,1,1))
par(mfrow=c(1,3))
plot((variogram((lag_2007_cp$fat)^1/2 ~ 1, lag_2007_cp, cutoff=55,cloud=TRUE)), main="2007")

######
#2008
library(geoR)
lag_2008_cp= copy(lag_2008)
lag_2008_cp2= lag_2008_cp[which(lag_2008_cp$fat > 0), ];lag_2008_cp2
lag_2008_cp2
coordinates(lag_2008_cp2) <- c("slon", "slat")

#plot(variogram(((lag_2007_cp$fat)^(1/3)) ~ 1, lag_2007_cp, alpha = c(0, 45, 90, 135)))
v.2008= variogram(log(lag_2008_cp2$fat) ~ 1, lag_2008_cp2)
v.2008.cl= variogram(log(lag_2008_cp2$fat) ~ 1, lag_2008_cp2, cloud = TRUE)
layout(matrix(1:2, 1, 2))
plot(v.2008, main="Variogram: Fatalities in 2008",  pch= 16, lty = 2, cex = 1.25,col="purple")
plot(v.2008.cl, main="Cloud Variogram: Fatalities in 2008", pch= 8, lty = 2, cex = 1.25,col="purple")

v.2008_= variogram((lag_2007_cp$fat)^1/2 ~ 1, lag_2007_cp)
v.fit.2008= fit.variogram(v.2008, vgm(.35,"Log",.85,.2))
                          
plot(v.2008,v.fit.2008,col="red", pch= 16, lty = 2, cex = 1.25, main="Variogram (Log(Fatalities) 2008:Nugget=0.326, Sill= 0.846, Range=9")
attr(v.fit.2008, "SSErr")


library(automap)
kr = autoKrige(log(lag_2008_cp2$fat) ~ 1, lag_2008_cp2)
plot(kr)
########
########


