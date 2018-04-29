require(ggmap)
require(ggplot2)
require(scales)
require(readr)
#read in longitude and latitude
data2 = read.csv("Data_Visualization_LON_LAT.csv")
data2$HEAT_TYPE<-"Electric"
for( i in 1:nrow(data2)){
  if(data2[i,13]=="F"){
    data2[i,19] = "Forced Air"
  }else if(data2[i,13]=="N"){
    data2[i,19] = "None"
  }else if(data2[i,13]=="O"){
    data2[i,19] = "Other"
  }else if(data2[i,13]=="P"){
    data2[i,19] = "Heat Pump"
  }else if(data2[i,13]=="S"){
    data2[i,19] = "Space Heater"
  }else if(data2[i,13]=="W"){
    data2[i,19] = "Hot Water"
  }
}
map = ggmap(get_googlemap(center = c(lon=-71.1, lat = 42.325), zoom = 12, color = "bw"))
map+geom_point(alpha = 0.3, aes(x = LONGITUDE, y = LATITUDE, colour = HEAT_TYPE),size = 0.00001,data = data2)
#forced air hot water
require(dplyr)
require(tidyr)
data2 = as.data.frame(data2)
fh = filter(data2, HEAT_TYPE %in% c("Forced Air", "Hot Water"))
fhplot = map+geom_point(alpha = 0.3, aes(x = LONGITUDE, y = LATITUDE, colour = HEAT_TYPE),size = 1,data = fh)
png(filename="fhres=1400test.png", width=10000, height=6000, res=1400)
fhplot
dev.off()

#electric none space heater other
ensh = filter(data2, HEAT_TYPE %in% c("Electric", "None", "Other", "Space Heater"))
enshplot = map+geom_point(alpha = 0.3, aes(x = LONGITUDE, y = LATITUDE, colour = HEAT_TYPE),size = 1,data = ensh)+guides(shape = guide_legend(override.aes = list(size = 5)))
png(filename="enshres=1400test.png", width=10000, height=6000, res=1400)
enshplot
dev.off()

#heat pumps
#Because of heat pumps are in small percentage 
data14 = read.csv("property-assessment-fy2014.csv")
#average assessed value per zip
data14lonlat = data14[,c("Parcel_ID", "R_HEAT_TYP", "U_HEAT_TYP", "Location", "AV_TOTAL", "LIVING_AREA")]
data14lonlat = na.omit(data14lonlat)
data14lonlat<- separate(data14lonlat, Location, into = c("tmp", "lat","tmp3","lon", "tmp2"), sep = c(1, 13, 16, -2))
data14lonlat = data14lonlat[, -c(4,6,8)]

foo = data14lonlat
foo$HEAT_TYP = paste0(foo$R_HEAT_TYP, foo$U_HEAT_TYP)
foo$R_HEAT_TYP = ifelse(foo$R_HEAT_TYP!="","R", "")
foo$U_HEAT_TYP = ifelse(foo$U_HEAT_TYP!="","U", "")
foo$RorU = paste0(foo$R_HEAT_TYP, foo$U_HEAT_TYP)
foo$R_HEAT_TYP = NULL
foo$U_HEAT_TYP = NULL
foo = foo[!(foo$RorU==""), ]
foo$AV_TOTALPerArea = foo$AV_TOTAL/foo$LIVING_AREA

foo = na.omit(foo)
#delete living area=0 and average total = 0
foo2 = foo[-which(foo$LIVING_AREA==0),]
foo2 = foo2[-which(foo2$AV_TOTAL == 0),]

#select heat pump for 2014
hp2014 = filter(foo2, HEAT_TYP == "P")
hp2014$lon = as.numeric(hp2014$lon)
hp2014$lat = as.numeric(hp2014$lat)
hp2014$lon = -1*abs(hp2014$lon)
map+geom_point(alpha = 0.3, aes(x = lon, y = lat, colour = HEAT_TYP),size = 0.00001,data = hp2014)


#heat pumps for 2018
data18 = read_csv("ast2018full.csv")
foop3 = data18[, c(1,42,70)]
foop3 = foop3 %>% replace_na(list(R_HEAT_TYP = "", U_HEAT_TYP = ""))


foop3$HEAT_TYP = paste0(foop3$R_HEAT_TYP, foop3$U_HEAT_TYP)
foop3$R_HEAT_TYP = ifelse(foop3$R_HEAT_TYP!="","R", "")
foop3$U_HEAT_TYP = ifelse(foop3$U_HEAT_TYP!="","U", "")
foop3$RorU = paste0(foop3$R_HEAT_TYP, foop3$U_HEAT_TYP)
foop3$R_HEAT_TYP = NULL
foop3$U_HEAT_TYP = NULL
foop3 = foop3[!(foop3$RorU==""), ]
#2018 heat pumps
foopp = filter(foop3, HEAT_TYP == "P")

#since 2018 parcel data doesn't have longitude and latitude
#data14lonlat
data14lonlat<- separate(data14lonlat, Parcel_ID, into = c("PID"), sep = c(-2))
lonlat14 = data14lonlat[, c(1,5,6)]

#2018 heat pumps joined by 2014 longtidue and latitude parcel data
foopp = left_join(foopp, lonlat14, by = "PID")
foopp$lon = as.numeric(foopp$lon)
foopp$lon = -1*abs(foopp$lon)
foopp$lat = as.numeric(foopp$lat)
map+geom_point(alpha = 0.3, aes(x = lon, y = lat),size = 0.00001,color = "Blue",data = foopp)

