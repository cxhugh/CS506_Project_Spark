#Data is filter in Boston Permits Datset comments column, number before KW(kilowatts)
#This is how we produce Figure 5.14 
#Solar panel capacity stack bar chart, increased by AV_TOTALPerArea_PerZip 
stack = read.csv("Permit_Solar.csv")
stack
colnames(stack) = c("ZIPCODE", "1-2FAM", "1-3FAM", "2unit", "Comm", "Mixed", "Multi", "Other")
stack = stack[-c(1,2),]
require(reshape2)
dstack <- melt(stack,id.vars = 1)

dstack$ZIPCODE<-as.vector(dstack$ZIPCODE)
dstack$ZIPCODE<-as.factor(dstack$ZIPCODE)


#zicode increased by AV_TOTALPerArea_PerZip
data = read.csv("C:/Users/cindy/Desktop/spring 2018/CS506/FinalProject/Data_visualization.csv")
data$zip = data$ZIPCODE
data$ZIPCODE = NULL
data$zip <- paste("0", data$zip, sep="")

f = data[
  with(data, order(data$AV_TOTALPerArea_PerZip)),
  ]

#which levels appear in permit solar?
levelswant = paste0("0", levels(dstack$ZIPCODE))

a = unique(data$zip)
order1 = match(levelswant,a)


dff = data.frame(cbind(order1, levelswant))
dff$order1 = as.integer(dff$order1)
dff = dff[order(order1),]


dff = data[
  with(data, order(data$AV_TOTALPerArea_PerZip)),
  ]

dstack$ZIPCODE = paste0("0", dstack$ZIPCODE)

require(ggplo2)
require(scales)
dstacktmp <- within(dstack, 
                   ZIPCODE <- factor(ZIPCODE, levels = levelswant))
dstacktmp$value = as.numeric(dstacktmp$value)
ggplot(dstacktmp, aes(x = ZIPCODE, y = value, fill = variable))+
  geom_bar(stat="identity")+scale_fill_brewer(palette="Spectral")+ylim(0,100)+ylab("Capacity")
+scale_y_continuous(percent_format())

