#load
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)


# total emission by year
total_by_year<-NEI %>% group_by(year) %>% summarize(emi=round(sum(Emissions)/1000000))
dev.copy(png, file="plot1.png")
plot(total_by_year, type="b", ylim=c(0, max(total_by_year$emi)), ylab="Emission (Million Tons)", main="US Total Emission")
dev.off()

# city of Beltimore
bmi<- NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(emi=sum(Emissions))
dev.copy(png, file="plot2.png")
plot(bmi, type="b", ylim=c(0, max(bmi$emi)), ylab="Emission (Tons)", main="Baltimore MD Total Emission")
dev.off()

# Baltimore by type
bmi_by_type<- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>% summarize(emi=sum(Emissions))
# use log(emi) to amplify smaller Y value if desire
dev.copy(png, file="plot3.png")
g<-qplot(year, emi , data=bmi_by_type)
g + geom_point(aes(col=type), size=4) + geom_line(aes(col=type))+ ggtitle("Baltimore Emission by Type") + ylab("emission (tons)")
dev.off()

# best match for "coal combustion" is in El.Sector AFAIK
# > grep("[C|c]oal",unique(SCC$EI.Sector), value = T)
# [1] "Fuel Comb - Electric Generation - Coal"      "Fuel Comb - Industrial Boilers, ICEs - Coal"
# [3] "Fuel Comb - Comm/Institutional - Coal"      
coal_code<-SCC[grep("Coal",SCC$EI.Sector), 1]
coal_emi_by_year<-NEI %>% filter(SCC %in% coal_code) %>% group_by(year) %>% summarize(emi=round(sum(Emissions)/1000))
dev.copy(png, file="plot4.png")
plot(coal_emi_by_year, type="b", ylim=c(0, max(coal_emi_by_year$emi)), ylab="Emission (Thousand Tons)", main="US Coal Emission(PM2.5)")
dev.off()

# Baltimore vehicle emission
bmi_onroad_by_year<- NEI %>% filter(type=='ON-ROAD', fips=="24510") %>% group_by(year) %>% summarize(emi=sum(Emissions))
dev.copy(png, file="plot5.png")
plot(bmi_onroad_by_year, type="b", ylim=c(0, max(bmi_onroad_by_year$emi)), ylab="Emission (tons)", main="Baltimore Vehicle Emission(PM2.5)")
dev.off()

# Baltimore vs Los Angeles
bmi_onroad_by_year<- NEI %>% filter(type=='ON-ROAD', fips=="24510") %>% group_by(year) %>% summarize(emi=sum(Emissions))
lax_onroad_by_year<- NEI %>% filter(type=='ON-ROAD', fips=="06037") %>% group_by(year) %>% summarize(emi=sum(Emissions))
# create percentage change because the total emission between those 2 are big
# add city name for 2 seriers in plot
bmi_onroad_by_year<-bmi_onroad_by_year %>% mutate(pct_change=emi/max(emi)*100, city="Baltimore City")
lax_onroad_by_year<-lax_onroad_by_year %>% mutate(pct_change=emi/max(emi)*100, city="Los Angeles County")
bmi_lax<-rbind(lax_onroad_by_year, bmi_onroad_by_year)
dev.copy(png, file="plot6.png")
g<-qplot(year, pct_change,data=bmi_lax)
g+geom_point(aes(col=city),size=4) + geom_line(aes(col=city), show.legend = F) + ggtitle("Percentage change over year") +  ylab("pct over worst year")
dev.off()



