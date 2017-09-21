#load
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
# total emission by year
total_by_year<-NEI %>% group_by(year) %>% summarize(emi=round(sum(Emissions)/1000000))
plot(total_by_year, type="b", ylab="Emission (Million Tons)", main="US Total Emission")


# city of Beltimore
bmi<- NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(emi=sum(Emissions))
plot(bmi, type="b", ylab="Emission (Tons)", main="Baltimore MD Total Emission")

library(ggplot2)
bmi_by_type<- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>% summarize(emi=sum(Emissions))
# use log(emi) to amplify smaller Y value
g<-qplot(year, emi , data=bmi_by_type)
g + geom_point(aes(col=type), size=4) + geom_line(aes(col=type))+ ggtitle("Baltimore Emission by Type") + ylab("emission (tons)")

# best match for "coal combustion" is in El.Sector AFAIK
# > grep("[C|c]oal",unique(SCC$EI.Sector), value = T)
# [1] "Fuel Comb - Electric Generation - Coal"      "Fuel Comb - Industrial Boilers, ICEs - Coal"
# [3] "Fuel Comb - Comm/Institutional - Coal"      
coal_code<-SCC[grep("Coal",SCC$EI.Sector), 1]
coal_emi_by_year<-NEI %>% filter(SCC %in% coal_code) %>% group_by(year) %>% summarize(emi=round(sum(Emissions)/1000))
plot(coal_emi_by_year, type="b", ylab="Emission (Thousand Tons)", main="US Coal Emission(PM2.5)")

