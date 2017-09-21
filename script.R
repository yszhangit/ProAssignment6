total_by_year<-NEI %>% group_by(year) %>% summarize(emi=sum(Emissions))
plot(total_by_year, type="b") # s is good too

bmi<- NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(emi=sum(Emissions))
plot(bmi, type="b")

bmi_by_type<- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>% summarize(emi=sum(Emissions))
# use log(emi) to amplify smaller Y value
qplot(year, emi , data=bmi_by_type, col=type, size=5) + geom_line(aes(size=2))

# best match for "coal combustion" is in El.Sector AFAIK
# > grep("[C|c]oal",unique(SCC$EI.Sector), value = T)
# [1] "Fuel Comb - Electric Generation - Coal"      "Fuel Comb - Industrial Boilers, ICEs - Coal"
# [3] "Fuel Comb - Comm/Institutional - Coal"      
coal_code<-SCC[grep("Coal",SCC$EI.Sector), 1]
coal_emi_by_year<-NEI %>% filter(SCC %in% coal_code) %>% group_by(year) %>% summarize(emi=sum(Emissions))