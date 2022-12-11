#set required libraries
library(dplyr)
library(readxl)
library(pdftools)
library(tidyr)
library(ggplot2)
library(reshape2)

#read in file which as sales data for pev models from 2011 to 2019
evSalesthru2019 <- read_xlsx("C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/10567_pev_sales_2-28-20.xlsx")

#promote first line to headers and remove extraneous notes and labels, filter by EV only
colnames(evSalesthru2019) <- as.character(evSalesthru2019[1,])
evSalesthru2019 <- evSalesthru2019[-c(1),]
evSalesthru2019 <- evSalesthru2019[-c(57:66),]
evSalesthru2019 <- evSalesthru2019 %>% filter(Type=="EV")
evSalesthru2019$Total <- as.integer(evSalesthru2019$Total)

#read in file which as sales data for pev models from 2020 to 2021
evSalesthru2022 <- pdftools::pdf_text(pdf = "C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/Q3-2022-Kelley-Blue-Book-Electrified-Vehicle-Sales-Report.pdf")

#keep electric vehicles and convert data into a matrix
evSalesthru2022 <- evSalesthru2022[2]
evSalesthru2022 <- gsub ('                                        ELECTRIC VEHICLES\n                                       Q3 Sales                   YTD Sales          Segment Share\n                                 ','Model  ',evSalesthru2022)
evSalesthru2022 <- as.data.frame(matrix(unlist(strsplit(evSalesthru2022, "\n"))))
evSalesthru2022[5,] = "BMW i8                -         -         -         5        10   -50.0%   0.0% 0.0%"
evSalesthru2022[35,] = "Tesla Model Y                  60,271   50,430     20%    191,451   127,007      50.7% 29.3% 33.2%"
evSalesthru2022 <- evSalesthru2022 %>% separate('V1', c('Model', 'Q32022', 'Q32021', 'Q3YOY', 'YTD2022', 'YTD2021', 'YTDYOY', 'Q2'), sep="\\s{2,}")
evSalesthru2022 <- evSalesthru2022[-c(45),]
evSalesthru2022 <- evSalesthru2022[-c(40:41),]
evSalesthru2022 <- evSalesthru2022[-c(1),]
evSalesthru2022 <- evSalesthru2022[,-c(7:8)]
evSalesthru2022 <- evSalesthru2022[,-c(2:4)]
evSalesthru2022[8,1] <- "Chevy Bolt"

#increase YTD2022 by 33% to match a full year of sales.  Assume 1/3 based on 3 quarters in YTD total.
evSalesthru2022$YTD2021 <- gsub (',','',evSalesthru2022$YTD2021)
evSalesthru2022$YTD2022 <- gsub (',','',evSalesthru2022$YTD2022)
evSalesthru2022$YTD2021 <- as.integer(evSalesthru2022$YTD2021)
evSalesthru2022$YTD2022 <- as.integer(evSalesthru2022$YTD2022)
evSalesthru2022$YTD2022 <- as.integer(evSalesthru2022$YTD2022 * 1.33)

#read in file which has vehicle model and published ranges
rangeByYearModel1 <- read_xlsx("C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/10963_EV_range_efficiency_1-6-22.xlsx")

#promote first line to headers and remove extraneous data
colnames(rangeByYearModel1) <- as.character(rangeByYearModel1[1,])
rangeByYearModel1 <- rangeByYearModel1[-c(1),]
rangeByYearModel1 <- rangeByYearModel1[,-c(5:9)]
rangeByYearModel1 <- rangeByYearModel1[-c(124:139),]
rangeByYearModel1$Range <- as.integer(rangeByYearModel1$Range)
summary(rangeByYearModel1$Range)

#read in file which has vehicle model and published ranges
rangeByYearModel2 <- read.csv("C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/vehicles-2022-09-11.csv")

#filter for only fully electric vehicles
rangeByYearModel2 <- rangeByYearModel2 %>% filter(Fuel=="Electric")
summary(rangeByYearModel2$All.Electric.Range)

#sort by total 2011-2019 by model
evSalesthru2019 <- evSalesthru2019[order(evSalesthru2019$Total,decreasing=FALSE), ]
evSalesthru2019 <- evSalesthru2019 %>% filter(Total>10000)

#sort by total 2022 by model
evSalesthru2022 <- evSalesthru2022[order(evSalesthru2022$YTD2022,decreasing=FALSE), ]
evSalesthru2022 <- evSalesthru2022 %>% filter(YTD2022>10000)

#combine data evSalesthru2019 & evSalesthru2022
evSales <- merge(evSalesthru2019, evSalesthru2022, by.x = "Vehicle", by.y = "Model",all=TRUE)
evSales <- evSales[,-c(12)]
evSales <- evSales[,-c(2)]
colnames(evSales)[colnames(evSales) == "YTD2021"] <- "2021"
colnames(evSales)[colnames(evSales) == "YTD2022"] <- "2022"
#3 models do not have 2021-2022 sales data.  All three are being discontinued or entering the market with a rebranded model.
evSales <- evSales[!is.na(evSales$`2022`),]

#plot sales from 2011 to 2022 sales by model from filtered file
evSales.melt <- reshape2::melt(evSales, id.vars="Vehicle")
evSales.melt <- na.omit(evSales.melt)
evSales.melt$variable <- as.integer(sub("^X", "", evSales.melt$variable))
colnames(evSales.melt)[colnames(evSales.melt) == "variable"] <- "Year"
colnames(evSales.melt)[colnames(evSales.melt) == "value"] <- "Total"
evSales.melt <- evSales.melt %>% mutate(Year = factor(Year))

ggplot(evSales.melt, aes(x = Year, y = Total, group = Vehicle)) +
  geom_line(aes(col = Vehicle)) + geom_point(aes(col = Vehicle),size=2) + theme(axis.text.x = element_text(angle=40))

ggplot(evSales.melt, aes(x = Year, y = Total)) +   
  geom_bar(aes(fill = Vehicle), stat="identity")

#create linear model to predict ev sales overall
evSales.melt$Year <- as.numeric(evSales.melt$Year)
with(evSales.melt,plot(Year,Total,xlab="Year",ylab="Total",main='EV Sales Linear Model'))
evSales.linear.model <- with(evSales.melt,lm(Total~Year))
abline(evSales.linear.model,col='blue')
summary(evSales.linear.model)$coef

#combine filtered list of current EV sales with documented range
modelRanges <- evSales
rangeByYearModel1$Vehicle <- paste(rangeByYearModel1$Make, rangeByYearModel1$Model, sep=" ")
rangeByYearModel1 <- rangeByYearModel1[,-c(2:3)]

rangeByYearModel1.filtered <- rangeByYearModel1 %>% filter(`Model Year` == '2021')
rangeByYearModel1.filtered[3,3] <- 'Chevy Bolt'
modelRanges <-  merge(rangeByYearModel1.filtered, modelRanges, by.x = "Vehicle", by.y = "Vehicle",all=TRUE)
modelRanges <- modelRanges[!is.na(modelRanges$`2022`),]

rangeByYearModel1.filtered <- rangeByYearModel1 %>% filter(`Model Year` == '2020')
modelRanges[5,1] <- 'Hyundai Ioniq'
modelRanges <-  merge(rangeByYearModel1.filtered, modelRanges, by.x = "Vehicle", by.y = "Vehicle", all=TRUE)
modelRanges <- modelRanges[!is.na(modelRanges$`2022`),]

rangeByYearModel2$Vehicle <- paste(rangeByYearModel2$Manufacturer, rangeByYearModel2$Model, sep=" ")
rangeByYearModel2 <- rangeByYearModel2[,-c(1:5)]
rangeByYearModel2 <- rangeByYearModel2[,-c(2:17)]
modelRanges[3,1] <- "Ford F150 Lightning 4WD"
modelRanges[6,1] <- "Kia EV6 RWD"
modelRanges[8,1] <- "Rivian  R1T"
modelRanges[13,1] <- "Volkswagen ID.4 AWD Pro S"
modelRanges <-  merge(rangeByYearModel2, modelRanges, by.x = "Vehicle", by.y = "Vehicle", all=TRUE)
modelRanges <- modelRanges[!is.na(modelRanges$`2022`),]

modelRanges$Range.x[is.na(modelRanges$`Range.x`)] <- modelRanges$All.Electric.Range[is.na(modelRanges$`Range.x`)]
modelRanges$Range.y[is.na(modelRanges$`Range.y`)] <- modelRanges$Range.x[is.na(modelRanges$`Range.y`)]

#model ranges with no reduction in range, mild reduction in range and higher reduction in range
modelRanges <- modelRanges[,-c(2:5)]
summary(modelRanges$Range.y)

modelRanges.med <- modelRanges 
modelRanges.med$Range.y <- modelRanges.med$Range.y * .75
summary(modelRanges.med$Range.y)

modelRanges.high <- modelRanges 
modelRanges.high$Range.y <- modelRanges.high$Range.y * .5
summary(modelRanges.high$Range.y)

modelRanges.plot <- data.frame(modelRanges$Range.y,modelRanges.med$Range.y,modelRanges.high$Range.y)
boxplot(modelRanges.plot,xlab="Electric Vehicle Ranges with reduction due to impacting factors", 
        ylab = "Range in miles",names=c("no impact","medium impact","high impact"))
  
#read in file which as charging station infrastructure
chargingStations <- read.csv("C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/alt_fuel_stations (Oct 15 2022).csv")

#removing level 1-2 charging connectors to create file with DC charging
chargingStations.dc <- chargingStations
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772"] <- "CHADEMO"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772 J1772COMBO"] <- "CHADEMO"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772 NEMA515"] <- "CHADEMO"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772COMBO"] <- "CHADEMO"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772 J1772COMBO NEMA520"] <- "CHADEMO"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772COMBO TESLA"] <- "CHADEMO TESLA"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772 J1772COMBO TESLA"] <- "CHADEMO TESLA"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "CHADEMO J1772 NEMA520 TESLA"] <- "CHADEMO TESLA"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "J1772 TESLA"] <- "TESLA"
chargingStations.dc$EV.Connector.Types[chargingStations.dc$EV.Connector.Types == "J1772COMBO TESLA"] <- "TESLA"

chargingStations.dc <- chargingStations.dc %>% 
    filter(`EV.Connector.Types` != "J1772") %>% filter(`EV.Connector.Types` != "J1772COMBO") %>%
    filter(`EV.Connector.Types` != "J1772 J1772COMBO") %>% filter(`EV.Connector.Types` != "J1772 J1772COMBO NEMA515")

chargingStations.dc <- rename(chargingStations.dc,EVConnectorTypes =  EV.Connector.Types)

write.csv(chargingStations.dc,file="C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/alt_fuel_stations_dc.csv")

#file with level 2 charging & DC, no tesla
chargingStations.lv2dc.filtered <- chargingStations
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772 J1772COMBO"] <- "CHADEMO J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772 NEMA515"] <- "CHADEMO J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772COMBO"] <- "CHADEMO J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772 J1772COMBO NEMA520"] <- "CHADEMO J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772COMBO TESLA"] <- "CHADEMO  J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772 J1772COMBO TESLA"] <- "CHADEMO  J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO J1772 NEMA520 TESLA"] <- "CHADEMO  J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "J1772 TESLA"] <- "J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "J1772COMBO TESLA"] <- "J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "J1772COMBO"] <- "J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "J1772 J1772COMBO NEMA515"] <- "J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "J1772 J1772COMBO"] <- "J1772"
chargingStations.lv2dc.filtered$EV.Connector.Types[chargingStations.lv2dc.filtered$EV.Connector.Types == "CHADEMO  J1772"] <- "CHADEMO J1772"
chargingStations.lv2dc.filtered <- chargingStations.lv2dc.filtered %>% filter(`EV.Connector.Types` != "TESLA") 

#combine totals of level 2 and fast chargers
chargingStations.lv2dc.filtered$EV.Level2.EVSE.Num[is.na(chargingStations.lv2dc.filtered$EV.Level2.EVSE.Num)] <- 0
chargingStations.lv2dc.filtered$Level2.DC.Total <- chargingStations.lv2dc.filtered$EV.Level2.EVSE.Num + chargingStations.lv2dc.filtered$EV.DC.Fast.Count

write.csv(chargingStations.lv2dc.filtered,file="C:/Users/sbace/Documents/DataScience/DS785_Capstone/DataSources/Good/alt_fuel_stations_lv2dc.filtered.csv")
