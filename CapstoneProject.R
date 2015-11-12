library(caret)
library(ggplot2)

## Load in the data
VTT_Bouncerate_Percentexit <- read.csv("VTT_BouncerateVsPercentexit.csv")
VTT_ConversionAOV <- read.csv("VTT_ConversionVsAOV.csv")
VTT_Revenue <- read.csv("VTT_Revenue.csv")
VTT_Traffic <- read.csv("VTT_Traffic.csv")
VTT_SkuCount <- read.csv("SkuCountbyWk.csv")

## Change the headers for the bounceRate vs percentExit table and convert everything to numeric
names(VTT_Bouncerate_Percentexit) <- c("Year", "Week_Index", "Bounce_Rate", "Percent_Exit")
VTT_Bouncerate_Percentexit$Bounce_Rate <- as.numeric(sub("%", "", VTT_Bouncerate_Percentexit$Bounce_Rate))/100
VTT_Bouncerate_Percentexit$Percent_Exit <- as.numeric(sub("%", "", VTT_Bouncerate_Percentexit$Percent_Exit))/100

## Change the headers for the conversion vs aov table and convert everything to numeric
names(VTT_ConversionAOV) <- c("Year", "Week_Index", "Conversion", "AOV")
VTT_ConversionAOV$Conversion <- as.numeric(sub("%", "", VTT_ConversionAOV$Conversion))/100
VTT_ConversionAOV$AOV <- as.numeric(sub("\\$", "", VTT_ConversionAOV$AOV))

## Change the headers for the revenue table and convert Revenue column to numeric
names(VTT_Revenue) <- c("Year", "Week_Index", "Revenue")
VTT_Revenue$Revenue <- sub("\\$", "", VTT_Revenue$Revenue)
VTT_Revenue$Revenue <- as.numeric(sub(",", "", VTT_Revenue$Revenue))

## Change the headers for the traffic table and convert Traffic column to numeric
names(VTT_Traffic) <- c("Year", "Week_Index", "Traffic")
VTT_Traffic$Traffic <- as.numeric(sub(",", "", VTT_Traffic$Traffic))

##Change the headers for the SkuCount table
names(VTT_SkuCount) <- c("Year", "Week_Index", "SkuCount")

## Build Summary Table
SummaryTable_VTT <- VTT_Bouncerate_Percentexit
SummaryTable_VTT$Conversion <- VTT_ConversionAOV$Conversion
SummaryTable_VTT$AOV <- VTT_ConversionAOV$AOV
SummaryTable_VTT$Revenue <- VTT_Revenue$Revenue
SummaryTable_VTT$Traffic <- VTT_Traffic$Traffic
SummaryTable_VTT$SkuCount <- VTT_SkuCount$SkuCount

## Build Regression Model
## summary(lm(Revenue ~ Conversion + AOV + Traffic, data = SummaryTable_VTT))

## Split the summary table into a train and test set, then test it
# VTT_Test <- subset(SummaryTable_VTT, Year == 2015 | Year == 2014 | Year == 2013)
# VTT_Train <- subset(SummaryTable_VTT, Year == 2010 | Year == 2011 | Year == 2012)
VTT_TrainIndex <- createDataPartition(SummaryTable_VTT$Revenue, p = .8, list = FALSE)
VTT_Train2 <- SummaryTable_VTT[VTT_TrainIndex,]
VTT_Test2 <- SummaryTable_VTT[-VTT_TrainIndex,]

mod1 <- lm(Revenue ~ Conversion + AOV + Traffic, data = VTT_Train2)
predictTest <- predict(mod1, newdata = VTT_Test2)

## Calc r^2 for test set
SSE <- sum((VTT_Test2$Revenue - predictTest)^2)
SST <- sum((VTT_Test2$Revenue - mean(SummaryTable_VTT$Revenue))^2)
1 - SSE/SST

#############################################################################
## read and write stuff
TestResult <- gather(Test, "Date", "Quantity", 6:112)
write.table(TestResult, file = "H:/852_2012-14.csv", sep = ",", col.name = NA)
