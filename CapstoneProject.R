## Load in the data
VTT_Bouncerate_Percentexit <- read.csv("VTT_BouncerateVsPercentexit.csv")
VTT_ConversionAOV <- read.csv("VTT_ConversionVsAOV.csv")
VTT_Revenue <- read.csv("VTT_Revenue.csv")
VTT_Traffic <- read.csv("VTT_Traffic.csv")

## Change the headers for the bounceRate vs percentExit table and convert everything to numeric
names(VTT_Bouncerate_Percentexit) <- c("Week_Index", "Bounce_Rate", "Percent_Exit")
VTT_Bouncerate_Percentexit$Bounce_Rate <- as.numeric(sub("%", "", VTT_Bouncerate_Percentexit$Bounce_Rate))/100
VTT_Bouncerate_Percentexit$Percent_Exit <- as.numeric(sub("%", "", VTT_Bouncerate_Percentexit$Percent_Exit))/100

## Change the headers for the conversion vs aov table and convert everything to numeric
names(VTT_ConversionAOV) <- c("Week_Index", "Conversion", "AOV")
VTT_ConversionAOV$Conversion <- as.numeric(sub("%", "", VTT_ConversionAOV$Conversion))/100
VTT_ConversionAOV$AOV <- as.numeric(sub("\\$", "", VTT_ConversionAOV$AOV))

## Change the headers for the revenue table and convert Revenue column to numeric
names(VTT_Revenue) <- c("Week_Index", "Revenue")
VTT_Revenue$Revenue <- sub("\\$", "", VTT_Revenue$Revenue)
VTT_Revenue$Revenue <- as.numeric(sub(",", "", VTT_Revenue$Revenue))

## Change the headers for the traffic table and convert Traffic column to numeric
names(VTT_Traffic) <- c("Week_Index", "Traffic")
VTT_Traffic$Traffic <- as.numeric(sub(",", "", VTT_Traffic$Traffic))

## Build Summary Table
SummaryTable_VTT <- VTT_Bouncerate_Percentexit
SummaryTable_VTT$Conversion <- VTT_ConversionAOV$Conversion
SummaryTable_VTT$AOV <- VTT_ConversionAOV$AOV
SummaryTable_VTT$Revenue <- VTT_Revenue$Revenue
SummaryTable_VTT$Traffic <- VTT_Traffic$Traffic

## Build Regression Model
summary(lm(Revenue ~ Conversion + AOV + Traffic, data = SummaryTable_VTT))
