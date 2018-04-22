condoDataset <- read.csv(file = "/home/niru/allInfoDir/otherWorkspaces/geophy/DOF__Condominium_comparable_rental_income___Manhattan_-_FY_2010_2011.csv", as.is = T)
condoDataset <- condoDataset[!is.na(condoDataset$Latitude), ]
greenThumbDataset <- read.csv(file = "/home/niru/allInfoDir/otherWorkspaces/geophy/NYC_Greenthumb_Community_Gardens.csv", as.is = T)
greenThumbDataset <- greenThumbDataset[!is.na(greenThumbDataset$Latitude), ]

library(ggmap)
library(mapproj)

range(condoDataset$Latitude)
range(condoDataset$Longitude)
## Visualisation of datasets
centerGreen <- c(mean(range(greenThumbDataset$Longitude)), mean(range(greenThumbDataset$Latitude)))

mapNY <- get_googlemap(center = centerGreen, zoom = 11)
mapPointsDA <- ggmap(mapNY) + 
  geom_point(aes(x = condoDataset$Longitude, y = condoDataset$Latitude), data = condoDataset, alpha = .5, col = "black") + 
  geom_point(aes(x = greenThumbDataset$Longitude, y = greenThumbDataset$Latitude), data = greenThumbDataset, alpha = 1, col = "purple")
mapPointsDA
ggsave(filename = "/home/niru/allInfoDir/otherWorkspaces/geophy/newyorkCondoGreen.png")


## most expensive

# colour
centerCondo <- c(mean(range(condoDataset$Longitude)), mean(range(condoDataset$Latitude)))

mapNYCondo <- get_googlemap(center = centerCondo, zoom = 12)
ggmap(mapNYCondo)
mapPointsDACondo <- ggmap(mapNY) + 
  geom_point(aes(x = condoDataset$Longitude, y = condoDataset$Latitude, color = condoDataset$MANHATTAN.CONDOMINIUM.PROPERTY.Market.Value.per.SqFt), data = condoDataset, alpha = .5) + 
  scale_color_gradient(high = "red4", low = "coral", name = "ValuePerSqFt") +
  geom_point(aes(x = greenThumbDataset$Longitude, y = greenThumbDataset$Latitude), data = greenThumbDataset, alpha = 1, col = "purple") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
mapPointsDACondo
# ggsave(filename = "/home/niru/allInfoDir/otherWorkspaces/geophy/newyorkCondoGreen.png")


range(condoDataset$MANHATTAN.CONDOMINIUM.PROPERTY.Market.Value.per.SqFt)
condoDataset$NTA <- gsub("[[:blank:]]+", "", condoDataset$NTA)
allCondoNTA <- table(condoDataset$NTA)[order(table(condoDataset$NTA), decreasing = T)]
ntaCostDF <- data.frame(condoDataset$NTA, condoDataset$MANHATTAN.CONDOMINIUM.PROPERTY.Market.Value.per.SqFt, stringsAsFactors = F)
avgNTACostDF <- data.frame(NTA = names(allCondoNTA), AverageValue = numeric(length = length(allCondoNTA)), stringsAsFactors = F)
for (i in 1:length(allCondoNTA)) {
  tmpRows <- ntaCostDF[grep(names(allCondoNTA)[i], ntaCostDF$condoDataset.NTA, fixed = T), ]
  avgNTACostDF$AverageValue[i] <- mean(tmpRows[[2]])
}
avgNTACostDF[which.max(avgNTACostDF$AverageValue), ]
priciestNTA <- avgNTACostDF$NTA[which.max(avgNTACostDF$AverageValue)]
priciestNTAData <- condoDataset[grep(priciestNTA, condoDataset$NTA, fixed = T), ]
mapPointsDAPriciest <- ggmap(mapNY) + 
  geom_point(aes(x = condoDataset$Longitude, y = condoDataset$Latitude), data = condoDataset, alpha = .5, col = "black") + 
  geom_point(aes(x = greenThumbDataset$Longitude, y = greenThumbDataset$Latitude), data = greenThumbDataset, alpha = .5, col = "darkgreen") +
  geom_point(aes(x = priciestNTAData$Longitude, y = priciestNTAData$Latitude), data = priciestNTAData, alpha = .5, col = "red")
mapPointsDAPriciest

## compare green thumb dataset and the condiminium dataset
greenThumbDataset$NTA <- gsub("[[:blank:]]+", "",  greenThumbDataset$NTA)
allGreenThumbNTA <- unique(greenThumbDataset$NTA)
commonNTA <- intersect(allGreenThumbNTA, names(allCondoNTA))
condoWithGreenThumb <- condoDataset[condoDataset$NTA %in% commonNTA, ]
dim(condoWithGreenThumb)
avgCostGreenThumb <- avgNTACostDF[avgNTACostDF$NTA %in% commonNTA, ]

colours <- ifelse(avgNTACostDF$NTA %in% commonNTA, "darkgreen", "black")
plot(avgNTACostDF$AverageValue, col = colours)

## rental income
valueIncome <- data.frame(value = condoDataset$MANHATTAN.CONDOMINIUM.PROPERTY.Market.Value.per.SqFt, 
                          income = condoDataset$MANHATTAN.CONDOMINIUM.PROPERTY.Gross.Income.per.SqFt)
str(cor(valueIncome[[1]], valueIncome[[2]]))
str(cor(valueIncome))
matplot((valueIncome[pos, ]), type = "l", lty = 1)
par(new = T)

y <- (valueIncome[[2]]/valueIncome[[1]])
plot(y[pos])
x <- valueIncome$value - valueIncome$income
pos <- order(x)
plot(x[pos], col = "green")
boxplot(t(valueIncome))
