#Eploratory Data Analysis - Assignment 2
# Author: Sasa Marjanovic
# date: May 4th 2017 

library(ggplot2)
library(gridExtra)

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subsetting data for Baltimore City and LA
NEI.Baltimore <- subset(NEI, fips == "24510")
NEI.LA <- subset(NEI, fips == "06037")

# Filtering out data that are related to motor vehicle pollution. 
# Criteria was that the word "Vehicle" appears in the SCC table. 
# This does not include vessels, but include all other motor vehicles, 
# onroad and nonroad, including industrial equipment, lawn and garden equipment, etc.
is.motor.vechicle <- apply(SCC, 2, function(x) 
                                   grepl("Vehicle", x, fixed = TRUE)) %>% 
                     apply(1, function(x) any(x, na.rm = TRUE))
motor.vehicle.SCC <- SCC[which(is.motor.vechicle),]
motor.vehicle.NEI.Baltimore <- NEI.Baltimore[NEI.Baltimore$SCC %in% 
                                                 motor.vehicle.SCC$SCC,]
motor.vehicle.NEI.LA <- NEI.LA[NEI.LA$SCC %in% motor.vehicle.SCC$SCC, ]

# Calculating total emmisions by year, from motor vehicles, 
# for Baltimore City and LA
Total.Emissions.By.Year.Baltimore <- 
    with(motor.vehicle.NEI.Baltimore, tapply(Emissions, year, sum))
Total.Emissions.By.Year.LA <- 
    with(motor.vehicle.NEI.LA, tapply(Emissions, year, sum))
years <- c(1999, 2002, 2005, 2008)

# Putting that into a dataframe
motor.vehicle.emissions <- 
  data.frame(emissions = c(as.vector(Total.Emissions.By.Year.Baltimore), as.vector(Total.Emissions.By.Year.LA)),
             year = as.factor(rep(years, 2)),
             city = rep(c("Baltimore", "LA"), each = 4))


# Calculating year-over-year change in emmisions for Baltimore City and LA, 
# and putting the results into a dataframe

x <- Total.Emissions.By.Year.Baltimore
y <- Total.Emissions.By.Year.LA
YoY.Baltimore <- vector()
YoY.LA <- vector()

for(i in 1:3){
  YoY.Baltimore[i] <- (x[i+1]-x[i]) / x[i] * 100
  YoY.LA[i] <- (y[i+1]-y[i]) / y[i] * 100
}
YoY <- data.frame(yoy = as.factor(rep(c("Y2/Y1", "Y3/Y2", "Y4/Y3"), 2)),
                  emission.change = c(YoY.Baltimore, YoY.LA),
                  city = rep(c("Baltimore", "LA"), each = 3))

# Opening a PNG Graphics Device
png(filename = "Plot6.png", width = 1000, pointsize = 16)

# Plotting three plots: 1. Total emissions from motor vehicles in Baltimore City
#                       2. Total emissions from motor vehicles in Los Angeles
#                       3. Year-on-year relative change of emissions from motor vehicles for both cities

# Plots 1 and 2
p12 <- ggplot(motor.vehicle.emissions, aes(year, emissions)) +
       geom_bar(aes(fill = year), stat = "identity") +
       facet_grid(city~., scales = "free") +
       xlab("Year") + ylab("PM2.5 Emission from Motor Vehicles (tons)") +
       theme(text = element_text(size = 14))

# Plot 3 Year-on-year relative change of emissions from motor vehicles for both cities
p3 <- ggplot(YoY, aes(yoy, emission.change)) +
      geom_point(aes(color = city), size = 5) +
      xlab("") + ylab("Relative change of emission from motor vehicles (%)") +
      theme(text = element_text(size = 14)) +
      scale_color_brewer(palette="Dark2")

grid.arrange(p12, p3, ncol = 2, top = "Comparisson of change of emissions from motor vehicles in Baltimore City and LA")

# Closing the graphics device
dev.off()
    
