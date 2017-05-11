#Eploratory Data Analysis - Assignment 2
# Author: Sasa Marjanovic
# date: May 4th 2017 
# 
# Question 5: How have emissions from motor vehicle sources changed 
#             from 1999-2008 in Baltimore City?

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subsetting data for Baltimore City 
NEI.Baltimore <- subset(NEI, fips == "24510")

# Filtering out SCC codes related to motor vehicles
is.motor.vechicle <- apply(SCC, 2, function(x) 
                                   grepl("Vehicle", x, fixed = TRUE)) %>% 
                     apply(1, function(x) any(x, na.rm = TRUE))
motor.vehicle.SCC <- SCC[which(is.motor.vechicle),]

# Subsetting and calculating data 
motor.vehicle.NEI <- NEI.Baltimore[NEI.Baltimore$SCC %in% motor.vehicle.SCC$SCC,]
Total.Emissions.By.Year <- with(motor.vehicle.NEI, tapply(Emissions, year, sum))
years <- c(1999, 2002, 2005, 2008)

# Opening a PNG Graphics Device
png(filename = "Plot5.png")

plot(years, Total.Emissions.By.Year, xlim = c(1998, 2009), ylim = c(100, 500),
     pch = 19, cex = 2, yaxp = c(100, 500, 4),
     ylab = "Sum of all emissions (tons)", xlab = "Year",
     main = "Total PM2.5 emission from motor vehicels \nin Baltimore City: 1999 - 2008")
lines(years, Total.Emissions.By.Year, lty = 2)

# Closing the graphics device
dev.off()

