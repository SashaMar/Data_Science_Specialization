#Eploratory Data Analysis - Assignment 2
# Author: Sasa Marjanovic
# date: May 4th 2017 
# 
# Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
#             Maryland (fips == "24510") from 1999 to 2008? Use the base 
#             plotting system to make a plot answering this question.

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subsetting and calculating emissions for Baltimore City
NEI.Baltimore <- subset(NEI, fips == "24510")
Total.Emissions.By.Year.Baltimore <- 
    with(NEI.Baltimore, tapply(Emissions, year, sum))
years <- c(1999, 2002, 2005, 2008)

# Opening a PNG Graphics Device
png(filename = "Plot2.png")

# Plot2
plot(years, Total.Emissions.By.Year.Baltimore, xlim = c(1998, 2009), 
     ylim = c(1500, 3500), pch = 19, cex = 2, yaxp = c(1500, 3500, 5),
     ylab = "Sum of all emissions (tons)", xlab = "Year",
     main = "Total PM2.5 emission in Baltimore City: 1999 - 2008")
lines(years, Total.Emissions.By.Year.Baltimore, lty = 2)

# Closing the graphics device
dev.off()
