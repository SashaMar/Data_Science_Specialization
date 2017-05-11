#Eploratory Data Analysis - Assignment 2, Plot 1
# Author: Sasa Marjanovic
# date: May 4th 2017 
# 
# Question 1: Have total emissions from PM2.5 decreased in the United States
#             from 1999 to 2008? Using the base plotting system, make a plot 
#             showing the total PM2.5 emission from all sources for each of the 
#             years 1999, 2002, 2005, and 2008.

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Calculating total emissions over years
Total.Emissions.By.Year <- with(NEI, tapply(Emissions, year, sum))
Total.Emissions.By.Year.M.Tons <- 
    apply(Total.Emissions.By.Year, 1, function(x) x/1000000)
years <- c(1999, 2002, 2005, 2008)

# Opening a PNG Graphics Device
png(filename = "Plot1.png")

# Plot 1
plot(years, Total.Emissions.By.Year.M.Tons, xlim = c(1998, 2009), 
     ylim = c(3, 8), pch = 19, cex = 2, yaxp = c(3, 8, 5),
     ylab = "Sum of all emissions (millions of tons)", xlab = "Year",
     main = "Total PM2.5 emission: 1999 - 2008")
lines(years, Total.Emissions.By.Year.M.Tons, lty = 2)

# Closing the graphics device
dev.off()
