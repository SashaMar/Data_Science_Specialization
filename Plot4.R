#Eploratory Data Analysis - Assignment 2
# Author: Sasa Marjanovic
# date: May 4th 2017 
# 
# Question 4: Across the United States, how have emissions from coal 
#             combustion-related sources changed from 1999-2008?

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Finding the coal combustion related: 
#   All that have a word "Coal" in SCC table, plus come from groups that begin
#   with 1 (external combustion), 20 (internal comb. - point), or 
#   21 (internal combustion - nonpoint). Rest of 2X and other codes are exlcuded
#   because they refer to other coal related activities/products.
#   More info: https://ofmpub.epa.gov/sccsearch/docs/SCC-IntroToSCCs.pdf
#   
is.coal.related <- apply(SCC, 2, function(x) 
                                 grepl("Coal", x, fixed = TRUE)) %>% 
                   apply(1, function(x) any(x, na.rm = TRUE))

coal.related.SCC <- SCC[which(is.coal.related),]

is.coal.comb <- startsWith(as.character(SCC[is.coal.related,1]), "1") |
                startsWith(as.character(SCC[is.coal.related,1]), "20") |
                startsWith(as.character(SCC[is.coal.related,1]), "21") 

coal.comb.SCC <- coal.related.SCC[which(is.coal.comb),]

# Subesetting an calculating emission from coal comb. - related sources, over years
coal.comb.NEI <- NEI[NEI$SCC %in% coal.comb.SCC$SCC,]

Total.Emissions.By.Year <- with(coal.comb.NEI, tapply(Emissions, year, sum))
Total.Emissions.By.Year.K.Tons <- 
    apply(Total.Emissions.By.Year, 1, function(x) x/1000)
years <- c(1999, 2002, 2005, 2008)

# Opening a PNG Graphics Device
png(filename = "Plot4.png")

plot(years, Total.Emissions.By.Year.K.Tons, xlim = c(1998, 2009), 
     ylim = c(300, 600), pch = 19, cex = 2, yaxp = c(300, 600, 5),
     ylab = "Sum of all emissions (thousands of tons)", xlab = "Year",
     main = "Total Coal Combustion PM2.5 emission: 1999 - 2008")
lines(years, Total.Emissions.By.Year.K.Tons, lty = 2)

# Closing the graphics device
dev.off()
