#Eploratory Data Analysis - Assignment 2
# Author: Sasa Marjanovic
# date: May 4th 2017 
# 
# Question 3: Of the four types of sources indicated by the type (point, 
#             nonpoint, onroad, nonroad) variable, which of these four sources 
#             have seen decreases in emissions from 1999-2008 for Baltimore City? 
#             Which have seen increases in emissions from 1999-2008? 
#             Use the ggplot2 plotting system to make a plot answer this question.
library(dplyr) 
library(ggplot2)

# Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset and summarize data 
NEI.Baltimore <- subset(NEI, fips == "24510")
NEI.Baltimore.Summary <- NEI.Baltimore %>% 
                         group_by(type, year) %>% 
                         summarise(total.emission = sum(Emissions), 
                                   avg.emission = mean(Emissions))
NEI.Baltimore.Summary$year <- as.factor(NEI.Baltimore.Summary$year)

# Opening a PNG Graphics Device
png(filename = "Plot3.png", width = 1000, height = 480)

# Total Emission
g <- ggplot(NEI.Baltimore.Summary, aes(year, total.emission)) + 
     geom_bar(stat = "identity", aes(fill = year)) + 
     facet_grid(.~type, scales = "free") +
     ggtitle("Total emission of PM2.5 in Baltimore City by Source Type") +
     xlab("Year") + ylab("Total emission of PM2.5 (tons)") + theme_bw() +
     theme(plot.title = element_text(hjust = 0.5))

print(g)

# Closing the graphics device
dev.off()
