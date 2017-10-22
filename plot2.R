#plot2.R
#copy the files summarySCC_PM25.rds and Source_Classification_Code.rds
#in your work directory
#set your work directory with setwd 

#setwd("E:/__DScienceJohnHopkins/ExploratoryDataAnalysis/tema2")

#load the data in R
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)

#prepare data for graphic visualisation
#keep just PM2.5 emmisions from Baltimore (fips == "24510"()
#break the dataset down into groups (after year)
#computes sum of emmisions for each group
#creates a new column named txt, which it contains the value 
#from column Total converted to string
dataG <- NEI %>% 
         filter(fips == "24510") %>%
         group_by(year) %>% 
         summarise(Total=sum(Emissions, na.rm=TRUE)) %>%
         mutate(txt=sprintf("%.2f", round(Total,2)))

##open png device
##create file plot2.png
png(width=480, height=480, filename="plot2.png")

#set the margins the graph
par(mar=c(5,5,4,1))

#create and save a barplot
bp<-barplot(dataG$Total, 
        col=c("orange","blue", "red", "green"),
        xlab="Year", 
        ylab=expression("Emissions of PM"[2.5]* " (tons)"),
        cex.main=1.2,
        cex.lab=1.2,
        cex.axis=1.2)

#put the text from column dataG$txt on each bar from barplot
text(bp, dataG$Total,  labels=dataG$txt,pos=3,xpd=NA)  

#write text into the top margin of the graph
mtext(side=3,
      expression("Total emissions from PM"[2.5]* " in Baltimore City (1999-2008)"),
      line=2,
      cex=1.2)

#creates axis x and put labels on it
axis(1, at=bp, labels=dataG$year)

# Close the PNG file
dev.off()
