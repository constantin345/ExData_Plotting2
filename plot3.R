#plot3.R
#copy the files summarySCC_PM25.rds and Source_Classification_Code.rds
#in your work directory
#set your work directory with setwd 

#setwd("E:/__DScienceJohnHopkins/ExploratoryDataAnalysis/tema2")

#load the data in R
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(grid)


#prepare the data for graphic visualisation
#filter data after fips
#group by data after type and year
#compute sum of emissions for each group
#variable year become factor
#converted Total to string and created a new column (named txt) with this values
dataG <- NEI %>% 
         filter(fips == "24510") %>%  
         group_by(type,year) %>% 
         summarise(Total=sum(Emissions, na.rm=TRUE)) %>%
         mutate(year=factor(year)) %>%
         mutate(txt=sprintf("%.1f", round(Total,1)))
        
##open png device
##create file plot3.png
png(filename = "plot3.png", width = 480, height = 480, units = "px")

#draw the graphic
p1 <- ggplot(dataG, aes(x = year, y = Total, width=.7)) +
      geom_bar(stat = "identity", fill=c("orange","blue", "red", "green"))+
      geom_text(aes(label = dataG$txt, vjust = -0.25), size=4)+
      facet_wrap( ~ type, ncol = 2, scales = "free")+
      scale_y_continuous(breaks = seq(0, 2500, 500), limits = c(0, 2500), 
                     expand = c(0, 0))+
      labs(x = "Year", y = expression("Emissions of PM"[2.5]* " (tons)"))+
      ggtitle(expression(
        "Total emissions from PM"[2.5]* " in Baltimore City (1999-2008)"))+
      theme_bw()+
  
     
      theme(panel.margin = unit(1, "lines"), #set the distance between panels
            axis.title.y=element_text(vjust=1.2), #set the position of text
            axis.text=element_text(size=11)) #set the size of text
p1

# Close the PNG file
dev.off()
