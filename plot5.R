#plot5.R
#copy the files summarySCC_PM25.rds and Source_Classification_Code.rds
#in your work directory
#set your work directory with setwd 

#setwd("E:/__DScienceJohnHopkins/ExploratoryDataAnalysis/tema2")

#load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)
library(ggthemes)

#prepare the data for graphic visualisation
#filter data after type and fips
#group data after year
#compute sum of emissions for each group
#variable year become factor
#converted Total to string and created a new column (named txt) with this values
dataG<-NEI %>%
       filter(type=="ON-ROAD" & fips == "24510") %>%
       group_by(year) %>% 
       summarise(Total=sum(Emissions, na.rm=TRUE)) %>%
       mutate(year=factor(year)) %>% 
       mutate(txt=sprintf("%.2f", round(Total,2)))

#open png device
#create file plot5.png
png(filename = "plot5.png", width = 480, height = 480, units = "px")

#draw the graphic
p1 <- ggplot(dataG, aes(x = year, y = Total, width=.7)) +
      geom_bar(stat = "identity", fill=c("orange","blue", "red", "green"))+
      geom_text(aes(label = dataG$txt, vjust = -0.25), size=5)+
      scale_y_continuous(breaks = seq(0, 400, 100), limits = c(0, 400), 
                     expand = c(0, 0))+
      labs(x = "Year", 
           y = expression("Emissions of PM"[2.5]* " (tons)")
           )+
  
      ggtitle("Total emissions from Motor Vehicle in Baltimore City (1999-2008)\n")+
    
      theme_bw()+
  
      #established the position of text
      theme(axis.title.y=element_text(vjust=1.2))

p1

# Close the PNG file
dev.off()
