#plot4.R
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
library(grid)

#select from SCC dataframe lines which contain 
#on column EI.Sector the word coal
SCCG<-SCC[grepl("coal",SCC$EI.Sector,ignore.case=TRUE),] %>%
      select(SCC) 


#prepare the data for graphic visualisation
#inner join between NEI and SCCG with SCC as key
#group data by year
#compute sum of emissions for each group and transform this values in kilotons
#variable year become factor
#converted Total to string and created a new column (named txt) with this values
dataG<-merge(NEI, SCCG, by="SCC") %>%
       group_by(year) %>% 
       summarise(Total=sum(Emissions, na.rm=TRUE)/1000) %>%
       mutate(year=factor(year)) %>%
       mutate(txt= sprintf("%.2f", round(Total,2)))

rm(NEI)
rm(SCC)

##open png device
##create file plot4.png
png(filename = "plot4.png", width = 480, height = 480, units = "px")

#draw the graphic
p1 <- ggplot(dataG, aes(x = year, y = Total, width=.7)) +
  geom_bar(stat = "identity", fill=c("orange","blue", "red", "green"))+
  geom_text(aes(label = dataG$txt, vjust = -0.25), size=5)+
  scale_y_continuous(breaks = seq(0, 800, 200), limits = c(0, 650), 
                     expand = c(0, 0))+
  labs(x = "Year",
       y = expression("Emissions of PM"[2.5]* " (kilotons)"))+
  ggtitle("Total emissions from Coal Combustion in USA (1999-2008)\n")+
  theme_bw()+
  
  #established the size and position of the text from graphics
  theme(axis.text=element_text(size=12), 
        axis.title.y=element_text(vjust=1.8),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) #set the margins the graph


p1

# Close the PNG file
dev.off()