#plot6.R
#copy the files summarySCC_PM25.rds and Source_Classification_Code.rds
#in your work directory
#set your work directory with setwd 

#setwd("E:/__DScienceJohnHopkins/ExploratoryDataAnalysis/tema2")

#load the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
library(ggplot2)

#prepare the data for graphic visualisation
#select from NEI dataframe just columns fips, year, type, Emissions
#filter data after type and fips 
#creates column city
#group by city and year
#compute total of emissions for each group
#create column yearN 
#create column TotalN 
#compute the percentege of change (see the column percent)
#create column txt which it contains the values of percent converted to string
#remove lines which have value of percent NA
#period became column factor
#keeps just columns city, period, percent and text

#how lag function works
##for example if year is (1999, 2002, 2005, 2008 ), then lag(year)
#will be the vector (2002, 2005, 2008, NA)

dataG<-NEI %>%
  select(fips, year, type, Emissions) %>%
  filter(type == "ON-ROAD") %>%
  filter(fips == "24510" | fips == "06037") %>%
  mutate(city=ifelse(fips == "24510", "Baltimore","Los Angeles")) %>%
  group_by(city, year) %>% 
  summarise(Total=sum(Emissions, na.rm=TRUE)) %>%
  mutate(yearN=lead(year)) %>%
  mutate(TotalN=lead(Total)) %>%
  mutate(percent=(TotalN-Total)/Total*100) %>% 
  mutate(period=paste0(year," \nto\n ", yearN)) %>% 
  mutate(txt=sprintf("%.2f%%", round(percent,2))) %>% 
  filter(is.na(percent)==FALSE) %>% 
  mutate(period=factor(period)) %>% 
  select(city, period, percent, txt)


##open png device
##create file plot6.png
png(filename = "plot6.png", width = 480, height = 480, units = "px")


#draw the graphic
p1 <- ggplot(dataG, aes(x = period, y = percent))+
      geom_bar(stat = "identity", position='identity',
              fill = c("orange","blue", "red"))+
      geom_text(aes(label = dataG$txt,  
                    vjust = ifelse(percent >= 0, -0.25, 1)))+
      geom_hline(yintercept=0)+
      facet_wrap( ~ city, ncol = 2, scales = "free")+
      scale_y_continuous(breaks = seq(-75, 15,15), 
                         limits = c(-75, 15), expand = c(0, 0))+
  labs(x = "Period of time",
       y = "Differences between total emissions of PM2.5\n for two period of time (in percents)"
       )+
  
  ggtitle("Change in Total Emissions from motor vehicles \nin Baltimore vs Los Angeles\n")+
  
      #established the size and position of the text from graphics
      theme(axis.text.x=element_text(size=11),
            axis.text.y=element_text(size=11),
            axis.title.y=element_text(size=12, vjust=1.4, hjust=0.5),
            axis.title.x=element_text(size=12, vjust=0.1),
            strip.text=element_text(size=12))
           
p1

# Close the PNG file
dev.off()

