library(WDI)
new_wdi_cache <- WDIcache()

#searching for indicators whose name contain a keyword 'area'
indicators = WDIsearch("area", field="name", cache = new_wdi_cache)

View(indicators)

#to get the land areas in sq. km for all countries in the year 2018
landarea <- WDI(country = "all",
                indicator = "AG.LND.TOTL.K2",
                start = 2018, end = 2018,
                cache = new_wdi_cache,
                extra = TRUE)

View(landarea)

#-------------------BAR CHARTS-------------------------#

#basic bar chart
library(ggplot2)
ggplot(landarea, aes(region)) + geom_bar() + 
         labs ( x = "Regions",
                y = "Number of countries",
                title = "Number of countries from each region in the world",
                caption = "The WDI Land area indicator" )

#removing bars corresponding to 'Aggregates' and 'NA'
landarea <- landarea[!is.na(landarea$region) & landarea$region != 'Aggregates', ]

ggplot(landarea, aes(region))+geom_bar()+
  labs ( x = "Regions",
         y = "Number of countries",
         title = "Number of countries from each region in the world",
         caption = "The WDI Land area indicator" )

#making the bars thinner
ggplot(landarea, aes(region))+geom_bar(width = 0.5)+
  labs(x="Regions", 
       y="Number of countries",
       title="Number of countries from each region in the world",
       caption= "The WDI Land Area Indicator")

#Group rows by 'region' and counting the number of rows for each group
library(tidyverse)

df<- landarea %>% group_by(region) %>% summarise(count=n())

ggplot(df, aes(region, count))+geom_bar(stat="identity")+
  labs(x="Regions", y="Number of countries",
       title="Number of countries from each region in the world",
       caption="The WDI Land Area Indicator")

#First we select only the countries in the 'South Asia' region
southasia <- landarea[landarea$region=="South Asia",]
#Next create the plot. We ask ggplot to use 'country' as the categories, and the land
#area (the column AG.LND.TOTL.K2) as the statistic for the height of the bar
ggplot(southasia, aes(country, AG.LND.TOTL.K2))+geom_bar(stat="identity")+
  labs(x="Countries", y="Land area in Sq. Km",
       title="Land areas of South Asian countries",
       caption="The WDI Land Area Indicator")


#clustered and stacked bar chart 
#to create stacked bar charts, we only need to map the variable to the parameter fill
ggplot(landarea, aes(region)) + geom_bar(aes(fill=income)) + 
  labs(x="Regions", y="Number of countries",
       title="Number of countries from each region in the world by income level",
       caption="The WDI Land Area Indicator")

#to create a clustered bar chart rather than a stacked bar chart, we add the position parameter to geom_bar
ggplot(landarea, aes(region)) + geom_bar(aes(fill=income), position = "dodge" ) +
  labs(x="Regions", y="Number of countries",
       title="Number of countries from each region in the world by income level",
       caption="The WDI Land Area Indicator")

#EXERCISE

#use a different indicator : AG.LND.FRST.K2 : bar chart displaying the forest area for countries in a region of your choice
forestarea <- WDI(country = "all",
                indicator = "AG.LND.FRST.K2",
                start = 2020, end = 2020,
                cache = new_wdi_cache,
                extra = TRUE)
View(forestarea)

forestarea <- forestarea[!is.na(forestarea$region) & forestarea$region != 'Aggregates', ]

southasia <- forestarea[forestarea$region=="South Asia",]
View(southasia)
africa <- forestarea[forestarea$region== "Sub-Saharan Africa", ]

#we ask ggplot to use 'country' as the categories, and the forest area
# the column (indicator) as the statistic for the height of the bar
ggplot(southasia, aes(country, AG.LND.FRST.K2)) + geom_bar(stat = "identity") + 
  labs(x = "Regions",
       y = "Forest area",
       title = "Forest areas of South Asian countries",
       caption = "The WDI Forest area indicator")

#create a stacked bar chart showing the number of countries by different
#income levels, sub-divided by region

ggplot(forestarea, aes(income)) + geom_bar(aes(fill=region)) + 
    labs(x = "Income",
       y = "Number of countries",
       title = "Number of countries by different income levels", subtitle = "sub-divided by region",
       caption = "The WDI Forest area indicator")

#Transform the stacked bar to a clustered bar chart 
ggplot(forestarea, aes(income)) + geom_bar(aes(fill=region), position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Income",
       y = "Number of countries",
       title = "Number of countries by different income levels", subtitle = "sub-divided by region",
       caption = "The WDI Forest area indicator")


#-----------------THEMES------------------------#

#Overlapping can be solved using the theme function 
ggplot(landarea, aes(region))+geom_bar()+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Regions", y="Number of countries",
       title="Number of countries from each region in the world",
       caption="The WDI Land Area Indicator")

#theme_bw changes the background of the plot and the axes
ggplot(landarea, aes(region))+geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Regions", y="Number of countries",
       title="Number of countries from each region in the world",
       caption="The WDI Land Area Indicator")
#NOTE : Plot modifications at the end of the chain of additions can overwrite previous changes


#EXERCISE : Apply 3 different themes and apply it to the clustered bar plot created earlier

ggplot(forestarea, aes(income)) + geom_bar(aes(fill=region), position = "dodge")+
  theme_dark() +
  theme_minimal() + 
  theme_linedraw() +  #linedraw overwrites dark and minimal
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Income",
       y = "Number of countries",
       title = "Number of countries by different income levels", subtitle = "sub-divided by region",
       caption = "The WDI Forest area indicator")


#------------PIE CHARTS-----------------#
#modifying stacked bar into a pie chart
ggplot(landarea, aes(factor(1), fill= region))+geom_bar()+
  coord_polar(theta="y")


#removing elements like the axis label using the theme function
#we can remove the elements using the function element_blank()

ggplot(landarea, aes(factor(1), fill = region)) + geom_bar()+
  coord_polar(theta = "y") +
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Regions",
       title = "Proportion of countries by region",
       caption = "WDI Land Area Indicator")

#creating a pie chart showing the proportion of land area in south asia


ggplot(southasia, aes(x="", y=AG.LND.TOTL.K2, fill= country))+geom_bar(stat="identity")+
  coord_polar(theta="y")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x=NULL, y=NULL, fill="Country",
       title="Proportion of land area of countries in South Asia (Sq km)",
       caption="WDI Land Area Indicator")


install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
df<- landarea %>% group_by(region) %>% summarise(count=n())
ggplot(df, aes(fill=region, values=count)) + geom_waffle()

df<- landarea %>% group_by(region) %>% summarise(count=n())
ggplot(df, aes(fill=region, values=count)) + geom_waffle(n_rows=7, colour="white")+
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) + coord_equal() +
  labs(x=NULL, y=NULL, fill="Regions",
       title="Proportion of countries by region",
       caption="WDI Land Area Indicator")

#EXERCISE : create a pie chart and a waffle chart based on the income levels 
# create a pie chart showing the proportion of forest areas for countrie in the region  you chose before
