install.packages("tidyverse")
library(tidyverse)

t<-seq(from=0, to=10, by=0.1)
y<-sin(t)

df <- data.frame(t,y)
ggplot(df, aes(x = t, y = y)) +
  geom_point()

#The data given, should be a data.frame or tibble because if not it will be converted to one and that could create errors
#The second argument is mapping. The mapping can be specified on the ggplot call, or it can be set in each layer. You can also create the same plot using

ggplot(df) +
  geom_point(aes(x=t, y=y))

#aes() defines the aesthetic mapping

library(WDI)
new_wdi_cache <- WDIcache()

gdp_capita <- WDI(country="all",
                  indicator = "NY.GDP.PCAP.KD",
                  start = 2000,
                  end = 2020,
                  cache = new_wdi_cache)

#only adding eu countries from the list of all countries

eu_countries <- subset(gdp_capita,
                       country=="United Kingdom" |
                         country=="France" |
                         country=="Spain" |
                         country =="Italy" |
                         country =="Netherlands")
View(eu_countries)

#expanding the list of countries : doing it directly from the data set across multiple indicators

country_data <- WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                    indicator = c("NY.GDP.PCAP.KD","NY.GDP.PCAP.KD.ZG","SP.POP.TOTL","SP.DYN.LE00.IN"),
                    start = 2000, end = 2020,
                    extra=TRUE,
                    cache = new_wdi_cache)
View(country_data)
#extra = true captures more data about the countries

#scatter plot to compare the displacement of fuel economy on the highway
ggplot(country_data) +
  geom_point(aes(year, NY.GDP.PCAP.KD))

#EXERCISE 
#1) Plot that displays GDP per capita growth for the countries for 2009-2020

WDIsearch("GDP per capita", cache = new_wdi_cache)

View(gdp_capita) #we already made this dataframe in the beginning 

ggplot(gdp_capita) +
  geom_point(aes(year, NY.GDP.PCAP.KD))

#plotting for 2009-2020 

gdp_capita2 <- WDI(country="all",
                   indicator = "NY.GDP.PCAP.KD",
                   start = 2009,
                   end = 2020,
                   cache = new_wdi_cache)

ggplot(gdp_capita2) +
  geom_point(aes(year, NY.GDP.PCAP.KD))

#2) Plot that displays unemployment (% of total unemployment) for the countries from 2009-2020.

WDIsearch("unemployment", cache = new_wdi_cache)

unemployment <- WDI(indicator = "UNEMPSA_")
View(unemployment)

unemployment <- WDI(country = "all",
                    indicator = "UNEMPSA_",
                    start = 2009,
                    end = 2020,
                    cache = new_wdi_cache)

#to plot it now 
ggplot(unemployment) +
  geom_point(aes(year, UNEMPSA_))

#Question : How do you which country each plot belongs to?

#------CHANGING COLOUR, SHAPES AND SIZES-------#

ggplot(country_data, aes(year, NY.GDP.PCAP.KD, colour = country)) +
  geom_point()

ggplot(country_data,aes(year,NY.GDP.PCAP.KD,colour=country,size=NY.GDP.PCAP.KD.ZG)) +
  geom_point()

#two variables; y: GDP per capita and the other is the size of the points 
#the points are smaller when the GDP values fall and vice versa

ggplot(country_data, aes(year,
                         NY.GDP.PCAP.KD,colour = country,
                         size=NY.GDP.PCAP.KD.ZG,
                         shape = region)) +
  geom_point()

#defining a very particular size and shape 
ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(colour = "blue", size = 5, shape = 4)

#notice that parameters that are fixed are outside the aes function, while
#parameters that vary based on variable are inside the aes function

##---MODIFYING THE AEXS AND THE LEGEND---##

#labs function are used to modify the labels of the plot

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_point() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)", colour="Country" )

#function xlim and ylim can modify the range of values shown on each axis.
#You can hide the data points if they are outside the range of values

ggplot(country_data, aes(year, NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_point() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)", colour="Country" )+
  xlim(2000,2016)+
  ylim(-10,10)


#-------SCATTERPLOT VS LINE PLOT--------#

#more useful to denote line plots for WDI to denote trends over time 

ggplot(country_data, aes(year,NY.GDP.PCAP.KD.ZG, colour = country)) +
  geom_line() +
  labs(x = "Year", y="GDP Per Capita Growth (annual %)", colour="Country")

#we have used geom_line instead of geom_point



#-----------------FINAL EXERCISE------------------------#

View(country_data)


ggplot(country_data, aes(income,SP.DYN.LE00.IN, colour = country)) +
  geom_line() +
  labs(x = "income", y="Life expectancy at birth, total(years)", colour="Country")


ggplot(country_data, aes(year,SP.DYN.LE00.IN, colour = country, size=income)) +
  geom_point() +
  labs(x = "years", y="Life expectancy at birth, total(years)", shape="Country")

