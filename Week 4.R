library(WDI)
library(tidyverse)
new_wdi_cache <- WDIcache()

#calling information using the WDI function
country_data <-WDI(country=c("GB","FR","ES","IT","NL", "CN","AE","IN","JO","US"),
                   indicator =c("NY.GDP.PCAP.KD","NY.GDP.PCAP.KD.ZG",
                                "SP.POP.TOTL","SP.DYN.LE00.IN"),
                   start = 2000, end = 2019,
                   extra=TRUE,
                   cache = new_wdi_cache)

#-------COLOUR-------#
#colour is specified using hexadecimal code : #RRGGBB

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(colour = "#FF0000") +
  labs(x="Year", y="GDP Per Capita Growth (annual %)")

#R contain different functions to create colours using different encodings
#rgb() and hsv() can be used whenever you would use a colour string

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(colour = rgb(0.8,0.2,0.5)) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)")


#EXERCISE 1 : scatterplot for life expectancy (Y axis) and year (X axis)

ggplot(country_data, aes(year, SP.DYN.LE00.IN))+
  geom_point(colour = "#82b446" ) +
  labs(x="Year", y="Life Expectancy")

?rgb()

#using hsv() and rgb()
ggplot(country_data, aes(year, SP.DYN.LE00.IN))+
  geom_point(colour = hsv(0.13, 0.18, 0.7) ) +
  labs(x="Year", y="Life Expectancy")

ggplot(country_data, aes(year, SP.DYN.LE00.IN))+
  geom_point(colour = rgb(0.13, 0.18, 0.7) ) +
  labs(x="Year", y="Life Expectancy")


#-----------COLOUR SCHEMES--------#

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(aes(colour =country)) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Country")   
#This plot provides a default colour scheme : we can modify it using the functuion scale_colour_brewer()

?scale_color_brewer

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(aes(colour =income)) +
  scale_colour_brewer(palette='Dark2') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")

#trying different palettes : 

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) )+
  geom_point(aes(colour =income)) +
  scale_colour_brewer(palette='Spectral') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")

#Defining your own colour scheme

#FUNCTIONS : 
#scale_colour_gradient() : specifying colors at the beginning and at the end
#scale_colour_gradient2() : can also specify the middle colour
#scale_colour_gradientn() : can generate a gradient based on any number of colors 

?scale_colour_gradient()

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradient(low='blue', high='#00FF00') +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")

#specifying low, mod and high:scale_colour_gradient2()
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradient2(midpoint=72.5, low='blue', mid='red', high='green') +#here we specify midpoint
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")

#generating gradient based on n number of colors: scale_colour_gradientn()
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_gradientn(colours=c('purple', 'pink', 'red', 'orange', 'blue')) +#here we specify combination of colours
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")


#viridis is a colour scheme designed with accessibility : 

#scale_colour_viridis_d() for discrete variables
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =income)) +
  scale_colour_viridis_d() +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Income\nlevel")

#scale_colour-viridis_c() for continuous values
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy")

#EXERCISE 2 : GDP (X-axis), life expectancy (Y-axis), colour mapped to income

#scale_color_hue() : evenly spaced colour for discrete data

ggplot(country_data,aes(NY.GDP.PCAP.KD.ZG, SP.DYN.LE00.IN) ) +
  geom_point(aes(colour =income)) +
  scale_colour_hue(h = c(0, 90), l = 70, c = 200)+
  labs(x="GDP Per Capita Growth (annual %)", y="Life Expectancy",
       colour="income")

#scale_color_discrete()
ggplot(country_data,aes(NY.GDP.PCAP.KD.ZG, SP.DYN.LE00.IN) ) +
  geom_point(aes(colour =income)) +
  scale_color_discrete()+
  labs(x="GDP Per Capita Growth (annual %)", y="Life Expectancy",
       colour="income")

#scale_color_grey()
ggplot(country_data,aes(NY.GDP.PCAP.KD.ZG, SP.DYN.LE00.IN) ) +
  geom_point(aes(colour =income)) +
  scale_color_grey()+
  labs(x="GDP Per Capita Growth (annual %)", y="Life Expectancy",
       colour="income")


#----------TITLES----------#

#setting the title and subtitle using the labs() function 

ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", 
       y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', 
       subtitle = 'Test subtitle')

#can also add comments, useful to add the source of the data
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", 
       y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', 
       subtitle = 'Test subtitle',
       caption='World Development Indicators, World Bank')

#using the tag parameter we can add numbers or letters to the upper corner if there are series of plots
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Test title', subtitle = 'Test subtitle',
       caption='World Development Indicators, World Bank',
       tag='A')

#EXERCISE 3
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =SP.DYN.LE00.IN)) +
  scale_colour_viridis_c()+
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Life\nexpectancy",
       title='Correlation of GDP and Life Expectancy', subtitle = 'This plot denotes the effect of GDP on the avg life\n expectancy across countries',
       caption='Creator: Swathi',
       tag='A')

#----------FACETING----------#

#faceting is  tool available in ggplot2 to create subplots based on featured of the data

#faceting along X-axis
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(. ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       colour="Country",
       title='Faceting test', subtitle = 'Facet on the X axis',
       caption='World Development Indicators, World Bank')

#faceting along y-axis
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ .) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on the Y axis',
       colour="Country",
       caption='World Development Indicators, World Bank')

#faceting on both axes 
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on both axes',
       colour="Country",
       caption='World Development Indicators, World Bank')


#when the values are very close to each other you can modify which values are shown on the axis
#scale_x_continuous and scale_y_continuous 
#function reduces the number of values shown the axis to create a cleaner plot
ggplot(country_data,aes(year, NY.GDP.PCAP.KD.ZG) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ income) +
  labs(x="Year", y="GDP Per Capita Growth (annual %)",
       title='Faceting test', subtitle = 'Facet on both axes',
       colour="Country",
       caption='World Development Indicators, World Bank')+
  scale_x_continuous(breaks=seq(2000,2020,10))

#----SAVING THE PLOT TO A FILE-----#
filename<-"test_image.png"
ggsave(filename)


#--------FINAL EXERCISE-------#

ggplot(country_data,aes(SP.DYN.LE00.IN, SP.POP.TOTL) ) +
  geom_point(aes(colour =country)) +
  facet_grid(region ~ income) +
  labs(x="Life Expectancy", y="Population",
       title='Faceting test', subtitle = 'Facet on both axes',
       colour="Country",
       caption='World Development Indicators, World Bank')+
  #scale_x_continuous(breaks=seq(2000,2020,10))
  scale_y_continuous(trans="log")

?scale_x_continuous

