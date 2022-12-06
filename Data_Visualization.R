library(WDI)
library(tidyverse)
library(ggplot2)
library(gridExtra)

new_wdi_cache <- WDIcache()

View(WDIsearch("water", cache = new_wdi_cache) ) 

sanitation <- WDI(indicator = c(#"HOU.STA.ACSN.ZS",
                                "SH.STA.BASS.ZS", #People using at least basic sanitation services (% of population)
                               'SH.H2O.BASW.ZS', #People using at least basic drinking water services (% of population)
                               "SH.STA.SMSS.ZS", #People using safely managed sanitation services (% of population)
                               "SH.H2O.SMDW.ZS",  #People using safely managed drinking water services (% of population)
                               "SH.STA.WASH.P5" #Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population
                            ),
                    start = 2016,
                    end = 2020,
                  extra = TRUE,
                  cache = new_wdi_cache)

#-------------checking for other years other than 2016
test <- WDI(indicator = c(#"HOU.STA.ACSN.ZS",
  "SH.STA.BASS.ZS", #People using at least basic sanitation services (% of population)
  'SH.H2O.BASW.ZS' #People using at least basic drinking water services (% of population)
  #"SH.STA.SMSS.ZS", #People using safely managed sanitation services (% of population)
  #"SH.H2O.SMDW.ZS"  #People using safely managed drinking water services (% of population)
  #"SH.STA.WASH.P5" #Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population
),
#start = 2018,
#end = 2020,
extra = TRUE,
cache = new_wdi_cache)

test <- test[ test$region != 'Aggregates',]
test <- na.omit(test)
View(test)

ggplot(test, aes(SH.STA.BASS.ZS, year))+
  geom_point(size=7, colour=rgb(0.2,0.8,0,0.25))+
  labs(y="year", x="People using at least basic sanitation services (% of population)",
       title="Access to Basic Sanitation",
       caption = 'WDI dataset'
  )

sanitation_access <- ggplot(test, aes(SH.STA.BASS.ZS))+
  geom_histogram(bins=10, fill="lightgreen")+ 
  labs(x="People using at least basic sanitation services (% of population)",
       y="Frequency", 
       caption="WDI dataset", 
       title='Sanitation')

water_access <- ggplot(test, aes(SH.H2O.BASW.ZS))+
  geom_histogram(bins=10, fill="plum")+ 
  labs(x="People using at least basic drinking water services (% of population))",
       y="Frequency", 
       caption="WDI dataset", 
       title='Water')

grid.arrange(sanitation_access, water_access, ncol = 1 )
#Limitation : SH.STA.WASH.P5 : Mortality Rate is only available for the year 2016 

#Removing aggregates and NA data
sanitation <- sanitation[sanitation$region != 'Aggregates',]
sanitation <- na.omit(sanitation)

View(sanitation)

rm(sanitation2)


ggplot(sanitation, aes(region, SH.STA.BASS.ZS) ) +
  geom_point(aes(colour = income), size = 7) +
  scale_colour_viridis_d() +
  labs(x="Region", y="People using at least basic sanitation services (% of population)",
       colour="Income\nlevel")

#Analysis of Mortality Rate
sanitation_mortality <- ggplot(sanitation, aes(SH.STA.BASS.ZS, SH.STA.WASH.P5))+
  geom_point(size=7, colour=rgb(0.2,0.8,0,0.25))+
  geom_smooth(method = "lm", se= FALSE) +
  labs(x="People using at least basic sanitation services (% of population)", y="Mortality rate",
       title="Mortality Rate wrt Access to Basic Sanitation",
       #caption = 'WDI Dataset'
  )

water_mortality <- ggplot(sanitation, aes(SH.H2O.BASW.ZS, SH.STA.WASH.P5))+
  geom_point(size=7, colour=rgb(0.2,0.4,0.6,0.25))+
  geom_smooth(method = "lm", se= FALSE) +
  labs(x="People using at least basic drinking water services (% of population)", y="Mortality rate",
       title="Mortality Rate wrt Access to Drinking Water",
       caption = 'WDI Dataset \nMortality Rate: Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population'
  )  

grid.arrange(sanitation_mortality, water_mortality, ncol = 1)
#------- is the line of best fit required?


#Or should I use a Line Plot
ggplot(sanitation, aes(SH.STA.BASS.ZS, SH.STA.WASH.P5))+
  geom_line(size=1, colour=rgb(0.2,0.8,0))+
  geom_smooth(method = "lm", se= FALSE) +
  labs(x="People using at least basic sanitation services (% of population)", y="Mortality rate",
       title="Analysis of Mortality Rate",
       caption = 'WDI dataset'
  )

#Mortality Rates based on Income Group 
ggplot(sanitation, aes(income, SH.STA.WASH.P5))+geom_point(size=6, colour=rgb(0.4,0.2,0.4,0.25))+
  labs(x="Income Group", y="Mortality rate",
       title="Analysis of Mortality Rate among Income Groups",
       caption = 'WDI dataset'
  )

#plot a world map with access to sanitation plotted and size based on ??
#but first confirm what packages area allowed 