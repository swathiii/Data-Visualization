t<-seq(from=0, to=10, by=0.1)
y<-sin(t)
plot(y)

#labeling the axis and graph
plot(x=t, y=y,
     type="l",
     xlab="Angle",
     ylab="Sine",
     main="Sine function")

#changing style of line and color
plot(x=t, y=y,
     type="l",
     col="blue",
     lty="dashed",
     xlab="Angle",
     ylab="Sine",
     main="Sine function")

help("points")
#adding graphical element by using points()
plot(x=t, y=y,
     type="l",
     lwd = 4,
     col="green",
     lty="dashed",
     xlab="Angle",
     ylab="Sine",
     main="Sine function")
points(x=t, y=y,
     pch=8,
     lwd = 3,
     col="pink",
     xlab="Angle",
     ylab="Sine",
     main="Sine function")

#-------------------------WDI----------------------------------

install.packages("WDI")

#need to call the library function everytime before you start R to use it
library(WDI)

new_wdi_cache <- WDIcache()
install.packages('curl')

WDIsearch("GDP per capita", cache = new_wdi_cache)

#creating a dataframe
gdp_capita <- WDI(country="all",
                  indicator = "NY.GDP.PCAP.KD",
                  start = 2009, end = 2019,
                  cache = new_wdi_cache)
gdp_capita

View(gdp_capita)

#````selecting specific parts of the dataset````

#selecting individual columns using their names 
gdp_capita$year #obtains the year column
gdp_capita[1,1] #obtains the first row and first column
gdp_capita[30,4] #obtains the 30th row and the fourth column
gdp_capita[,4] #obtains all rows and the fourth column
gdp_capita[12,] #obtains the 12th row and all columns
gdp_capita[1:5, 1:3] #obtains rows 1 to 5 and columns 1 to 3
gdp_capita[c(1,3,7), c(2,4)] #obtains rows 1, 3 and 7, and columns 2 and 4

#selecting rows using conditions
gdp_capita[gdp_capita$country=="China",] #return the row where the country is China
gdp_capita[gdp_capita$year>2018,]

#to find the GDP of UK
UK<-gdp_capita[gdp_capita$country=="United Kingdom",]
UK

uk_gdp<-UK[,5]
uk_gdp

year<-UK[,4]
year

plot(year, uk_gdp,
     type='l',
     xlab = "year",
     ylab = "GDP",
     main = "GDP of UK",
     col = "blue",
     lwd = 4 )

#to add another line plotting the GDP of UAE 
#plotting GDP of UAE
UAE<-gdp_capita[gdp_capita$country=="United Arab Emirates",]
UAE
uae_gdp<-UAE[,5]
uae_gdp
plot(year, uae_gdp,
     type='l',
     xlab = "year",
     ylab = "GDP",
     main = "GDP of UAE",
     col = "red",
     lwd = 4 )

#combining two plots 
plot(year, uk_gdp,
     type='l',
     xlab = "year",
     ylab = "GDP",
     main = "GDP of UK and UAE",
     col = "blue",
     ylim = c(30000, 50000),
     lwd = 4 )
points(year, uae_gdp,
     type='l',
     xlab = "year",
     ylab = "GDP",
     main = "GDP of UAE",
     col = "red",
     ylim = c(30000, 50000),
     lwd = 4 )

#creating dataframes for both the countries
uk <- WDI(country="GB",
          indicator = "NY.GDP.PCAP.KD",
          start = 2009, end = 2019,
          cache = new_wdi_cache)
uae <- WDI(country="AE",
           indicator = "NY.GDP.PCAP.KD",
           start = 2009, end = 2019,
           cache = new_wdi_cache)
uk
uae

#creating a DF with data for the 2 countries by providing vector to the country parameter
uk_uae <- WDI(country=c("AE","GB"),
              indicator = "NY.GDP.PCAP.KD",
              start = 2009, end = 2019,
              cache = new_wdi_cache)
uk_uae

#using two indicators
us <- WDI(country="US",
          indicator = c("NY.GDP.PCAP.KD","NY.GDP.PCAP.CD"),
          cache = new_wdi_cache)
us
#start and end dates were not included so by default its 1960-2021

#assigning names to indicator columns to make it less confusing
us_better_names <- WDI(country="US",
                       indicator = c("GDP_pc_constant_dollar"="NY.GDP.PCAP.KD",
                                     "GDP_pc_current_dollar"="NY.GDP.PCAP.CD"),
                       cache = new_wdi_cache)
us_better_names


#adding extra parameter : r. If we set that parameter to
#true, the resulting data frame will contain several extra columns with information regarding the country: the
#iso3c code, its region, its capital, its longitude, its latitude, its lending status and its income level.

us_better_names <- WDI(country="US",
                       indicator = c("GDP_pc_constant_dollar"="NY.GDP.PCAP.KD",
                                     "GDP_pc_current_dollar"="NY.GDP.PCAP.CD"),
                                      extra = TRUE,
                                      cache = new_wdi_cache)
us_better_names

#plot comparing the two measures of GDP directly, one in the X axis and the other in the Y axis using dots
plot( gdp1, gdp2,
      #type='l',
      xlab = "gdp1",
      ylab = "gdp2",
      main = "Comparing two GDPs",
      col = "orange",
      ylim = c(30000, 50000),
      lwd = 4 )

#plot showing both indicators as lines in the Y axis and the X axis being the year.
gdp1 <- us_better_names[,7]
gdp2 <- us_better_names[,8]
year_plot <- us_better_names[,4]

plot(year_plot, gdp1,
     type='l',
     xlab = "year",
     ylab = "GDP",
     main = "Two indicators of US",
     col = "purple",
     ylim = c(30000, 50000),
     lwd = 4 )
points(year_plot, gdp2,
       type='l',
       xlab = "year",
       ylab = "GDP",
       col = "green",
       ylim = c(30000, 50000),
       lwd = 4 )

#``````writing data into files``````
uk<-gdp_capita[gdp_capita$country=="United Kingdom",]
write.table(uk, "UK GDP per capita 2009 2019.csv", sep=",", row.names=FALSE)

#reading the CSV file
uk_copy<-read.csv("UK GDP per capita 2009 2019.csv", header=TRUE)
uk_copy
#header=TRUE parameter is used to indicate that the CSV file has column headers on the first line

#Final exercise of week 1 : indicator- Food Production per capita

WDIsearch("Forest area", cache = new_wdi_cache)

forest_area <- WDI(country="all",
                  indicator = "AG.LND.FRST.ZS",
                  cache = new_wdi_cache)
View(forest_area)

#comparing one indicator across two countries across time
syc <- WDI(country="SC",
          indicator = "AG.LND.FRST.ZS",
          start = 1990, end = 2020,
          cache = new_wdi_cache)
fin <- WDI(country="FI",
           indicator = "AG.LND.FRST.ZS",
           start = 1990, end = 2020,
           cache = new_wdi_cache)
syc
fin

area1 <- syc[,5]
area2 <- fin[,5]
year_plot <- syc[,4]

plot(year_plot, area1,
     type='l',
     xlab = "year",
     ylab = "area",
     main = "Forest areas of Sychelles and Finland",
     col = "green",
     lwd = 2 )
points(year_plot, area2,
       type='l',
       xlab = "area",
       ylab = "GDP",
       col = "brown",
       lwd = 2  )