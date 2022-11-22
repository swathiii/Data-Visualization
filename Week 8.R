#-----------VVISUALIZATIONS OF HIGH DIMENSIONAL DATA-------------------

#INDICATORS
# SE.PRM.ENRR: School enrollment, primary (% gross)
# SE.SEC.ENRR: School enrollment, secondary (% gross)
# SE.TER.ENRR: School enrollment, tertiary (% gross)
# SE.SEC.PROG.ZS: Progression to secondary school (%)
# SE.PRM.CMPT.ZS: Primary completion rate, total (% of relevant age grouP

library(ggplot2)
library(WDI)

new_wdi_cache <- WDIcache()

education <- WDI(indicator = c("SE.PRM.ENRR","SE.SEC.ENRR",
                               "SE.TER.ENRR","SE.SEC.PROG.ZS","SE.PRM.CMPT.ZS"),
                 start = 2014,
                 end = 2014,
                 extra = TRUE,
                 cache =  new_wdi_cache)

#removing aggregate data as they are not countries
education <- education[education$region != "Aggregates",]

#removing countries that have missing  data in any columns
education <- na.omit(education)


#-----------------PRINCIPAL COMPONENT ANALYSIS (PCA)--------------------------

#calculating the PCA on the education data, it extracts the position of 
#the countries for the first two principal components and plots them (geom_text)

#we only use the indicator columns to calculate the principal components
pca <- prcomp(education[,7:11])  #7:11 are all the indicator columns
education.pca <- data.frame(
  country = education$country,
  region = education$region,
  PC1 = pca$x[,1],
  PC2 = pca$x[,2]
)

View(education[,7:11])

ggplot(education.pca, aes(PC1, PC2, label = country)) +
  geom_text(size=3) +
  labs(title = "PCA of education data",
       caption = "World Bank")

#telling the prcomp function to scale all the variables
pca <- prcomp(education[,7:11], scale=TRUE)

education.pca.scaled <- data.frame(
  country = education$country,
  region = education$region,
  PC1 = pca$x[,1],
  PC2 = pca$x[,2]
)

ggplot(education.pca.scaled, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs( title = "Scaled PCA of education data",
        caption = "World Bank")

#the african counties spread out while other countries ended up closer
#the ranges are already similar, scaling does not help the analysis here

#changing the plot to scatterplot, with points coloured by continent 
ggplot(education.pca, aes(PC1, PC2)) +
  geom_point(aes(colour = region), size =3) +
  labs(title = "PCA of education data",
       caption = "World Bank",
       colour = "Region")
#in some cases countries of same reguin end up closer together
# implying eduacation patterns tend to be similar in different countries
#of the same region

#creating a loading plot to understand why each country is positioned that way
#we plot the loading of each original dimensions for  PC1 and PC2
pca <- prcomp(education[,7:11])

education.loading <- data.frame(
  dimensions = colnames(education)[7:11],
  PC1 = pca$rotation[,1],
  PC2 = pca$rotation[,2]
)

ggplot(education.loading, aes(PC1, PC2, label = dimensions)) +
  geom_text(size=3) +
  labs(title = "Loading plot of education data",
       caption = "World Bank")
#it seems the cluster of sub-saharan african countries on the right 
#seems to be driven by low proportion in enrollment in tertiary and secondary education

#-----EXERCISE-----------------------------------

#do this on a different set of data : year 2019

#INDICATORS 
# SL.TLF.CACT.ZS : Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)
# SL.AGR.EMPL.ZS : Employment in agriculture (% of total employment) (modeled ILO estimate)
# SL.IND.EMPL.ZS : Employment in industry (% of total employment) (modeled ILO estimate)
# SL.SRV.EMPL.ZS : Employment in services (% of total employment) (modeled ILO estimate)
# SL.EMP.TOTL.SP.ZS : Employment to population ratio, 15+, total (%) (modeled ILO estimate)
# SL.UEM.TOTL.ZS : Unemployment, total (% of total labor force) (modeled ILO estimate)

data("WDI_data")
View(WDI_data)


WDIsearch("Employment in agriculture", cache = new_wdi_cache)     
WDIsearch("Unemployment", cache = new_wdi_cache)  

labourforce <- WDI(indicator = c("SL.TLF.CACT.ZS", "SL.AGR.EMPL.ZS", "SL.IND.EMPL.ZS", 
                                 "SL.SRV.EMPL.ZS", "SL.EMP.TOTL.SP.ZS", "SL.UEM.TOTL.ZS"),
                   start = 2019,
                   end = 2019,
                   extra = TRUE,
                   cache = new_wdi_cache )


#removing aggregates and null values
labourforce <- labourforce[labourforce$region != "Aggregates",]
labourforce <- na.omit(labourforce)

lab <- prcomp(labourforce[,7:12])

labourforce.pca <- data.frame(
  country = labourforce$country,
  region = labourforce$region,
  PC1 = lab$x[,1],
  PC2 = lab$x[,2]
)

ggplot(labourforce.pca, aes(PC1, PC2, label = country)) +
  geom_text(size=3) +
  labs(
    title = "PCA of labour force data",
    caption = "World Bank"
  )

#changing the plot to scatterplot, with points coloured by continent
ggplot(labourforce.pca, aes(PC1, PC2)) +
  geom_point(aes(colour = region), size = 3) +
  labs(
    title = "PCA of labourforce data",
    caption = "World Bank"
  )
#plotting the loading of each original dimension to understand why
#each country is positioned that way 

labourforce.loading <- data.frame(
  dimensions = colnames(labourforce)[7:12],
  PC1 = lab$rotation[,1],
  PC2 = lab$rotation[,2]
)

ggplot(labourforce.loading, aes(PC1, PC2, label = dimensions)) +
  geom_text(size = 3) +
  labs(
    title = "loading plot of labour force data",
    caption = "World Bank"
  )
#imo : asian and sub saharan african regions have more labourers in the agricultural industry
# Eurpean, latin american and middle east regions are more prominent in the service and inductries


#-----EXTRA------
#SCALING to see if it makes any difference 
lab.scaled <- prcomp(labourforce[,7:12], scale = TRUE)

labourforce.pca.scaled <- data.frame(
  country = labourforce$country,
  region = labourforce$region,
  PC1 = lab.scaled$x[,1],
  PC2 = lab.scaled$x[,2]
)

ggplot(labourforce.pca.scaled, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(
    title = "Scaled PCA of labour force data",
    caption = 'World Bank'
  )
#the ranges are already similar, scaling does not help the analysis here



#-------------------MULTIDIMENSIONAL SCALING------------------------------------#
#function : isoMDS
#first normalise data using the scale function
#use dist function to calculate the distance matrix between countries
#using isoMDS we extract the positions of the countries in MDS space

library(MASS)
scaled.data <- scale(education[,7:11])
distance.matrix <- dist(scaled.data)  #uses eucledian distance by default
mds <- isoMDS(distance.matrix)

education.mds <- data.frame(
  country = education$country, 
  region = education$region,
  MDS1 = mds$points[,1],
  MDS2 = mds$points[,2]
)

ggplot(education.mds, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title = "MDS of education data",
    caption = "World Bank"
  )

#results are similar to PCA plot, the way distance between
#countries is computed can have a large effect on the resulting plot

#using canberra distance measure
distance.matrix <- dist(scaled.data, 'canberra')
mds <- isoMDS(distance.matrix)
education.mds <- data.frame(
  country = education$country,
  region = education$region,
  MDS1 = mds$points[,1],
  MDS2 = mds$points[,2]
)

ggplot(education.mds, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title = "MDS of education data with Canberra distance measure",
    caption = 'World Bank'
  )

#EXERCISE : perform MDS on previous exercise : Labour Force
#extra : try Canberra, manhattan, minkowski and compare 

labourforce.scaled.data <- scale(labourforce[,7:12])
labourforce.distance.matrix <- dist(labourforce.scaled.data) #default eucledian distance
mds2 <- isoMDS(labourforce.distance.matrix)

labourforce.mds <- data.frame(
  country = labourforce$country,
  region = labourforce$region,
  MDS1 = mds2$points[,1],
  MDS2 = mds2$points[,2]
)

ggplot(labourforce.mds, aes(MDS1, MDS2, label = country)) +
  geom_text(size = 3) +
  labs(
    title = "MDS of labour force data : eucledian distance",
    caption = 'World Bank'
  )

#EXTRA : canberra, manhattan, minkowski

#canberra
labourforce.distance.matrix <- dist(labourforce.scaled.data, 'canberra')
#a lot of distance, it's all scaled away from the distance

#manhattan 
labourforce.distance.matrix <- dist(labourforce.scaled.data, 'manhattan')
#the plot seems to be mirrored plot of eucledian 

#minkowski
labourforce.distance.matrix <- dist(labourforce.scaled.data, 'minkowski')
#no difference seen in the plot, same as eucledian distance

#--------------PARALLEL COORDINATES---------------------------------

#it's not easy to plot parallel coordinated in ggplot
#GGally contains afunction to plot simple parallel coordinates

install.packages('GGally')
library(GGally)

View(education)
ggparcoord(education, columns = c(7,8,9,10,11), groupColumn = 12) +#a function for plotting static parallel coordinates
            labs(colour='Region', x='Variables', y='Scaled values', 
                 title = 'Parallel coordinates of education data', 
                 caption = 'World Bank')

#feature names are too close : we use a function to fix that 
ggparcoord(education, columns = c(7,8,9,10,11), groupColumn = 12) +
  theme(axis.text.x = element_text(angle=45)) +
  labs( colour = 'Region',
        x = 'Variables',
        y = 'Scaled values',
        title = 'Parallel coordinates of education data',
        caption = 'World Bank')

#refining it further by removing some elements and adding some vertical lines
#we can use geom_hline to define a set of lines by at which point they intercept the X axis
ggparcoord(education, columns = c(7,8,9,10,11), groupColumn = 12) +
  theme(axis.text.x = element_text(angle=45),
        panel.background = element_blank()) +
  geom_vline(xintercept = c(1, 2, 3, 4, 5)) +
  labs(colour='Region', x='Variables', y='Scaled values',
       title = 'Parallel coordinates of education data', 
       caption = 'World Bank')

#EXERCISE : create a similar plot using the labour dataset and colour the lines
#according to the income status 

View(labourforce)
ggparcoord(labourforce, columns = c(7,8,9,10,11,12), groupColumn = 17) +
  theme(axis.text.x = element_text(angle = 45),
  panel.background = element_blank()) +
  geom_vline(xintercept = c(1, 2, 3, 4, 5, 6)) +
  labs(colour = 'Income', 
       x = 'Variables',
       y = 'Scaled values',
       title = 'Parallel coordinates of labour force data',
       caption = 'World Bank')

#---------------------------SPIDER CHART---------------------------------------
#ggradar : installed via github
install.packages('devtools')
devtools::install_github('ricardo-bion/ggradar',
                         dependencies = TRUE,
                         force = TRUE)

library(ggradar)
library(scales)
library(tidyverse)

#removing year col : a numerical col not necessary for the visualization
education$year <- NULL

education%>%
  mutate_if(is.numeric, rescale) %>% #scaling all numeric columns between 0 and 1
  mutate(new_region = str_replace_all(region, " ", "_")) %>% #saving names of regions with spaces is replaced with underscores to new_region
  group_by(new_region) %>% #grouping data by new region column
  summarise_if(is.numeric, mean) %>% #calculating the mean of every numeric column
  ggradar() #passing the final data into this function

#this shows the average characteristics of countries in each region

#creating a more balanced viz by reducing the size of text in the labels and legends
education %>% 
  mutate_if(is.numeric, rescale) %>% 
  mutate(new_region = str_replace_all(region, " ", "_")) %>%
  group_by(new_region) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(
    axis.label.size = 3, 
    legend.text.size = 8
  )

#EXERCISE : labour force dataset : group based on income levels
#modify aesthetics 

labourforce %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_income = str_replace_all(income, " ", "_")) %>%
  group_by(income) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(
    axis.label.size = 3,
    legend.text.size = 8
  )

#EXTRA : make the background white and the axis lines
#black and thicker

?ggradar
labourforce %>%
  mutate_if(is.numeric, rescale) %>%
  mutate(new_income = str_replace_all(income, " ", "_")) %>%
  group_by(income) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(
    axis.label.size = 3,
    legend.text.size = 8,
   background.circle.colour = "#F7F7F7",
    axis.line.colour = 'black',
    #group.line.width = 2
   #grid.line.width = 0.5,

  )


#--------------FINAL EXERCISE : Pick indicators from Population Dynamics-------------

library(WDI)
new_wdi_cache <- WDIcache()

#SP.POP.TOTL : Population Total
#SP.POP.GROW  : Population Growth
#SP.DYN.CBRT.IN : Birth rate, crude (per 1,000 people)
#SP.DYN.CDRT.IN : Death rate, crude (per 1,000 people)
#SP.DYN.TFRT.IN : Fertility rate, total (births per woman)

WDIsearch("Fertility rate", cache = new_wdi_cache)     

#setting up population dynamics for the year 2020
population <- WDI(indicator = c("SP.POP.TOTL",
                                "SP.POP.GROW",
                                "SP.DYN.CBRT.IN",
                                "SP.DYN.CDRT.IN",
                                "SP.DYN.TFRT.IN"),
                  start = 2020,
                  end = 2020,
                  extra = TRUE,
                  cache = new_wdi_cache)

View(population)


#Removing aggregated and missing data 
population <- population[population$region != 'Aggregates',]
population <- na.omit(population)

