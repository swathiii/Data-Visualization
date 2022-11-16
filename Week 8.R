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

lab = labourforce[,7:12]
pca_labour <- prcomp(labourforce[,7:11], scale = TRUE)  #we have six indicators this time instead of 5

labourforce.pca_labour <- data.frame(
  country = labourforce$country,
  region = labourforce$region,
  PC1 = pca$x[,1],
  PC2 = pca$x[,2]
)
