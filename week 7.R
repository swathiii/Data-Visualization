#VISUALIZATIONS OF CONTINUOUS DATA

#--------------SCATTER PLOT-------------
#we wll use grid.arrange function which is contained in the gridExtra package
#included in tidyverse, to create a set of subplots, first each plot is 
#saved into a variable. Then the function combines them 
library(tidyverse)
data("mpg")
View(mpg)


size.plot <- ggplot(mpg, aes(displ,cyl)) + geom_point(aes(size=cty)) +
  labs(x='Displacement', y='Cylinders', size = 'City mpg')

color.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(colour = hwy)) +
  labs(x = 'Displacement', y= 'Cylinders', colour = 'Highway\nmpg')

shape.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(shape=fl)) + 
  labs(x = 'Displacement', y= 'Cylinders', shape = 'Fuel Type')

grid.arrange( size.plot, color.plot, shape.plot, ncol = 1 )


#several points end up one over the other, so we use translucent color

ggplot(mpg, aes(displ, cyl)) + geom_point(size=6, colour = rgb(1,0,0,0.25)) + #setting transparency to 25% using the fourth parameter of the rgb function
  labs(x = 'Displacement', y='Cylinders', title = 'Translucent colours can show data point density',
       caption = 'mpg dataset')


#second way to show there are large numbers of data points in the same position is to use jitter
#jitter adds a bit of random noise to the position of the data points
#if several points are in the same position, they will end up as a cloud of points
#around that position

ggplot(mpg, aes(displ, cyl)) +
  geom_point(size=2, position = position_jitter(w=0.05, h=0.05)) +
  labs( x= 'Displacement', y = 'Cylinders', 
        title = 'Random noise can also show data point density',
        caption = 'mpg dataset')

#bigger the value, larger the noise that is introduced. We can also introduce different
#noise along the X-axis (w parameter) and the Y-axis (h parameter)
#adding noise only to the y-axis where values do not vary much

ggplot(mpg, aes(displ, cyl)) +
  geom_point(size = 3, colour = 'purple', position = position_jitter(w = 0, h = 0.15)) +
  labs(x= "Displacement", y = 'Cylinders',
        title = 'Noice can be added to just one axis',
       caption = 'mpg dataset')


#adding a line of best fit to indicate how the variables are related. 
#can be added using the geom_smooth function. function will automatically calculate
#the line based on the method parameter, se : determines if the CI of the line are shown

ggplot(mpg, aes(cty, hwy)) + geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  labs(x="City", y="Highway", title = "Comparing fuel economy",
       caption = "mpg dataset")


#EXERCISE : SctPlt comparing the fuel economy in highway and in the city. 
#create two additional sctplts where you show the density of data points at each
#position by using a translucent green colour in one and by adding noise in the other
#add a line of best fit to the last scatter plot
#NOTE: mpg is usually higher on a highway when compared to city

ggplot(mpg, aes(cty, hwy)) + geom_point(size = 15 ) +
  labs( x='City', y='Highway', title = 'Comparing fuel economy', caption = 'mpg dataset')

#adding jitter and translucency and line of best fit
ggplot(mpg, aes(cty, hwy)) + 
  geom_point(size = 10, color = rgb(0.2,0.4,0, 0.15), position = position_jitter(w= 0.05, h=0.05)) +
  geom_smooth(method = "lm", se = FALSE)
  labs( x = 'city', y= 'highway', 
        title = 'Depicting density using noise',
        caption = 'mpg dataset')

#-----------------LINE CHART---------------------------------------#
  
#only appropriate when there is a progression along one of the axis
#the date or progression variable is added as the X axis
#the metric we are analyzing goes to the Y axis
  
data("economics")

ggplot(economics, aes(x=date, y=unemploy)) + geom_line() +
  labs(x='Date', y="Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset")


#changing the colour and width of the line, thick line can hide small changes
#thick lines can add more visual contrast

ggplot(economics, aes(x=date, y=unemploy)) +
  geom_line(size = 2, color = 'red') + 
  labs(x='Date', y="Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset")


#colour can be mapped to a variable, line has to be thicker to make the color
#change more clear

ggplot(economics, aes(x=date, y=unemploy)) +
  geom_line(size = 1, aes(colour = pop)) + 
  labs(x='Date', y="Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset",
       colour = "Population\n(in thousands)" )


#different lines in the same plot can be done by calling geom_line several times
#in that case the y variable needs to be mapped in each geom_line cell

ggplot(economics, aes(x=date)) +
  geom_line(aes(y = unemploy)) +
  geom_line(aes( y = pop )) + 
  labs( x = 'date', y = 'Numbers (in thousands)',
        title = 'Umemployment progression',
        caption = 'economics dataset') + 
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6))
#transforming the Y axis to logarithmic since values of both lines vary
#also defining where the axis ticks should appear with the break parameter


#using colors to differentiate the lines and assigning what the lines represent
#settling label of each line using the col parameter of the aes function, then
#use the function scale_colour_manual to defind the colour for each label

ggplot(economics, aes(x= date)) +
  geom_line(aes(y = unemploy, col = "Unemployed")) + 
  geom_line(aes(y = pop, col = "Total population")) +
  labs(x = "date", y = 'Numbers (in thousands)',
       title = "Unemployment progression",
       caption = "economics dataset") +
  scale_y_continuous(trans = "log", 
                     breaks = c(10**3, 10**4, 10**5, 10**6)) +
  scale_colour_manual(name = "Legend",
                      values = c("Unemployed" = "red",
                                 "Total population" = "blue"))
  

#EXERCISE : plot personal savings rate against date and set the colour to change 
#based on the personal consumption expenditures 

View(economics)

ggplot(economics, aes( x = date)) + 
  geom_line(size = 2, aes(y = psavert, colour = pce)) +
  labs(x = 'Date', y = 'Savings',
       title = 'Personal Savings rate',
       caption = 'economics dataset')

#extra : unemployment data as a percentage of the total population

economics$employed <- economics$pop - economics$unemploy
economics$unempercent <- (economics$unemploy / economics$employed) * 100

View(economics)

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y= unempercent, col = "Unemployment percentage")) +
  geom_line(aes(y = unemploy, col = "Unemployed")) +
  geom_line(aes(y = pop, col = "Total population")) +
  geom_line(aes(y = employed)) +
  labs(x = 'Date', y = 'Numbers (in thousands)',
       title = 'Unemployed data as a percentage of the total population',
       caption = 'economics dataset') +
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6)) +
  scale_colour_manual(name = "Legend",
                      values = c("Unemployment percentage" = "red",
                                 "Unemployed" = "blue",
                                 "Total population" = "green"))


#-------------------HISTOGRAM AND 2D HISTOGRAM-----------------------#  

#geom_histogram : parameter bins defines the number of bins to use, bin width :
#for the width of each bin. breaks : vector of numbers representing the bin boundaries

#bins : number of bins to use
hist1 <- ggplot(mpg, aes(cty)) +
  geom_histogram(bins = 10, fill = 'lightgreen') + labs(x = 'Fuel economy in the city',
                                                        y = 'Frequency',
                                                        caption = 'mpg dataset',
                                                        title = 'Bins')
#bin width : width of each bin
hist2 <- ggplot(mpg, aes(cty)) +
  geom_histogram(binwidth = 5, fill = 'lightblue') +
  labs( x = 'Fuel economy in the city',
        y = 'Frequency',
        title = 'Binwidth')

#breaks : vector of numbers representing the bin boundaries 
hist3 <- ggplot(mpg, aes(cty)) +
  geom_histogram(breaks = c(5, 10, 15, 20, 25 , 30 , 35 , 40), fill = 'lightcoral') +
  labs(x = 'Fuel economy in the city',
       y = 'Frequency',
       title = 'Breaks')

grid.arrange(hist1, hist2, hist3, ncol = 1 )

#EXERCISE: create 3 hist to show the dist of engine displacement values : 
# one with bin boundaries at 1, 3, 5, and 7
# one with 15 bins 
# one with a bin width of 1

histg1 <- ggplot(mpg, aes(displ)) +
  geom_histogram(breaks = c(1, 3, 5, 7), fill = 'plum') +
  labs( x = 'Engine displacement',
        y = 'Frequency',
        title = 'Breaks',
        caption = 'mpg dataset')

histg2<- ggplot(mpg, aes(displ)) +
  geom_histogram(bins = 15, fill = 'lightblue') +
  labs( x = 'Engine displacement',
        y = 'Frequency',
        title = 'Bins',
        caption = 'mpg dataset')

histg3 <- ggplot(mpg, aes(displ)) +
  geom_histogram(binwidth = 1, fill = 'lightpink') +
  labs( x = 'Engine displacement',
        y = 'Frequency',
        title = 'Bin width',
        caption = 'mpg dataset')

grid.arrange(histg1, histg2, histg3, ncol = 1)

#EXTRA : function geom_bin2d can be used to create a 2D histogram. Look at the
#help section of the function and create a 2D histogram using the fuel economy
#in the city and in the highway 

?geom_bin2d

ggplot(mpg, aes(cty, hwy)) +
  geom_bin2d()

#bin width 
ggplot(mpg, aes(cty, hwy)) +
  geom_bin2d(binwidth = c(1.5, 1.5))

#------------BOXPLOT AND VIOLIN PLOT-----------------#

#geom_boxplot : the function must have continuous variable mapped. There are
#2 parameters that modify the style of the boxplot ; 
#varwidth = TRUE will make the width of the box proportional to the number of
#data points ; notch = TRUE will create a notch around the median to show its 
#confidence interval

ggplot(mpg, aes(cty, class)) +
  geom_boxplot(varwidth = TRUE, fill = "plum") +
  labs( title = 'Fuel economy in city grouped by class of vehicle',
        y = "Class of vehicle",
        x = "City Mileage",
        caption = 'mpg dataset')

#EXERCISE : boxplot that  shows the distribution of engine displacement
#grouped by number of cylinders 

ggplot(mpg, aes(displ, cyl, group = cyl)) +
  geom_boxplot(varwidth = TRUE, fill = 'lightblue') +
  labs( x = "Engine Displacement",
        y = 'number of cylinders',
        title  = 'Distribution of engine displacement grouped by number of cylinders',
        caption = 'mpg dataset')

#EXTRA : geom_violin creates violin plots very similarly, read document and
#transform the box plot you've created into a violin plot

?geom_violin

ggplot(mpg, aes(displ, cyl, group = cyl)) +
  geom_violin(trim = FALSE, fill = 'plum') +
  labs( x = "Engine Displacement",
        y = 'number of cylinders',
        title  = 'Distribution of engine displacement grouped by number of cylinders',
        caption = 'mpg dataset')


#FINAL EXERCISE : using the diamonds dataset : recreate the two plots shown
data("diamonds")
View(diamonds)

#plot 1 : Comparison of price and carat

ggplot(diamonds, aes(price, carat)) +
  geom_point(colour = rgb(0,0,1, 0.10)) +
  labs(  x = "Price",
         y = 'Carat',
         title = 'Comparison of price and carat',
         caption = 'diamonds dataset')


#plot 2 : Distribution of price 

ggplot(diamonds, aes(cut, price)) +
  geom_boxplot( fill = 'green') + 
  labs ( x = "Cut type",
         y = 'Price',
         title = 'Distribution of price based on cut type',
         caption = 'diamonds dataset')

?geom_boxplot

ggplot(diamonds, aes(carat, price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.25)))