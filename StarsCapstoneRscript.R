##### Stars Data set Exploration ######

# Installing necessary packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = 
                                       "http://cran.us.r-project.org")
# Loading Packages

library(tidyverse)
library(caret)
library(dplyr)

# Stars Dataset
# https://www.kaggle.com/deepu1109/star-dataset/download

dl <- tempfile()

download.file("https://www.kaggle.com/deepu1109/star-dataset/download", dl)

stars <- read.csv("~/Downloads/6 class csv.csv")

# In the case of failure of automatic download:
# Got to "https://www.kaggle.com/deepu1109/star-dataset/download" (This should automatically download a zip file) (**You might need to create an account if you don't already have one**)
# Then import that data by: going to RSTUDIO, clicking on file, importing dataset, clicking import From Text (baser), and searching for a 6 class csv file. Import that file.
# Finally, store that code to an object called "stars" (From here on you can run the code manually starting from "names(stars)")

# Renaming Columns 

names(stars) <- c("Temperature", "Luminosity", "Radius", "Magnitude", "Type", "Color", "Class")

#Load necessary packages:

library(ggplot2)
library(dplyr)
library(ggpmisc)
library(tidyr)
library(lattice)
library(grid)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(scales)

# Dimensions and Summary of the Dataset

# Number of Rows
nrow(stars)

# Number of Columns
ncol(stars)

# First 6 rows
head(stars)

# Summary
summary(stars)

# Data Exploration and Visualization 


# Temp vs. Radius Plot

stars %>% select(Temperature, Luminosity) %>% ggplot(aes(Temperature, Luminosity)) + geom_point(color = "black") + geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, color = "coral4") + theme_light() + ggtitle("Temperature vs Luminosity") + xlab("Temperature (K)") + ylab("Radius (R/Ro)") + theme(plot.title = element_text(hjust = 0.5)) + stat_poly_eq(formula = "y ~ x", aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~~~~")), parse = TRUE, color = "palevioletred4") 

# Temp vs. Radius (Spectral Class)

stars %>% ggplot(aes(x = Temperature, y = Radius, col = Class)) + geom_point() + theme_economist_white() + ggtitle("Temperature vs. Radius (Ft. Spectral Class)") + guides(color = guide_legend(title = "Spectral Class")) + xlab("Temperature (K)") + ylab("Radius (R/Ro)") + theme(plot.title = element_text(hjust = 0.5))

# Spectral Class & Temperature (Box Plot)

stars %>% group_by(Class) %>% ggplot(aes(x = reorder(Class, Temperature), y = Temperature, fill = Class)) + geom_boxplot() + theme_linedraw() + ggtitle("Spectral Class & Temperature") + xlab("Spectral Class") + theme(plot.title = element_text(hjust = 0.5))

# Spectral Class & Magnitude

stars %>% group_by(Class) %>% ggplot(aes(x = reorder(Class, Magnitude), y = Magnitude, fill = Class)) + geom_boxplot() + theme_linedraw() + ggtitle("Spectral Class & Magnitude") + xlab("Spectral Class") + theme(plot.title = element_text(hjust = 0.5))

# Tables for each Spectral Class in relation to its tmperature, radius, luminosity, and magnitude

# Spectral Class A

stars %>% filter(Class == "A") %>% select(Temperature, Luminosity, Radius, Magnitude) %>% summary()

# Spectral Class B

stars %>% filter(Class == "B") %>% select(Temperature, Luminosity, Radius, Magnitude) %>% summary()

# Spectral Class F

stars %>% filter(Class == "F") %>% select(Temperature, Luminosity, Radius, Magnitude) %>% summary()

# Spectral Class G

stars %>% filter(Class == "G") %>% select("Temperature", "Luminosity", "Radius", "Magnitude") %>% summary()

# Spectral Class K

stars %>% filter(Class == "K") %>% select("Temperature", "Luminosity", "Radius", "Magnitude") %>% summary()

# Spectral Class M

stars %>% filter(Class == "M") %>% select("Temperature", "Luminosity", "Radius", "Magnitude") %>% summary()

# Spectral Class O

stars %>% filter(Class == "O") %>% select("Temperature", "Luminosity", "Radius", "Magnitude") %>% summary()


# Further Data Exploration and Visualization


# Temp vs Radius (Star Type)

stars %>% ggplot(aes(Temperature, log2(Radius), color = as.factor(Type))) + geom_point() + theme_linedraw() + scale_color_discrete(breaks = c(0, 1, 2, 3, 4, 5), labels = c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence", "Super Giants", "Hyper Giants")) + xlab("Temperature (K)") + ylab("Radius (Log2)") + ggtitle("Temperature vs. Radius (ft. Star Type)") + theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Star Type")

# Temp vs Luminosity (Star Type)

stars %>% ggplot(aes(Temperature, log2(Luminosity), color = as.factor(Type))) + geom_point() + theme_linedraw() + scale_color_discrete(breaks = c(0, 1, 2, 3, 4, 5), labels = c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence", "Super Giants", "Hyper Giants")) + xlab("Temperature (K)") + ylab("Luminosity (Log2)") + ggtitle("Temperature vs. Luminosity (ft. Star Type)") + theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Star Type")

# Temp vs Magnitude (Star Type)

stars %>% ggplot(aes(Temperature, Magnitude, color = as.factor(Type))) + geom_point() + theme_linedraw() + scale_color_discrete(breaks = c(0, 1, 2, 3, 4, 5), labels = c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence", "Super Giants", "Hyper Giants")) + xlab("Temperature (K)") + ylab("Magnitude") + ggtitle("Temperature vs. Magnitude (ft. Star Type)") + theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Star Type")

# Big model: Hertzprung-Russell Diagram

stars %>% ggplot(aes(x = Temperature, y = Magnitude, color = as.factor(Class), shape = as.factor(Type))) + geom_point() + theme_minimal() + scale_y_reverse() + scale_x_reverse() + scale_shape_discrete(breaks = c(0, 1, 2, 3, 4, 5), labels = c("Brown Dwarf", "Red Dwarf", "White Dwarf", "Main Sequence", "Super Giants", "Hyper Giants")) + labs(color = "Star Class", shape = "Star Type") + xlab("Temperature (K)") + ylab("Magnitude (MV)") + ggtitle("Hertzsprung-Russell Diagram") + theme(plot.title = element_text(hjust = 0.5))

# Modeling and Predictions

# Loading necessary packages

library(nnet)

# Training and prtitioning stars datset

trainingSamp <- stars$Type %>% createDataPartition(p = 0.7, list = FALSE)

trainData  <- stars[trainingSamp, ]
testData <- stars[-trainingSamp, ]

# We train a k-nearest neighbor algorithm with a tunegrid parameter to optimize for k

train_knn <- train(Type ~ ., method = "knn", data = trainData,tuneGrid = data.frame(k = seq(2, 30, 2)))

# Visualize/plot and save the optimal value for k

knnplot <- ggplot(train_knn, highlight = TRUE)

knnplot

optim_knn <- train_knn$bestTune[1, 1]

optim_knn

# Multinomial logistic regression model to predict star type


# Fitting model

model <- nnet::multinom(Type ~ Temperature + Magnitude + Radius + Luminosity, data = trainData)

# Summarizing model

summary(model)

# Predicting Class

classPredict <- model %>% predict(testData)
head(classPredict)

# Accuracy of Model

mean(classPredict == testData$Type)

