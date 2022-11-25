# load the requisite libraries
pack <- function(lib){
  new.lib <- lib[!(lib %in% 
                     installed.packages()[, "Package"])]
  if (length(new.lib)) 
    install.packages(new.lib, dependencies = TRUE)
  sapply(lib, require, character.only = TRUE)
}

# neuralnet for neural networks, corrplot and caret for correlations
# caTools for train_test split, and ggplot for plotting
packages <- c('neuralnet', 'corrplot', 'caret', 'caTools', 'ggplot2', 'ggpubr',
              'cowplot', 'h2o', 'lime')
pack(packages)

# Set Working Directory
getwd() # establish current working directory

# set new working directory
working_dir = paste('C:/Users/lshpaner/OneDrive/Cornell University/Coursework/',
                    'CEEM586 - Neural Networks and Machine Learning/', sep='')
setwd(working_dir)

# Read in the data
election <- read.csv(paste("https://raw.githubusercontent.com/lshpaner/",
                           "CEEM586_Neural_Networks_and_ML/main/data/",
                           "ElectionData.csv", sep=""), row.names = 1, 
                     header = TRUE,
                     stringsAsFactors = FALSE)

# remove index column to better adapt to machine learning format
rownames(election) <- NULL
head(election) #inspect the df

# remove Clinton from the dataframe
election_new <- election; election_new$Clinton <- NULL

head(election_new) # reinspect the new df
str(election_new[1,]) # inspect the structure of the df
cat("Dimensions of dataset:", dim(election_new), # dimensions of dataset
    "\n", "There are", sum(is.na(election_new)),  
    "'NA' values in the entire dataset.")

#######################################
### Exploratory Data Analysis (EDA) ###
#######################################

frame_trump <- cor(election_new) # create a correl. matrix between all variables
corrplot(frame_trump, mar = c(0, 0, 0, 0), method="color",
         col=colorRampPalette(c("#FC0320", "white", "#FF0000"))(100), 
         addCoef.col = "black", tl.col="black", tl.srt=6, number.cex=0.55, 
         tl.cex = 0.7, tl.offset = 1,  type='lower')

# assign variable to count how many highly correlated
# variables there exist based on 0.75 threshold
highCorr <- findCorrelation(frame_trump, cutoff = 0.75)

# find correlated names
highCorr_names <- findCorrelation(frame_trump, cutoff = 0.75, 
                                  names = TRUE)

cat("There are", 
    length(highCorr_names), 
    "highly correlated predictors. \n \n")

highCorr_names

#####################
### Scatter Plots ###
#####################

x1 = election$Trump; y1 = election$Clinton
corrplot1 <- ggplot(election, aes(x = x1, y = y1)) +
  ggtitle("Clinton vs. Trump by Fraction of Votes") +
  xlab('Trump') + ylab('Clinton') +
  geom_point(pch=1) +
  geom_smooth(method="lm", se=FALSE) +
  theme_classic() +
  # Add correlation coefficient
  stat_cor(method = "pearson", label.x = 0.05, label.y = 0.02)

x2 = election$PercentBelowPoverty; y2 = election$IncomeperCapita
corrplot2 <- ggplot(election, aes(x = x2, y = y2)) +
  ggtitle("Income Per Capita vs. Percent Below Poverty") +
  xlab('Percent Below Poverty') + ylab('Income Per Capita') +
  geom_point(pch=1) +
  geom_smooth(method="lm", se=FALSE) +
  theme_classic() +
  # Add correlation coefficient
  stat_cor(method = "pearson", label.x = 0.15, label.y = 0.20)

x3 = election$PercentBelowPoverty; y3 = election$HomeOwnership
corrplot3 <- ggplot(election, aes(x = x3, y = y3)) +
  ggtitle("Home Ownership vs. Percent Below Poverty") +
  xlab('Percent Below Poverty') + ylab('Home Ownership') +
  geom_point(pch=1) +
  geom_smooth(method="lm", se=FALSE) +
  theme_classic() +
  # Add correlation coefficient
  stat_cor(method = "pearson", label.x = 0.15, label.y = 10)

x4 = election$PercentBelowPoverty; y4 = election$PersonsPerHouse
corrplot4 <- ggplot(election, aes(x = x4, y = y4)) +
  ggtitle("Persons Per House vs. Percent Below Poverty") +
  xlab('Percent Below Poverty') + ylab('Persons Per House') +
  geom_point(pch=1) +
  geom_smooth(method="lm", se=FALSE) +
  theme_classic() +
  # Add correlation coefficient
  stat_cor(method = "pearson", label.x = 0.15, label.y = 0.20)

plot_grid(corrplot1, corrplot2, corrplot3, corrplot4, labels="AUTO", ncol = 2,
          align = "v")

# remove highly correlated predictors
election_new$Percent.White.Not.Hispanic <- NULL
election_new$Percent.foreign.born <- NULL 
election_new$PercentLangDiffEnglish <- NULL 
election_new$PercentWhite <- NULL 
election_new$Bachlorsorhigher <- NULL

###########################
### Partition The Data  ###
###########################

set.seed(222) # set seed for reproducibility
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(election_new), replace=TRUE, 
                 prob=c(0.7,0.3))
train  <- election_new[sample, ] # training set
test   <- election_new[!sample, ] # test set

cat("\n Training Dimensions:",dim(train),
    "\n Testing Dimensions:", dim(test), "\n",
    "\n Training Dimensions Percentage:", round(nrow(train)/
                                                  nrow(election_new),2),
    "\n Testing Dimensions Percentage:", round(nrow(test)/
                                                 nrow(election_new),2))

# Create a function to normalize the data by scaling it between 0 and 1
normalize <- function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Use the normalize function to normalize each column of train and test
# This creates a new dataframe by applying the normalize function to each row 
# of the dataset ‘frame’
maxmindtrain <- as.data.frame(lapply(train, normalize)) 
maxmindtest <- as.data.frame(lapply(test, normalize))

# Define input and output variables to create the training data data frame
input_train <- train[c(1:22)]
input_test <- test[c(1:22)]
output_train <- train$Trump # training output
output_test <- test$Trump # validation output
trainingdata <- cbind(input_train, output_train)
testdata <- cbind(input_test, output_test)

################################
### Generalized Linear Model ###
################################

set.seed(222)
lm.fit <- glm(Trump~., data=trainingdata)
summary(lm.fit)

###################################
### Simple Neural Network Model ###
###################################

set.seed(222)
n_train <- names(trainingdata)
n_test <- names(testdata)

f_train <- as.formula(paste("Trump ~", paste(n_train[!n_train %in% "Trump"], 
                                             collapse = " + ")))
f_test <- as.formula(paste("Trump ~", paste(n_test[!n_test %in% "Trump"], 
                                            collapse = " + ")))

# 2 hidden layers with 5 and 3 neurons, respectively
nn_train <- neuralnet(f_train,data=trainingdata,hidden=c(5,3),linear.output=T) 
nn_test <- neuralnet(f_test,data=testdata,hidden=c(5,3), linear.output=T) 
plot(nn_train, rep = "best") # plot the neural network - training data

# Predict on Training Data and Test Data

set.seed(222) 
# Compute fitted values from the training data
predictions_train <- predict(nn_train, newdata=trainingdata)

# Test the neural networks out of sample performance
predictions_test <- predict(nn_test, newdata=testdata)
# Compute mean absolute error between true and fitted values
# we are wrong on average by this many fraction of votes

train_mae = mean(abs(predictions_train-output_train)) 
test_mae = mean(abs(predictions_test-output_test)) 

cat('\n', 'Train MAE:', train_mae,
    '\n', 'Test MAE:', test_mae,
    '\n', 'Difference in MAE Between Train and Validation Set:', 
    train_mae - test_mae)

##################
#### Part Two ####
##################

h2o.init()

# Read in the data
housing <- read.csv(paste("https://raw.githubusercontent.com/lshpaner/",
                          "CEEM586_Neural_Networks_and_ML/main/data/",
                          "DC_PropertieResidentialunder1mill.csv", sep=""), 
                    header=TRUE)
head(housing[,1:10]) # examine first five columns of data

str(housing[1,]) # examine structure of dataframe

#######################
#### Preprocessing ####
#######################

# remove non-numeric variables s.t. amendable to ML modeling
housing$CENSUS_BLOCK <- NULL
housing$BATHRM <- NULL # unrounded expression of bathrooms
housing$CNDTN <- NULL
housing$EXTWALL <- NULL
housing$ROOF <- NULL
housing$INTWALL <- NULL
housing$ASSESSMENT_SUBNBHD <- NULL
# contains 53 levels (already as variables/columns)
housing$ASSESSMENT_NBHD <- NULL
housing$SQUARE <- NULL
housing$QUADRANT <- NULL
# remove logPrice since PRICE is the target, and we do not
# need to linearize it
housing$logPrice <- NULL
housing$X <- NULL; housing$Y <- NULL # GPS coordiantes (x, y) --> not necessary

# supply names of columns that have 0 variance
names(housing[, sapply(housing, function(v) var(v, na.rm=TRUE)==0)])

# exclude zero variance columns
housing <- housing[,sapply(housing, function(v) var(v, na.rm=TRUE)!=0)]
# dimensions of dataset
cat(" Dimensions of dataset:", dim(housing),
    "\n", "There are", sum(is.na(housing)), 
    "'NA' values in the entire dataset.")

# assign variable to count how many highly correlated
# variables there exist based on 0.75 threshold
highCorr_names <- findCorrelation(cor(housing, use="pairwise.complete.obs"),
                                  cutoff = 0.75, names = TRUE)

highCorr <- findCorrelation(cor(housing), cutoff = 0.75)
cat("\n There are", length(highCorr_names),
    "highly correlated predictors. \n \n")
highCorr_names

# remove highly correlated predictors
housing$NW <- NULL
housing$Ward6 <- NULL
housing$Multi <- NULL
housing$NUM_UNITS <- NULL
# independent variables
X_var <- colnames(housing); X_var <- list(colnames(housing))
# remove response variable[-16] and index[-1] from list
X_var <- X_var[[1]][-15]; X_var <- X_var[-1]; X_var


###############################
#### Partitioning The Data ####
###############################

# dataset is partitioned using a 70/30 train_test split as follows:
# make this example reproducible
set.seed(222)
seventy_percent = 0.70*nrow(housing) # what is 70% of length of dataframe?
ind <- sample(1:nrow(housing), seventy_percent)
train_data <- as.h2o(housing[ind,]) # create training set as h2o data frame
test_data <- as.h2o(housing[-ind,]) # create test set as h2o data frame
cat(" Training Dimensions:",dim(train_data),
    "\n Testing Dimensions:", dim(test_data), "\n",
    "\n Training Percentage:", round(nrow(train_data)/nrow(housing),2),
    "\n Testing Percentage:", round(nrow(test_data)/nrow(housing), 2))

# Estimate The Deep Neural Network

dl_DC_Properties1 <- h2o.deeplearning(y="PRICE", x=c(X_var), 
                                      training_frame=train_data,
                                      validation_frame=test_data, 
                                      activation="Tanh", 
                                      epochs=1000, hidden=c(4,4), 
                                      standardize=TRUE, 
                                      l1=0.0001, l2=0.001, 
                                      adaptive_rate=TRUE,
                                      variable_importances=TRUE,
                                      nfolds=3, reproducible=TRUE, 
                                      seed=222)

# Plot and Model Summary
plot(dl_DC_Properties1, metric='mae') # loss plotted throughout training
summary(dl_DC_Properties1) # Print model summary information

# Variable Importance
h2o.varimp_plot(dl_DC_Properties1, 10) # plot the first 10 important variables

# Retrieve the variable importance
varimp <- h2o.varimp(dl_DC_Properties1)
top_10 <- varimp[1:10,] # for data exploration
top_20 <- varimp[1:20,] # top 20 variables for subsequent modeling
top20_var <- top_20$variable
print(top_10) # print the top 10 variables and their respective importance

# Additional Exploratory Data Analysis (EDA) 

# The 20 most important variables are taken into consideration, but scatter 
# plots on the full dataset (not training) are created only for columns with 
# quantitative and continuous values.

# plot CENSUS-TRACT VS. PRICE
x5 = housing$CENSUS_TRACT; y5 = housing$PRICE
corrplot5 <- ggplot(housing, aes(x = x5, y = y5)) +
                    ggtitle("Price vs. Census Tract") +
                    xlab('Census Tract') + ylab('Price') +
                    geom_point(pch=1) +
                    geom_smooth(method="lm", se=FALSE) +
                    theme_classic() +
                    # Add correlation coefficient
                    stat_cor(method = "pearson", label.x = 3, label.y = 30)

# plot BATHROOMS VS. PRICE
x6 = housing$BATHROOM; y6 = housing$PRICE
corrplot6 <- ggplot(housing, aes(x = x6, y = y6)) +
                    ggtitle("Price vs. Bathrooms") +
                    xlab('Bathrooms') +
                    ylab('Price') +
                    geom_point(pch=1) +
                    geom_smooth(method="lm", se=FALSE) +
                    theme_classic() +
                    # Add correlation coefficient
                    stat_cor(method = "pearson", label.x = 0.5, label.y = 30)

plot_grid(corrplot5, corrplot6, labels="AUTO", ncol = 2, align = "v")

# Correlation Matrix
# Since we have already determined and omitted the highly correlated predictors 
# from the main dataframe, this is just another sanity check to confirm that no 
# more of them exist.

# create list from top 10 variables
list <- c(top_10['variable']) 

# subset top 10 variables into new df
top_ten_housing <- housing[c(top_10[,'variable'])]

# assign correlation function call to variable
cor_top_ten_housing <- cor(top_ten_housing, 
                           use="pairwise.complete.obs")

# plot the correlation table (matrix)
corrplot(cor_top_ten_housing, 
         method="color",
         col=colorRampPalette(c("#FC0320", 
                                "white", 
                                "#FF0000"))(200),
         addCoef.col = "black", 
         tl.col="black", 
         tl.srt=45, 
         type="lower", 
         number.cex = 0.8)

# Re-estimate The Deep Neural Network

dl_DC_Properties2 <- h2o.deeplearning(y="PRICE", x=c(top20_var),
                                      training_frame=train_data,
                                      validation_frame=test_data,
                                      activation="Tanh",
                                      # hidden_layer, node
                                      epochs=1000, hidden=c(2,2),
                                      standardize=TRUE, l1=0.0001,
                                      l2=0.01, adaptive_rate=TRUE,
                                      variable_importances=TRUE,
                                      nfolds=3, reproducible=TRUE,
                                      seed=222)

plot(dl_DC_Properties2, metric='mae') # training and test loss plotted
summary(dl_DC_Properties2) # print out model summary information and statistics

# Predict outputs on the test set
predictions<-h2o.predict(dl_DC_Properties2, test_data)

# print the predictions
print(predictions)

# Create data set for analysis with LIME
# Pick 5 indices from the training set
for_lime <- sample(1:nrow(housing[ind,]), 5)
data_for_lime <- housing[for_lime,]

# Fit Deep Neural Network
dl_DC_Properties3 <- h2o.deeplearning(y="PRICE", x=c(top20_var),
                                      training_frame=train_data,
                                      validation_frame=test_data,
                                      activation="Tanh",
                                      # hidden_layer, node
                                      epochs=1000, hidden=c(2,2),
                                      standardize=TRUE, l1=0.0001,
                                      l2=0.015, adaptive_rate=TRUE,
                                      variable_importances=TRUE,
                                      reproducible = TRUE,
                                      seed=222)

plot(dl_DC_Properties3, metric='mae') # training and test loss plotted
summary(dl_DC_Properties3) # print out model summary information and statistics

# Convert data_for_lime into an h2o data frame
predict_data_for_lime <- as.h2o(data_for_lime)
# Compute predictions with estimated neural network for the lime dataset
predictionsforlime <- h2o.predict(dl_DC_Properties3, predict_data_for_lime)

# Use lime to analyze the predictions
explainer_price <- lime(data_for_lime, dl_DC_Properties3)
explanation <- explain(data_for_lime, explainer_price, n_labels = 2,
                       n_features = 4)

print(explanation) # print explanation output
print(data_for_lime)

# Visualize the lime output
plot_features(explanation, ncol=1)
plot_explanations(explanation)

print(explanation)
price_prediction <- as.data.frame(explanation$prediction)
price_prediction <- as.numeric(unlist(price_prediction))
cat('\n', 'Mean Price Prediction:', mean(price_prediction),
    '\n', 'Mean Home Price:', mean(housing$PRICE, na.rm=TRUE),
    '\n', 'Difference:', mean(price_prediction)-mean(housing$PRICE, na.rm=TRUE),
    '\n', '% Difference:', 1-(mean(housing$PRICE, na.rm=TRUE)/
                                mean(price_prediction)))

plot(explanation$model_r2, main='Predictions: R-Squared', xlab='Index',
     ylab='R-Squared') # plot the model explanation

