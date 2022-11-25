# Neural Networks and Machine Learning
## Instructions:  
In this project, you will:  

* Examine what common architectures are and activation functions for neural networks  
* Examine how to optimize the parameters in a neural network  
* Make predictions using neural networks in R  
* Practice Deep Learning using R  
* Apply ideas for cross-validation for neural network model development and validation  
* Tune parameters in a neural network using a grid search  
* Use the package Lime in R to recognize which variables are driving the recommendations your neural
network is making  

## Part One - Projecting Data for Neural Networks    

In this part of the project, you will project data for neural networks using the file **ElectionData.csv**, which contains the fraction of votes by county earned by President Trump and Secretary Clinton in the 2016 US Presidential election, sorted by county FIPS code (FIPS stands for **F**ederal **I**nformation **P**rocessing **S**ystem, and is a geographic identifier). The data file also includes several variables that may be useful to predict these fractions. Use the data to develop a neural network model for either individual.

This part of the project requires some work in RStudio, located on the project page in Canvas. Use that space, along with the provided script and data file, to perform the work, then use this document to answer questions on what you discover.

1. How well did your model predict the election results?  
There were five highly correlated predictors above a threshold of *r* = 0.75 which are omitted from
the model. They were as follows: "Percent.White.Not.Hispanic", "Percent.foreign.born",
"PercentLangDiffEnglish", "PercentWhite", and "Bachlorsorhigher."

Training Dimensions: 2186 27  
Testing Dimensions: 957 27  
Training Dimensions Percentage: 0.7  
Testing Dimensions Percentage: 0.3  

<img src = "https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-5-1.png">

2. Do you think your model will generalize well to new data? Why or why not?  

3. What could you do to improve the model?  

4. In the space below, describe your neural network structure (i.e., how many hidden layers were in the network?). How many nodes are in each hidden layer? Which activation function 

## Part Two

In this part of the course project, you will use H2O and LIME for neural network predictions on a data set predicting the sale prices of houses. This part of the project requires some work in RStudio, located on the project page in Canvas. Use that space, along with the provided script and data file, to perform the work, then use this document to record your accuracy measures.

**Additional information:**  
Many attributes in this dataset are categorical. For example, ASSESSMENT_NBHD (assessment neighborhood) will need to be converted to numerical data from a neighborhood name. Consider carefully how you might do this. There are also a lot of variables, and it will be easy to overfit your model. To this end, in the `deeplearning()` function in H2O, there is a setting called L2. Investigate this setting and see if you find it valuable. There is also a function `h2o.varimp_plot()` that will create a histogram of the relative importance of all the inputs to your model. That might help you decide how to select variables for inclusion in your final model.
When constructing your model, also consider the following:  

* Explore the data set with scatter plots and compute the correlation matrix.  
* Split the entire dataset into a training and test set, and both should be converted into H2O data frames. The test set should contain a random sample of roughly 30% of the entire data.  
* Select several data points from the training data set to analyze with LIME later on.  
* Fit the neural network using `h2o.deeplearning()`  
     * Decide which independent variables to include.  
     * Decide how many hidden layers to include in the network.  
     * Decide how many nodes will be in each hidden layer.  
     * Decide which activation function to use (for reference, h2o supports the following: "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout" ).  
     * Specify whether or not to use an adaptive learning rate (argument: adaptive_rate).  
     * L1 regularization can result in many weights being set to 0, add stability, and improve generalization. Set the l1 argument (larger values correspond to more regularization).  
     * L2 regularization can result in many weights being set to small values, add stability, and improve generalization. Set the l2 argument (larger values correspond to more regularization). Note: if the L1 coefficient is higher than the L2 coefficient, the algorithm will favor L1 regularization and vice versa.  
     * Specify how many epochs the training algorithm should run for.  
     * Set the random seed argument (seed) to guarantee the same results each time you run the training algorithm.  
* Plot the training and validation loss vs. the training epochs.  
* Use the `summary()` function to examine the fit model.  
* Use the data points selected earlier to compute and analyze the neural network's predictions using the `lime()` function from the lime package.  
     * Visualize the results of this analysis.  
* In this course project document, analyze the model and summarize your findings. How well does your model predict house price? Do you think it will generalize well to new data? Which variables ended up being most important? What could be done to improve the model?  

1. How well did your model predict the house prices?  

2. Do you think your model will generalize well to new data? Why or why not?  

3. Which variables ended up being the most important and why do you think this is so?  

4. What could you do to improve the model?  

5. In the space below, describe your neural network structure (i.e., which independent variables did you use?). How many hidden layers were in the network? How many nodes were in each hidden layer? Which activation function did you use? Did you use an adaptive learning rate? How many epochs did the training algorithm run for?  