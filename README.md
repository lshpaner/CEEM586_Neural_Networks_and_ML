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

**The mean absolute error between true and fitted values was 0.1349895 fraction of votes, so it was a fairly small error, thereby rendering the model predictions sound and proper. Prior to calling on the neural network, a generalized linear model is fitted with the training data. The following output is obtained. All features are statistically significant at an  $\alpha$ = 0.05 significance level. The mean absolute error between true and fitted values was 0.1349895 fraction of votes, so it was a fairly small error, thereby rendering the model predictions sound and proper.**

2. Do you think your model will generalize well to new data? Why or why not?  

**The model should generalize well to new data because the mean absolute error on the out-of-sample (test) data is 0.1285449 (in close proximity to that of the training MAE (0.1362437)). There is a negligible difference between the two of 0.007698762.**

3. What could you do to improve the model?  

**Apart from eliminating the highly correlated predictors, various additional feature reduction techniques like PCA (Principal Component Analysis) can be applied. Furthermore, the number of hidden layers and nodes can be changed and the model hyperparameters tuned.**

4. In the space below, describe your neural network structure (i.e., how many hidden layers were in the network?). How many nodes are in each hidden layer? Which activation function did you use? Which independent variables did you include?  

**The neural network structure includes 2 hidden layers with 5 and 3 nodes, respectively:**
<br>
<!-- HTML generated using hilite.me --><div style="background: #ffffff; overflow:auto;width:auto;border:solid gray;border-width:.1em .1em .1em .8em;padding:.2em .6em;"><pre style="margin: 0; line-height: 125%">nn_train <span style="color: #333333">&lt;-</span> neuralnet(f_train, data<span style="color: #333333">=</span>trainingdata, hidden<span style="color: #333333">=</span>c(<span style="color: #0000DD; font-weight: bold">5</span>,<span style="color: #0000DD; font-weight: bold">3</span>), linear.output<span style="color: #333333">=</span>T)
</pre></div>

<p align = "center"> <img src ="https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-13-1.png"></p>

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
**The model is re-trained with the top 20 features over the same hyperparameters as the original model. There exists a narrower gap between the training and validation MAE scores over roughly the first twenty epochs. However, the gap progressively widens.**  

<p align = "center"> <img src = "https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-33-1.png"> </p>

**LIME Analysis** 

**Using only the top 20 input features and the LIME package library, a substantial amount of variation is explained by the data according to the  values - the highest of which is 0.71, but starts off as 0.68, and then increases, but decreases in a step-wise pattern to .65, until it gradually drops off and reaches 0.43, a moderate amount of variation.**  

<p align = "center"> <img src = "https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-45-1.png"> </p>

<center>

<!-- HTML generated using hilite.me --><div style="background: #f8f8f8; overflow:auto;width:fit-content;border:border-width:.1em .1em .1em .8em;padding:.6em .6em;"><pre style="margin: 0; line-height: 125%"><span style="color: #000000">## # A tibble: 20 × 11</span>
<span style="color: #000000">##    model_type case  model_r2 model_int…¹ model…² feature featu…³ featu…⁴ featu…⁵</span>
<span style="color: #000000">##    &lt;chr&gt;      &lt;chr&gt;    &lt;dbl&gt;       &lt;dbl&gt;   &lt;dbl&gt; &lt;chr&gt;     &lt;dbl&gt;   &lt;dbl&gt; &lt;chr&gt;  </span>
<span style="color: #000000">##  1 regression 5009     0.684     517791. 513531. Ward        4    2.11e5 Ward &lt;…</span>
<span style="color: #000000">##  2 regression 5009     0.684     517791. 513531. BATHRO…     2.5 -1.13e5 BATHRO…</span>
<span style="color: #000000">##  3 regression 5009     0.684     517791. 513531. FIREPL…     0   -7.83e4 FIREPL…</span>
<span style="color: #000000">##  4 regression 5009     0.684     517791. 513531. LANDAR…  1658   -2.39e4 LANDAR…</span>
<span style="color: #000000">##  5 regression 480      0.710     729583. 304552. Ward        8   -2.09e5 5 &lt; Wa…</span>
<span style="color: #000000">##  6 regression 480      0.710     729583. 304552. BATHRO…     1   -1.12e5 BATHRO…</span>
<span style="color: #000000">##  7 regression 480      0.710     729583. 304552. FIREPL…     0   -8.24e4 FIREPL…</span>
<span style="color: #000000">##  8 regression 480      0.710     729583. 304552. LANDAR…  1541   -2.11e4 LANDAR…</span>
<span style="color: #000000">##  9 regression 7067     0.647     430790. 640012. Ward        5    2.07e5 Ward &lt;…</span>
<span style="color: #000000">## 10 regression 7067     0.647     430790. 640012. BATHRO…     3.5  9.97e4 3.0 &lt; …</span>
<span style="color: #000000">## 11 regression 7067     0.647     430790. 640012. FIREPL…     0   -8.21e4 FIREPL…</span>
<span style="color: #000000">## 12 regression 7067     0.647     430790. 640012. LANDAR…  1680   -1.52e4 1658 &lt;…</span>
<span style="color: #000000">## 13 regression 5089     0.664     418250. 692433. Ward        5    2.06e5 Ward &lt;…</span>
<span style="color: #000000">## 14 regression 5089     0.664     418250. 692433. BATHRO…     3.5  1.00e5 3.0 &lt; …</span>
<span style="color: #000000">## 15 regression 5089     0.664     418250. 692433. FIREPL…     0   -7.84e4 FIREPL…</span>
<span style="color: #000000">## 16 regression 5089     0.664     418250. 692433. LANDAR…  6200    4.66e4 2926 &lt;…</span>
<span style="color: #000000">## 17 regression 4938     0.433     401948. 824680. Ward        4    2.11e5 Ward &lt;…</span>
<span style="color: #000000">## 18 regression 4938     0.433     401948. 824680. EYB      1947   -2.16e4 EYB &lt;=…</span>
<span style="color: #000000">## 19 regression 4938     0.433     401948. 824680. BATHRO…     3    1.29e4 2.5 &lt; …</span>
<span style="color: #000000">## 20 regression 4938     0.433     401948. 824680. YearSo…  2016    2.21e5 2016 &lt;…</span>
<span style="color: #000000">## # … with 2 more variables: data &lt;list&gt;, prediction &lt;dbl&gt;, and abbreviated</span>
<span style="color: #000000">## #   variable names ¹​model_intercept, ²​model_prediction, ³​feature_value,</span>
<span style="color: #000000">## #   ⁴​feature_weight, ⁵​feature_desc</span>
</pre></div>

</center>

**Moreover, the mean price prediction of $578,529.80 differs by only $19,528.26 from the actual mean housing price of $598,058.10. That is an almost negligible difference of approximately 3%, so the model predicted well.**

**The ensuing plots show how five random features explain the model’s fit with a mean of 63%.**

<p align="center">
<img src = "https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-42-1.png"> </p>

<p align="center">
<img src = "https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-43-1.png"> </p>

2. Do you think your model will generalize well to new data? Why or why not?  
**The 613.31 difference in the mean absolute error between the train (80,587.15) and validation (81,200.46) sets is negligible. However, at each time step of the epoch progression, that difference becomes wider, and thus, will not generalize well to new data unless the epochs are limited.**

3. Which variables ended up being the most important and why do you think this is so?  
**Using the `var_imp()` function call, the following variables ended up being most important:**

<p align="center">
<img src ="https://github.com/lshpaner/CEEM586_Neural_Networks_and_ML/blob/main/code/figs/unnamed-chunk-28-1.png"> </p>

<center>

<!-- HTML generated using hilite.me --><div style="background: #f8f8f8; overflow:auto;width:fit-content;border:border-width:.1em .1em .1em .8em;padding:.6em .6em;"><pre style="margin: 0;text-align:left; line-height: 125%"><span style="color: #000000">## Variable Importances: </span>
<span style="color: #000000">##           variable relative_importance scaled_importance percentage</span>
<span style="color: #000000">## 1             Ward            1.000000          1.000000   0.162422</span>
<span style="color: #000000">## 2              EYB            0.584610          0.584610   0.094954</span>
<span style="color: #000000">## 3         OldCity1            0.581093          0.581093   0.094382</span>
<span style="color: #000000">## 4        BATHROOMS            0.456646          0.456646   0.074170</span>
<span style="color: #000000">## 5               NE            0.403440          0.403440   0.065528</span>
<span style="color: #000000">## 6            Ward4            0.322812          0.322812   0.052432</span>
<span style="color: #000000">## 7      CapitolHill            0.273834          0.273834   0.044477</span>
<span style="color: #000000">## 8            Ward7            0.268182          0.268182   0.043559</span>
<span style="color: #000000">## 9         LANDAREA            0.250221          0.250221   0.040641</span>
<span style="color: #000000">## 10        OldCity2            0.237900          0.237900   0.038640</span>
<span style="color: #000000">## 11 CongressHeights            0.221452          0.221452   0.035969</span>
<span style="color: #000000">## 12       RowInside            0.216636          0.216636   0.035187</span>
<span style="color: #000000">## 13        Deanwood            0.206530          0.206530   0.033545</span>
<span style="color: #000000">## 14          RowEnd            0.198102          0.198102   0.032176</span>
<span style="color: #000000">## 15      FIREPLACES            0.194298          0.194298   0.031558</span>
<span style="color: #000000">## 16           Ward3            0.185223          0.185223   0.030084</span>
<span style="color: #000000">## 17        YearSold            0.178580          0.178580   0.029005</span>
<span style="color: #000000">## 18       RiggsPark            0.165993          0.165993   0.026961</span>
<span style="color: #000000">## 19       Eckington            0.108953          0.108953   0.017696</span>
<span style="color: #000000">## 20           Ward5            0.102287          0.102287   0.016614</span>
</pre></div>


</center>

**Number of bathrooms and bedrooms, among other features are important predictors in real estate data because they drive price.**

4. What could you do to improve the model?  
**Additional preprocessing like one-hot encoding other columns with many classes could prove beneficial. Understanding the dataset better would also lead to better preprocessing, which would ultimately lead to better model creation. Additional iterations with cross-validation over 5 folds and a more robust hyperparameter tuning mechanism like that of grid search could be implemented. Grid search on a CPU with 4 cores and 8 logical processors at a base speed of 2.30 GHz would not do this endeavor any justice. To this end, it may be necessary to purchase a more powerful computer with gaming GPUs.**

5. In the space below, describe your neural network structure (i.e., which independent variables did you use?). How many hidden layers were in the network? How many nodes were in each hidden layer? Which activation function did you use? Did you use an adaptive learning rate? How many epochs did the training algorithm run for?  
**The final (third iteration) model’s neural network structure includes all twenty of the top most important features listed in number 4. 2 hidden layers and 2 nodes were used, respectively, with an activation function of Tanh. The adaptive learning rate was passed in and set to TRUE, even though it is set to TRUE by default. The model was iterated through 1,000 epochs.**