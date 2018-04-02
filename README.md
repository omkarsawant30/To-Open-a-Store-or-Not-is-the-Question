# Retail-Store
To predict if a store should be opened in a particular area depending on sales, population, area, etc.

This data set is related with retail domain and challenge is to predict whether a store should get opened or not based on certain factors such as sales, population,area etc.

We have given you two datasets , store_train.csv and store_test.csv . You need to use data store_train to build predictive model for response variable ‘store’. store_test data contains all other factors except ‘store’, you need to predict that using the model that you developed and submit your predicted values in a csv files.

You have to submit the probability scores, not the hard classes.

If you are using decision trees or random forest here, probability scores can be calculated as

score=predict(rf_model,newdata= testdata, type="prob")[,1]

score=predict(tree_model,newdata= testdata, type=‘vector’)[,1]
