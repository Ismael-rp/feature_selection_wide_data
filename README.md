# When is resampling beneficial for feature selection with imbalanced wide data?

In this repository are stored the feature selection algorithms used in article *When is resampling beneficial for feature selection with imbalanced wide data?* , the stored algorithms are:

- Chi squared
- Information gain
- Relief
- ANOVA
- Random forest importance
- T-test
- SVM-RFE


# How to use


Run *requeriments.R* which will install the necessary libraries.

```Rscript requeriments.R```

Once the libraries are installed, inside out code we must import the functions before using them.

```source("featureSelectors.R")```

These functions receive as input a list with element "d" as the dataset and "tag" as its tags, function *partitionDataTag* can be used to format any dataframe in that way, taking into account that the tag is placed in the last column.


```
df = iris[1:100,]
df = partitionDataTag(df)
```

Finally we can launch any of the following algorithms:
```
fSelection_t_test(data)
fSelection.filter.anova(data)
fSelection.filter.chi.squared(data)
fSelection.filter.information.gain(data)
fSelection_random_forest_importance(data)
fSelection.filter.relief(data)
fSelection_svm_rfe(data)
```

The complete example can be seen on file ```example.R```
