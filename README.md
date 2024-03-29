# When is resampling beneficial for feature selection with imbalanced wide data?

In this repository are stored the feature selection algorithms used in article *When is resampling beneficial for feature selection with imbalanced wide data?* , the stored algorithms are:

- T-test
- ANOVA
- Chi squared
- Information gain
- Random forest importance
- Relief
- SVM-RFE


# How to use


Run *requeriments.R* which will install the necessary libraries.

```Rscript requeriments.R```

Once the libraries are installed, inside out code we must import the functions before using them.

```
source("featureSelectors.R")
```

These functions receive as input a list with element "d" as the dataset and "tag" as its tags, function *partitionDataTag* can be used to format any dataframe in that way, taking into account that the tag is placed in the last column.


```
df = iris[1:100,]
df = partitionDataTag(df)
```

Finally we can launch any of the following algorithms:
```
fSelection_t_test(data)
fSelection_filter_anova(data)
fSelection_filter_chi.squared(data)
fSelection_filter_information.gain(data)
fSelection_random_forest_importance(data)
fSelection_filter_relief(data)
fSelection_svm_rfe(data)
```

The complete example can be seen on file ```example.R```

# Cite this article

```
@article{ramos2022resampling,
  title={When is resampling beneficial for feature selection with imbalanced wide data?},
  author={Ramos-P{\'e}rez, Ismael and Arnaiz-Gonz{\'a}lez, {\'A}lvar and Rodr{\'\i}guez, Juan J and Garc{\'\i}a-Osorio, C{\'e}sar},
  journal={Expert Systems with Applications},
  volume={188},
  pages={116015},
  year={2022},
  publisher={Elsevier}
}
```



