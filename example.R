

source("featureSelectors.R")

# Create data based on iris dataset, we only select 2 classes since svm_rfe only
# works this way
data = iris[1:100,]

data = partitionDataTag(data)

fSelection_t_test(data)
fSelection.filter.anova(data)
fSelection.filter.chi.squared(data)
fSelection.filter.information.gain(data)
fSelection_random_forest_importance(data)
fSelection_filter_relief(data)
fSelection_svm_rfe(data)

