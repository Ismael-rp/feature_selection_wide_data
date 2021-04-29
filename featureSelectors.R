library(FSelector)

# fSelection_filter_anova
library(mlr3)
library(mlr3filters)

# fSelection_t_test
library(sigFeature)




#' Returns all columns from a dataset but the tag, which must be placed on the last column
#' 
getDataCol = function(data){
  data[-ncol(data)]
}

#' Returns tag column from a dataset, it must be placed on the last column
#' 
getTagCol = function(data){
  data[,ncol(data)]
}

#' Returns the same data separating the tag from the rest, tag column must be
#' the last one
#' 
#' @return list whit parameters:
#' "d" with the dataset and
#' "tag" with it tags
#'  
partitionDataTag = function(data){
  return(list(d=getDataCol(data), tag=getTagCol(data)))
}


#' Chi squared feature selector from FSelector package
#' 
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be and/or NUMERICS
#'  tag feature must be NUMERIC or CATEGORICAL
#'
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their ranking 
#'
fSelection_filter_chi_squared = function(data){
  
  data = cbind(data$d, tag = data$tag)
  weights <- FSelector::chi.squared(tag~. , data)
  
  return( weights[order(-weights$attr_importance),,drop = FALSE] )
}


#' Information gain feature selector from FSelector package
#'   
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be and/or NUMERICS
#'  tag feature must be NUMERIC or CATEGORICAL
#'
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their ranking
#'
fSelection_filter_information_gain = function(data){
  
  data = cbind(data$d, tag = data$tag)
  weights <- FSelector::information.gain(tag~. , data)
  
  return( weights[order(-weights$attr_importance),,drop = FALSE] )
}


#' Relief feature selector from FSelector package
#'
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be and/or NUMERICS
#'  tag feature must be NUMERIC or CATEGORICAL
#'  
#' @param neighbours.count:
#'
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their ranking
#' 
fSelection_filter_relief = function(data, neighbours.count = 1){
  
  data = cbind(data$d, tag=data$tag)
  weights <- FSelector::relief(tag~. , data, neighbours.count = neighbours.count)
  
  return( weights[order(-weights$attr_importance),,drop = FALSE] )
}


#' Random forest feature selector from FSelector package
#' 
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be and/or NUMERICS
#'  tag feature must be NUMERIC or CATEGORICAL
#'  
#' @param importance.type: It can have 2 values:
#'     1: reduction in mean predictive reliability
#'     2: reduction in the mean impurity of nodes
#' 
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their ranking
#'
fSelection_random_forest_importance = function(data, importance.type=1){
  data = cbind(data$d, tag=data$tag)
  weights = FSelector::random.forest.importance(tag~., data,
                                                importance.type=importance.type)
  return( weights[order(-weights$attr_importance),,drop = FALSE] )
}




#' Feature selector based on the t-test from sigFeature package
#'
#' @param data: List with element "d" with the dataset and "tag" with its labels
#'  dataset features must be NUMERICS
#'  tag feature must be NUMERIC or CATEGORICAL
#'
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their p-value (the lower the p-value the greater the importance)
#'
fSelection_t_test = function(data){
  
  weights = sigFeaturePvalue(data$d,data$tag)
  
  # To dataframe
  weights = data.frame(unlist(weights), row.names = names(data$d))
  colnames(weights) = c("attr_importance")
  
  weights[order(weights$attr_importance),,drop = FALSE]
}





#' Wrapper for the filters feature selectors from the mlr3filters library,
#' instead of using the type parameter to choose the type of algorithm,
#' it is chosen with the name of the function.
#'  
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be NUMERICS
#'  tag feature must be CATEGORICAL
#'  
mlr3filters_filter = function(data, type){
  
  data = as.data.frame(unclass(data))
  data$tag = as.factor(data$tag)
  
  # We create a task for the data
  task = TaskClassif$new("data", backend = data, target = "tag")
  
  filter = flt(type)
  weights = as.data.table(filter$calculate(task))
  
  # Format data.frame out
  weights = data.frame(attr_importance=weights$score, row.names=weights$feature)
  
  # Remove the start characters from the name attributes "d."
  rownames(weights) = 
    sapply(rownames(weights), function(x) substr(x,3,nchar(x)))
  
  
  return( weights[order(-weights$attr_importance),,drop = FALSE] )
}




#' ANOVA feature selector from mlr3filters package
#' 
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be NUMERICS
#'  tag feature must be CATEGORICAL
#'
#' @return dataset with the names of the parameters ordered from best to worst
#' alongside their p-value (the lower the p-value the greater the importance)
#'
fSelection_filter_anova = function(data){
  weights = mlr3filters.filter(data, "anova")
  
  # Pasarlo a p-value y reordenar
  weights$attr_importance = 10^(-weights$attr_importance)
  
  return(weights[order(weights$attr_importance),,drop = FALSE])
}


#'Algoritmo selector de características SVM-RFE (Recursive Feature Elimination)
#'
#' @param data: List with element "d" with the dataset and "tag" with its tags
#'  dataset features must be NUMERICS
#'  tag feature must be NUMERICS or CATEGORICAL and have 2 levels
#'   
#' @return vector with the order of the parameters ordered from best to worst
#'
fSelection_svm_rfe = function(data){
  
  # Pasamos a factor, borramos niveles no usados por si ya lo fuera
  data$tag = as.factor(data$tag)
  data$tag = droplevels(data$tag)
  
  if (length(levels(data$tag)) != 2 ) 
    stop("Number of classes must be 2, not ", length(levels(data$tag)))
  
  orderFeatures = svmrfeFeatureRanking(data$d, data$tag)
  names(data$d)[orderFeatures]
}











