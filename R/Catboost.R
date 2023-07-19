# @file Catboost.R
# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#' Create setting for gradient boosting machine model using catboost (https://github.com/catboost/catboost/tree/6720dc89298b58f608a1b4ec5eece2b7a24f5281/catboost/R-package).
#' Default value: Required argument
#' @param label The label vector or label matrix
#' @param cat_features A vector of categorical features indices.
#' The indices are zero based and can differ from the given in the Column descriptions file.
#' @param column_description The path to the input file that contains the column descriptions.
#' @param pairs A file path, matrix or data.frame that contains the pairs descriptions. The shape should be Nx2, where N is the pairs' count.
#' The first element of pair is the index of winner document in training set. The second element of pair is the index of loser document in training set.
#' @param delimiter Delimiter character to use to separate features in a file.
#' @param has_header Read column names from first line, if this parameter is set to True.
#' @param weight The weights of the objects.
#' @param group_id The group ids of the objects.
#' @param group_weight The group weight of the objects.
#' @param subgroup_id The subgroup ids of the objects.
#' @param pairs_weight The weights of the pairs.
#' @param baseline Vector of initial (raw) values of the objective function.
#' Used in the calculation of final values of trees.
#' @param feature_names A list of names for each feature in the dataset.
#' @param thread_count The number of threads to use while reading the data. Optimizes reading time. This parameter doesn't affect results.
#' If -1, then the number of threads is set to the number of CPU cores.
#'
#' @examples
#' @export
setCatboost <- function(loss_function = 'Logloss', #
                        iterations = 150,
                        depth = 5, #
                        learning_rate = 0.01,
                        l2_leaf_reg = 3,
                        auto_class_weights = 'None',
                        random_seed = sample(10000000, 1)){
  ensure_installed("catboost")
  checkIsClass(random_seed, c("numeric", "integer"))
  if (!inherits(x = random_seed, what = c("numeric", "integer"))) {
    stop("Invalid seed")
  }
  if(sum(iterations < 1) > 0){
    stop('numIterations must be greater that 0')
  }
  if(sum(learning_rate <= 0) > 0){
    stop('learning_rate must be greater that 0')
  }
  if (sum(l2_leaf_reg < 0) > 0){
    stop('l2_leaf_rag must be 0 or greater')
  }
  if(sum(list('None', 'Balanced', 'SqrtBalanced') %in% auto_class_weights) == 0){
    stop('auto_class_weights must be None, Balanced, or SqrtBalanced')
  }
  
  parameters <- list(
    loss_function = loss_function,
    iterations = iterations,
    depth = depth,
    learning_rate = learning_rate,
    l2_leaf_reg = l2_leaf_reg,
    auto_class_weights = auto_class_weights,
    random_seed = random_seed
  )

  param <- listCartesian(parameters)
  
  attr(param, "settings") <- list(
    modelType = "Catboost",
    seed = random_seed,
    modelName = "Catboost",
    varImpRFunction = "varImpCatboost",
    trainRFunction = "fitCatboost",
    predictRFunction = "predictCatboost"
  )
  
  attr(param, "saveType") <- "catboost"
  
  result <- list(
    fitFunction = "fitCatboost",
    param = param
  )

  class(result) <- "modelSettings"
  
  return(result)
}


predictCatboost <- function(plpModel,
                            data,
                            cohort){
  if (inherits(data, "plpData")) {
    # convert
    matrixObjects <- toSparseM(
      plpData = data,
      cohort = cohort,
      map = plpModel$covariateImportance %>%
        dplyr::select("columnId", "covariateId")
    )
    
    # use the include??
    
    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
  } else {
    newData <- data
  }
  
  if (inherits(plpModel, "plpModel")) {
    model <- plpModel$model
  } else {
    model <- plpModel
  }
  
  # pred <- data.frame(value = catboost::catboost.predict(model, newData))
  # prediction <- cohort
  # prediction$value <- pred$value
  # 
  # prediction <- prediction %>%
  #   dplyr::select(-"rowId") %>%
  #   dplyr::rename(rowId = "originalRowId")
  # 
  # attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))
  # 
  # return(prediction)
  
}


fitCatboost <- function(trainData,
                        labels,
                        modelSettings){
  param <- modelSettings$param
  # settings <- attr(param, 'settings')
  #print(trainData)
  print(plpModel)
  train_pool <- catboost::catboost.load_pool(
    data = trainData,
    label = labels$outcomeCount
  )
  print(1)
  outcomes <- sum(labels$outcomeCount > 0)
  N <- nrow(labels)
  outcomeProportions <- outcomes / N
  model <- catboost::catboost.train(
    data = train_pool,
    param = list(
      loss_function = param$loss_function,
      iterations = param$iterations,
      depth = param$depth,
      learning_rate = param$learning_rate,
      auto_class_weights = param$auto_class_weights,
      random_seed = param$random_seed
    )
  )

  return (model)
}

# train_pool <- catboost::catboost.load_pool(
#   data = tt,
#   label = trainData$labels
# )
#      data <-result$data[population$rowId,,]
#list(label = population$outcomeCount)
# folder <- tempfile()
# dir.create(folder)
# 
# exportToCsv(folder)
# list.files(folder)
# link = folder
# 
# connectionDetails <- getEunomiaConnectionDetails()
# connection <- connect(connectionDetails)
# querySql(connection, "SELECT COUNT(*) FROM person;")
# querySql(connection, "SELECT* FROM person WHERE PERSON_ID=1;")
# querySql(connection, "SELECT* FROM condition_occurrence;")
# querySql(connection, "SELECT* FROM condition_occurrence WHERE condition_concept_id=435783;") #schizo
# querySql(connection, "SELECT* FROM condition_occurrence WHERE condition_concept_id=320128;") #HTN