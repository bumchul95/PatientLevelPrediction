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
#' @examples
#' setmodel <- setCatboost()
#' model <- fitPlp(trainData, setmodel, analysisId=1)
#' @export
setCatboost <- function(loss_function = 'Logloss',
                        iterations = c(100, 150),
                        depth = 5,
                        learning_rate = 0.1,
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
    iterations = iterations,
    depth = depth,
    learning_rate = learning_rate,
    l2_leaf_reg = l2_leaf_reg
  )

  param <- listCartesian(parameters)

  attr(param, "settings") <- list(
    modelType = "Catboost",
    modelName = "Catboost",
    seed = random_seed,
    loss_function = loss_function,
    auto_class_weights = auto_class_weights,
    varImpRFunction = "varImpCatboost",
    trainRFunction = "fitCatboost",
    predictRFunction = "predictCatboost"
  )

  attr(param, "saveType") <- "catboost"

  result <- list(
    fitFunction = "fitRclassifier",
    param = param
  )

  class(result) <- "modelSettings"

  return(result)
}

fitCatboost <- function(dataMatrix,
                        labels,
                        hyperParameters, # setmodel$param[[1]]
                        settings){ # attr(setmodel$param, "settings")
  set.seed(settings$seed)
  
  train_pool <- catboost::catboost.load_pool(
    data = as.matrix(dataMatrix),
    label = labels$outcomeCount
  )
  print(hyperParameters)
  model <- catboost::catboost.train(
    train_pool,
    params = list(
      loss_function = settings$loss_function,
      iterations = hyperParameters$iterations,
      depth = hyperParameters$depth,
      learning_rate = hyperParameters$learning_rate,
      auto_class_weights = settings$auto_class_weights,
      random_seed = settings$seed
    )
  )
  
  return (model)
}

varImpCatboost <- function(model,
                           covariateMap) { # result$covariateMap
  varImp <- catboost::catboost.get_feature_importance(model)

  temp <- varImp
  for (i in 1:nrow(temp)){
    temp[[i]] = i
  }
  
  varImp <- data.frame(
    covariateId = temp,
    covariateValue = varImp,
    included = 1
  )
  
  varImp <- merge(covariateMap, varImp, by.x = "columnId", by.y = "covariateId")
  varImp <- varImp %>%
    dplyr::select("covariateId", "covariateValue", "included")
  
  return(varImp)
}

predictCatboost <- function(plpModel, # model...
                            data, # testData
                            cohort) { # label
  if (inherits(data, "plpData")) {
    # convert
    matrixObjects <- toSparseM(
      plpData = data,
      cohort = cohort,
      map = plpModel$covariateImportance %>%
        dplyr::select("columnId", "covariateId")
    )
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

  test_pool <- catboost::catboost.load_pool(
    data = as.matrix(newData),
    label = cohort$outcomeCount
  )

  pred <- catboost::catboost.predict(model, test_pool)

  prediction <- cohort
  prediction$value <- pred

  prediction <- prediction %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = originalRowId)

  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))

  return(prediction)
}
