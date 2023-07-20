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
#' @export
setCatboost <- function(loss_function = 'Logloss', #
                        iterations = 150,
                        depth = 5, #
                        learning_rate = 0.1,
                        l2_leaf_reg = 3,
                        auto_class_weights = 'None',
                        random_seed = sample(10000000, 1)){
  ensure_installed("catboost")
  ensure_installed("reshape2")
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

  param <- parameters
  
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



# Training
fitCatboost <- function(trainData,
                        modelSettings,
                        search = 'none',
                        analysisId){

  param <- modelSettings$param
  melt_test <- as.data.frame(trainData$covariateData$covariates)
  x_train = dcast(melt_test, rowId ~ covariateId)
  x_train[is.na(x_train)] <- 0
  
  train_pool <- catboost::catboost.load_pool(
    data = x_train,
    label = trainData$labels$outcomeCount
  )
  
  model <- catboost::catboost.train(
    train_pool,
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





# matrixObjects <- toSparseM(
#   plpData = data,
#   cohort = cohort,
#   map = plpModel$covariateImportance %>%
#     dplyr::select("columnId", "covariateId")
# )
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