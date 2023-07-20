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
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}

setCatboost <- function(loss_function = 'Logloss', #
                        iterations = 150,
                        depth = 5, #
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
    loss_function = loss_function,
    iterations = iterations,
    depth = depth,
    learning_rate = learning_rate,
    l2_leaf_reg = l2_leaf_reg,
    auto_class_weights = auto_class_weights,
    random_seed = random_seed
  )

  #param <- parameters
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
    fitFunction = "fitRclassifier", ###########catboost?
    param = param
  )

  class(result) <- "modelSettings"
  
  return(result)
}



# Training
fitCatboost <- function(dataMatrix,
                        labels,
                        hyperParameters, # setmodel$param[[1]]
                        search = 'none',
                        analysisId){

  #param <- modelSettings$param
  
  train_pool <- catboost::catboost.load_pool(
    data = as.matrix(dataMatrix),
    label = labels$outcomeCount
  )
  
  model <- catboost::catboost.train(
    train_pool,
    params = list(
      loss_function = hyperParameters$loss_function,
      iterations = hyperParameters$iterations,
      depth = hyperParameters$depth,
      learning_rate = hyperParameters$learning_rate,
      auto_class_weights = hyperParameters$auto_class_weights,
      random_seed = hyperParameters$random_seed
    )
  )

  return (model)
}

varImpCatboost <- function(model,
                           covariateMap) { # result$covariateMap
  varImp <- catboost.get_feature_importance(model)

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