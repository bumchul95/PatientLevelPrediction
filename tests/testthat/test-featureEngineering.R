# Copyright 2021 Observational Health Data Sciences and Informatics
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

library("testthat")
context("FeatureEngineering")


testFEFun <- function(type = 'none'){
  
  result <- createFeatureEngineeringSettings(type = type)
  
  return(result)
}

  
test_that("createFeatureEngineeringSettings correct class", {
  
  featureEngineeringSettings <- testFEFun()
  
  expect_is(featureEngineeringSettings, 'featureEngineeringSettings')
  
  checkFun <- 'sameData'  # this is the only option at the moment, edit this when more are added
  expect_equal(attr(featureEngineeringSettings, "fun"), checkFun)
  
})


testUniFun <- function(k = 100){
  
  result <- createUnivariateFeatureSelection(k = k)
  
  return(result)
}



test_that("createUnivariateFeatureSelection correct class", {
  k <- sample(1000,1)
  featureEngineeringSettings <- testUniFun(k = k)
  
  expect_is(featureEngineeringSettings, 'featureEngineeringSettings')
  expect_equal(featureEngineeringSettings$k, k)
  expect_equal(attr(featureEngineeringSettings, "fun"), 'univariateFeatureSelection')
  
  expect_error(testUniFun(k = 'ffdff'))
  expect_error(testUniFun(k = NULL))
  expect_error(testUniFun(k = -1))
})


test_that("univariateFeatureSelection", {
  
  k <- 20+sample(10,1)
  featureEngineeringSettings <- testUniFun(k = k)
  newTrainData <- copyTrainData(trainData)
  
  trainDataCovariateSize <- newTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  
  reducedTrainData <- univariateFeatureSelection(
    trainData = newTrainData, 
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
    )
  
  newDataCovariateSize <- reducedTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  expect_true(newDataCovariateSize <= trainDataCovariateSize)
  
  # expect k many covariates left
  expect_equal(k,reducedTrainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  
})


test_that("createRandomForestFeatureSelection correct class", {
  ntreesTest <- sample(1000,1)
  maxDepthTest <- sample(20,1)
  featureEngineeringSettings <- createRandomForestFeatureSelection(
    ntrees = ntreesTest, 
    maxDepth = maxDepthTest
    )
  
  expect_is(featureEngineeringSettings, 'featureEngineeringSettings')
  expect_equal(featureEngineeringSettings$ntrees, ntreesTest)
  expect_equal(featureEngineeringSettings$max_depth, maxDepthTest)
  expect_equal(attr(featureEngineeringSettings, "fun"), 'randomForestFeatureSelection')
  
  # error due to params
  expect_error(
    createRandomForestFeatureSelection(
      ntrees = -1, 
      maxDepth = maxDepthTest
    )
  )
  
  expect_error(
    createRandomForestFeatureSelection(
      ntrees = 'dfdfd', 
      maxDepth = maxDepthTest
    )
  )
  
  expect_error(
    createRandomForestFeatureSelection(
      ntrees = 50, 
      maxDepth = 'maxDepthTest'
    )
  )
  
  expect_error(
    createRandomForestFeatureSelection(
      ntrees = 50, 
      maxDepth = -1
    )
  )
  
})


test_that("randomForestFeatureSelection", {
  
  ntreesTest <- sample(1000,1)
  maxDepthTest <- sample(20,1)
  featureEngineeringSettings <- createRandomForestFeatureSelection(
    ntrees = ntreesTest, 
    maxDepth = maxDepthTest
  )
  
  newTrainData <- copyTrainData(trainData)
  trainDataCovariateSize <- newTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  
  reducedTrainData <- randomForestFeatureSelection(
    trainData = newTrainData, 
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
  )
  
  newDataCovariateSize <- reducedTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  expect_true(newDataCovariateSize < trainDataCovariateSize)

})

test_that("featureSelection is applied on test_data", {
  k <- 20
  featureEngineeringSettings <- testUniFun(k = k)
  newTrainData <- copyTrainData(trainData)
  newTrainData <- univariateFeatureSelection(
    trainData = newTrainData, 
    featureEngineeringSettings = featureEngineeringSettings,
    covariateIdsInclude = NULL
  )
  
  modelSettings <- setLassoLogisticRegression()
  
  # added try catch due to model sometimes not fitting
  plpModel <- tryCatch(
    {fitPlp(newTrainData, modelSettings, analysisId='FE')}, 
    error = function(e){return(NULL)}
  )
  
  if(!is.null(plpModel)){ # if the model fit then check this
    prediction <- predictPlp(plpModel, testData, population)
    expect_true(attr(prediction, 'metaData')$featureEngineering) 
  }
})