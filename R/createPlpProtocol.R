#' @export
createPlpProtocol <- function(json,
                              outputLocation = file.path(getwd(),'plp_protocol.docx')){

  #============== STYLES =======================================================
  style_title <- officer::shortcuts$fp_bold(font.size = 28)
  style_title_italic <- officer::shortcuts$fp_bold(font.size = 30, italic = TRUE)
  style_toc <- officer::shortcuts$fp_bold(font.size = 16)
  style_helper_text <- officer::shortcuts$fp_italic(color = "#FF8C00")
  style_citation <- officer::shortcuts$fp_italic(shading.color = "grey")

  #============== VARIABLES ====================================================
  #analysis information
  analysisList <- PatientLevelPrediction::loadPredictionAnalysisList(predictionAnalysisListFile)
  
  targetCohortNamesList <- paste(analysisList$cohortNames, collapse = ', ')
  targetCohorts <- as.data.frame(cbind(analysisList$cohortNames,rep("TBD",length(analysisList$cohortNames))))
  names(targetCohorts) <- c("Cohort Name","Description")
  
  outcomeCohortNamesList <- paste(analysisList$outcomeNames, collapse = ', ')
  outcomeCohorts <- as.data.frame(cbind(analysisList$outcomeNames,rep("TBD",length(analysisList$outcomeNames))))
  names(outcomeCohorts) <- c("Cohort Name","Description")
  
  #time at risk
  tar <- unique(
    lapply(json$populationSettings, function(x) 
      paste0("Risk Window Start:  ",x$riskWindowStart,
             ', Add Exposure Days to Start:  ',x$addExposureDaysToStart,
             ', Risk Window End:  ', x$riskWindowEnd,
             ', Add Exposure Days to End:  ', x$addExposureDaysToEnd)))
  
  covSettings <- lapply(json$covariateSettings, function(x) cbind(names(x), unlist(lapply(x, function(x2) paste(x2, collapse=', '))))) 
  
  popSettings <- lapply(json$populationSettings, function(x) cbind(names(x), unlist(lapply(x, function(x2) paste(x2, collapse=', '))))) 
  
  #-----------------------------------------------------------------------------
  
  #============== CITATIONS =====================================================
  plpCitation <- paste0("Citation:  ", citation("PatientLevelPrediction")$textVersion)
  tripodCitation <- paste0("Citation:  Collins, G., et al. (2017.02.01). 'Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): The TRIPOD statement.' from https://www.equator-network.org/reporting-guidelines/tripod-statement/ ")
  progressCitation <- paste0("Citation:  Steyerberg EW, Moons KG, van der Windt DA, Hayden JA, Perel P, Schroter S, Riley RD, Hemingway H, Altman DG; PROGRESS Group. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS Med. 2013;10(2):e1001381. doi: 10.1371/journal.pmed.1001381. Epub 2013 Feb 5. Review. PubMed PMID: 23393430; PubMed Central PMCID: PMC3564751.")
  rCitation <- paste0("Citation:  R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.")
  #-----------------------------------------------------------------------------
  
  #============== CREATE DOCUMENT ==============================================
  # create new word document
  doc = officer::read_docx()
  #-----------------------------------------------------------------------------
  
  #============ TITLE PAGE =====================================================
  title <- officer::fpar(
    officer::ftext("Patient-Level Prediction:  ", prop = style_title), 
    officer::ftext(json$packageName, prop = style_title_italic)
    )

  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(title) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Prepared on:  ", Sys.Date()), style = "Normal") %>%
    officer::body_add_par(paste0("Created by:  ", json$createdBy$name, " (", json$createdBy$email,")"), style = "Normal") %>%
    officer::body_add_break() 
  #-----------------------------------------------------------------------------  
  
  #============ TOC ============================================================
  toc <- officer::fpar(
    officer::ftext("Table of Contents", prop = style_toc)
  )
  
  doc <- doc %>%
    officer::body_add_fpar(toc) %>%
    officer::body_add_toc(level = 2) %>%
    officer::body_add_break() 
  #----------------------------------------------------------------------------- 
  
  #============ LIST OF ABBREVIATIONS ==========================================
  abb <- data.frame(rbind(
    c("AUC", "Area Under the Receiver Operating Characteristic Curve"),
    c("CDM","Common Data Model"),
    c("O","Outcome Cohort"),
    c("OHDSI","Observational Health Data Sciences & Informatics"),
    c("OMOP","Observational Medical Outcomes Partnership"),
    c("T", "Target Cohort"),
    c("TAR", "Time at Risk")
  ))
  names(abb) <- c("Abbreviation","Phrase")
  abb <- abb[order(abb$Abbreviation),]

  doc <- doc %>%
    officer::body_add_par("List of Abbreviations", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(abb, header = TRUE) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Rest to be completed outside of ATLAS >>", prop = style_helper_text)
        ))
  #----------------------------------------------------------------------------- 
  
  
  #============ RESPONSIBLE PARTIES ============================================
  doc <- doc %>%
    officer::body_add_par("Responsible Parties", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS ", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Includes author, investigator, and reviewer names and sponsor information. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ Executive Summary ==============================================
  doc <- doc %>%
    officer::body_add_par("Executive Summary", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< A few statements about the rational and background for this study. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The objective of this study is to develop and validate patient-level prediction models for patients in ",
                                 length(json$targetIds)," target cohort(s) (",
                                 targetCohortNamesList,") to predict ",
                                 length(json$outcomeIds)," outcome(s) (",
                                 outcomeCohortNamesList,") for ",
                                 length(tar)," time at risk(s) (",
                                 tar,")."), 
                          style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The prediction will be implemented using ",
                                 length(json$modelSettings)," algorithms (",
                                 paste(lapply(analysisList$modelAnalysisList$models, function(x) x$name), collapse = ', '),")."),
                          style = "Normal")
  #----------------------------------------------------------------------------- 

  #============ RATIONAL & BACKGROUND ==========================================
  doc <- doc %>%
    officer::body_add_par("Rational & Background", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Provide a short description of the reason that led to the initiation of or need for the study and add a short critical review of available published and unpublished data to explain gaps in knowledge that the study is intended to fill. >>", prop = style_helper_text)
      )) 
  #-----------------------------------------------------------------------------
  
  #============ OBJECTIVE ======================================================
  prep_objective <- merge(analysisList$cohortNames, analysisList$outcomeNames)
  objective <- merge(prep_objective, tar)
  names(objective) <-c("Target Cohorts","Outcome Cohorts","Time at Risk") 
  
  doc <- doc %>%
    officer::body_add_par("Objective", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The objective is to develop and validate patient-level prediction models for the following prediction problems:"),style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(objective, header = TRUE, style = "Table Professional")
    
  #----------------------------------------------------------------------------- 
  
  #============ METHODS ======================================================
  dfTar <- data.frame(tar)
  names(dfTar) <- c("Time at Risk")
  
  doc <- doc %>%
    officer::body_add_par("Methods", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Design", style = "heading 2") %>%
    officer::body_add_par("This study will follow a retrospective, observational, patient-level prediction design. We define 'retrospective' to mean the study will be conducted using data already collected prior to the start of the study. We define 'observational' to mean there is no intervention or treatment assignment imposed by the study. We define 'patient-level prediction' as a modeling process wherein an outcome is predicted within a time at risk relative to the target cohort start and/or end date.  Prediction is performed using a set of covariates derived using data prior to the start of the target cohort.",style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Figure 1, illustrates the prediction problem we will address. Among a population at risk, we aim to predict which patients at a defined moment in time (t = 0) will experience some outcome during a time-at-risk. Prediction is done using only information about the patients in an observation window prior to that moment in time.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_img(src = 'vignettes/Figure1.png', width = 6.5, height = 2.01, style = "centered") %>%
    officer::body_add_par("Figure 1: The prediction problem", style="graphic title") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(plpCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("We follow the TRIPOD guidance for presenting the prediction model results for ensuring model transparency and we follow the PROGRESS best practice recommendations for prediction model development.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(tripodCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(progressCitation, prop = style_citation)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Data Source(s)", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("For each database, provide database full name, version information (if applicable), the start and end dates of data capture, and a brief description of the data source.  Also include information on data storage (e.g. software and IT environment, database maintenance and anti-fraud protection, archiving) and data protection.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Important Citations: OMOP Common Data Model:  'OMOP Common Data Model (CDM).' from https://github.com/OHDSI/CommonDataModel.", prop = style_helper_text)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Populations", style = "heading 2") %>%
    officer::body_add_par("Target Cohort(s) [T]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_table(targetCohorts, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Outcome Cohorts(s) [O]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_table(outcomeCohorts, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Time at Risk", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(dfTar, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("Additional Population Settings", style = "heading 3") %>%
    officer::body_add_par("")
  
    for(i in 1:length(popSettings)){
      onePopSettings <- as.data.frame(popSettings[i])
      names(onePopSettings) <- c("Item","Settings")
      
      doc <- doc %>% 
        officer::body_add_par(paste0("Population Settings #",i), style = "table title") %>%
        officer::body_add_table(onePopSettings, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
    }
  
    #```````````````````````````````````````````````````````````````````````````
  
  modelEvaluation <- data.frame(rbind(
    c("ROC Plot", "The ROC plot plots the sensitivity against 1-specificity on the test set. The plot shows how well the model is able to discriminate between the people with the outcome and those without. The dashed diagonal line is the performance of a model that randomly assigns predictions. The higher the area under the ROC plot the better the discrimination of the model."),
    c("Calibration Plot", "The calibration plot shows how close the predicted risk is to the observed risk. The diagonal dashed line thus indicates a perfectly calibrated model. The ten (or fewer) dots represent the mean predicted values for each quantile plotted against the observed fraction of people in that quantile who had the outcome (observed fraction). The straight black line is the linear regression using these 10 plotted quantile mean predicted vs observed fraction points. The two blue straight lines represented the 95% lower and upper confidence intervals of the slope of the fitted line."),
    c("Smooth Calibration Plot", "Similar to the traditional calibration shown above the Smooth Calibration plot shows the relationship between predicted and observed risk. the major difference is that the smooth fit allows for a more fine grained examination of this. Whereas the traditional plot will be heavily influenced by the areas with the highest density of data the smooth plot will provide the same information for this region as well as a more accurate interpretation of areas with lower density. the plot also contains information on the distribution of the outcomes relative to predicted risk.  However the increased information game comes at a computational cost. It is recommended to use the traditional plot for examination and then to produce the smooth plot for final versions."),
    c("Prediction Distribution Plots", "The preference distribution plots are the preference score distributions corresponding to i) people in the test set with the outcome (red) and ii) people in the test set without the outcome (blue)."),
    c("Box Plots", "The prediction distribution boxplots are box plots for the predicted risks of the people in the test set with the outcome (class 1: blue) and without the outcome (class 0: red)."),
    c("Test-Train Similarity Plot", "The test-train similarity is presented by plotting the mean covariate values in the train set against those in the test set for people with and without the outcome."),
    c("Variable Scatter Plot", "The variable scatter plot shows the mean covariate value for the people with the outcome against the mean covariate value for the people without the outcome. The size and color of the dots correspond to the importance of the covariates in the trained model (size of beta) and its direction (sign of beta with green meaning positive and red meaning negative), respectively."),
    c("Precision Recall Plot", ""),
    c("Demographic Summary Plot", "This plot shows for females and males the expected and observed risk in different age groups together with a confidence area.")
  ))
  names(modelEvaluation) <- c("Evaluation","Description")
  modelEvaluation <- modelEvaluation[order(modelEvaluation$Evaluation),]

  doc <- doc %>%
    officer::body_add_par("Statistical Analysis Method(s)", style = "heading 2") %>%
    officer::body_add_par("Classifiers", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Model Evaluation", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The following evaluations will be performed on the model:", style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(modelEvaluation, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Quality Control", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The PatientLevelPrediction package itself, as well as other OHDSI packages on which PatientLevelPrediction depends, use unit tests for validation.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(plpCitation, prop = style_citation)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Tools", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("This study will be designed using OHDSI tools and run with R.  More information about the tools can be found in the Appendix 'Version Information'.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(rCitation, prop = style_citation)
      )) 
    #----------------------------------------------------------------------------- 
  
  #============ DIAGNOSTICS ====================================================
  doc <- doc %>%
    officer::body_add_par("Diagnostics", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Reviewing the incidence rates of the outcomes in the target population prior to performing the analysis will allow us to assess its feasibility.  The full table can be found in the 'Table and Figures' section under 'Incidence Rate of Target & Outcome'.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Additionally, reviewing the characteristics of the cohorts provides insight into the cohorts being reviewed.  The full table can be found below in the 'Table and Figures' section under 'Characterization'.",style="Normal")
  #----------------------------------------------------------------------------- 
  
  #============ DATA ANALYSIS PLAN =============================================
  
  doc <- doc %>%
    officer::body_add_par("Data Analysis Plan", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Algorithm Settings", style = "heading 2") %>%
    officer::body_add_par("") 
  
    for(i in 1:length(covSettings)){
      
      modelSettingsTitle <- names(json$modelSettings[[i]])
      modelSettings <- lapply(json$modelSettings[[i]], function(x) cbind(names(x), unlist(lapply(x, function(x2) paste(x2, collapse=', '))))) 
      
      oneModelSettings <- as.data.frame(modelSettings)
      names(oneModelSettings) <- c("Covariates","Settings")
      
      doc <- doc %>% 
        officer::body_add_par(paste0("Model Settings Settings #",i, " - ",modelSettingsTitle), style = "table title") %>%
        officer::body_add_table(oneModelSettings, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
    }
  
    #```````````````````````````````````````````````````````````````````````````
  doc <- doc %>%
    officer::body_add_par("Covariate Settings", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The baseline covariates (covariates constructed using records on or prior to the target cohort start date) are used within this prediction mode include the following.  Each covariate needs to contain at least ",
                          json$runPlpArgs$minCovariateFraction, 
                          " subjects to be considered for the model."),
                          style="Normal") %>%
    officer::body_add_par("") 
    
    for(i in 1:length(covSettings)){
      oneCovSettings <- as.data.frame(covSettings[i])
      names(oneCovSettings) <- c("Covariates","Settings")
      
      doc <- doc %>% 
        officer::body_add_par(paste0("Covariate Settings #",i), style = "table title") %>%
        officer::body_add_table(oneCovSettings, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
    }
    #```````````````````````````````````````````````````````````````````````````
  doc <- doc %>%
    officer::body_add_par("Model Development & Evaluation", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("To build and internally validate the models, we will partition the labelled data into a train set (",
                                 (1-analysisList$testFraction)*100,
                                 "%) and a test set (",
                                 analysisList$testFraction*100,
                                 "%)."), 
                          style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The hyper-parameters for the models will be assessed using ",
                                 analysisList$nfold,
                                 "-fold cross validation on the train set and a final model will be trained using the full train set and optimal hyper-parameters."),
                          style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The internal validity of the models will be assessed on the test set.  We will use the area under the receiver operating characteristic curve (AUC) to evaluate the discriminative performance of the models and plot the predicted risk against the observed fraction to visualize the calibration.  See 'Model Evaluation' section for more detailed information about additional model evaluation metrics.") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Analysis Execution Settings", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("There are ",
                                 length(json$targetIds),
                                 " target cohorts evaluated for ",
                                 length(json$outcomeIds),
                                 " outcomes over ",
                                 length(json$modelSettings),
                                 " models over ",
                                 length(covSettings),
                                 " covariates settings and over ",
                                 length(popSettings),
                                 " population settings.  In total there are ",
                                 length(json$targetIds) * length(json$outcomeIds) * length(json$modelSettings) * length(covSettings) * length(popSettings),
                                 " analysis performed.  For a full list refer to appendix 'Complete Analysis List'."),
                          style = "Normal") %>%
    officer::body_add_par("")
  #----------------------------------------------------------------------------- 
  
  #============ STRENGTHS & LIMITATIONS ========================================
  doc <- doc %>%
    officer::body_add_par("Strengths & Limitations", style = "heading 1") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Some limitations to consider:", 
                       prop = style_helper_text), 
        officer::ftext("--It may not be possible to develop prediction models for rare outcomes. ", 
                       prop = style_helper_text),
        officer::ftext("--Not all medical events are recorded into the observational datasets and some recordings can be incorrect.  This could potentially lead to outcome misclassification.", 
                       prop = style_helper_text),
        officer::ftext("--The prediction models are only applicable to the population of patients represented by the data used to train the model and may not be generalizable to the wider population.", 
                       prop = style_helper_text)
      ))
  
  #----------------------------------------------------------------------------- 
  
  #============ PROTECTION OF HUMAN SUBJECTS ===================================
  doc <- doc %>%
    officer::body_add_par("Protection of Human Subjects", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Describe any additional safeguards that are appropriate for the data being used.", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Here is an example statement:", prop = style_helper_text),
        officer::ftext("Confidentiality of patient records will be maintained always. All study reports will contain aggregate data only and will not identify individual patients or physicians. At no time during the study will the sponsor receive patient identifying information except when it is required by regulations in case of reporting adverse events.", prop = style_helper_text),
        officer::ftext(">>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ DISSEMINATING & COMMUNICATING ==================================
  doc <- doc %>%
    officer::body_add_par("Plans for Disseminating & Communicating Study Results", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("List any plans for submission of progress reports, final reports, and publications.", 
                       prop = style_helper_text),
        officer::ftext(">>", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_break()

  #----------------------------------------------------------------------------- 
  
  #============ TABLES & FIGURES ===============================================
  doc <- doc %>%
    officer::body_add_par("Tables & Figures", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Incidence Rate of Target & Outcome", style = "heading 2") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Characterization", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_break() 
  #----------------------------------------------------------------------------- 
  
  #============ APPENDICES =====================================================
  doc <- doc %>%
    officer::body_add_par("Appendices", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Version Information", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Skeleton Version:  ",json$skeletonType," - ", json$skeletonVersion),style="Normal") %>%
    officer::body_add_par("Identifier / Organization: ",style="Normal") %>%
    officer::body_add_break() %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Code List", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_break() %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Complete Analysis List", style = "heading 2") %>%
    officer::body_add_par("") %>%
    
    officer::body_add_break() 
  #-----------------------------------------------------------------------------
  
  #============ REFERNCES ======================================================
  doc <- doc %>%
    officer::body_add_par("References", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 

  print(doc, target = file.path(outputLocation))
}
