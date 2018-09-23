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
  outcomeCohortNamesList <- paste(analysisList$outcomeNames, collapse = ', ')
  
  #time at risk
  tar <- unique(
    lapply(json$populationSettings, function(x) 
      paste0("Risk Window Start:  ",x$riskWindowStart,
             ', Add Exposure Days to Start:  ',x$addExposureDaysToStart,
             ', Risk Window End:  ', x$riskWindowEnd,
             ', Add Exposure Days to End:  ', x$addExposureDaysToEnd)))
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
    officer::ftext("Patient Level Prediction:  ", prop = style_title), 
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
    officer::body_add_par(paste0("The objective of this study iS develop and validate patient-level prediction models for patients in ",
                                 length(json$targetIds)," target cohort(s) (",
                                 targetCohortNamesList,") to predict ",
                                 length(json$outcomeIds)," outcome(s) (",
                                 outcomeCohortNamesList,") for ",
                                 length(tar)," time at risk(s) (",
                                 tar,")."), 
                          style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The prediction will be implemented",
                                 length(json$modelSettings)," methods (",
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
    officer::body_add_par(paste0("The objective is to develop and validate the following prediction models:"),style = "Normal") %>%
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
    officer::body_add_par("This study will follow a retrospective, observational, patient level prediction design. We define 'retrospective' to mean the study will be conducted using data already collected prior to the start of the study. We define 'observational' to mean there is no intervention or treatment assignment imposed by the study. We define 'patient level prediction' as a modeling process wherein an outcome is predicted within a time at risk relative to the target cohort start and end date.  Prediction is preformed using a set of covariates derived using data prior to the start of the target cohort.",style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Figure 1 illustrates the prediction problem we will address. Among a population at risk, we aim to predict which patients at a defined moment in time (t = 0) will experience some outcome during a time-at-risk. Prediction is done using only information about the patients in an observation window prior to that moment in time.", style="Normal") %>%
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
    officer::body_add_par("Outcome Cohorts(s) [O]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Time at Risk", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(dfTar, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("Additional Population Settings", style = "heading 3") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Statistical Analysis Method(s)", style = "heading 2") %>%
    officer::body_add_par("Classifiers", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Model Evaluation", style = "heading 3") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Quality Control", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The model will be investigated by:",style="Normal") %>%
    officer::body_add_par("--Calculating the calibration and discrimination measures and comparing against existing model benchmarks (identified using a literature search)",style="Normal") %>%
    officer::body_add_par("--Inspection of the fitted outcome model for large coefficients and predictors that we cannot explain (post-hoc).  The goal is to not “explain” the predictors instead find potential issues with cohorts.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The PatientLevelPrediction package itself, as well as other OHDSI packages on which PatientLevelPrediction depends, use unit tests for validation.",style="Normal") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(plpCitation, prop = style_citation)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Tools", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("This study will be designed using OHDSI tools and run with R Q.  Important versioning information about the tools can be found in the Appendix “Version Information”.",style="Normal") %>%
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
    officer::body_add_par("Reviewing the incidence rates of the outcomes for the target population prior to performing the analysis will allow us to understand if we have cohorts that are appropriate to perform prediction on (i.e. if you have a cohort without an outcome a prediction model cannot be built).  The full table can be found below in the “Table and Figures” section under “Incidence Rate of Target & Outcome”.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Additionally, reviewing the characteristics of the cohorts provides insight into the cohorts being reviewed.  The full table can be found below in the “Table and Figures” section under “Characterization”.",style="Normal")
  #----------------------------------------------------------------------------- 
  
  #============ DATA ANALYSIS PLAN =============================================
  doc <- doc %>%
    officer::body_add_par("Data Analysis Plan", style = "heading 1")
  #----------------------------------------------------------------------------- 
  
  #============ STRENGTHS & LIMITATIONS ========================================
  doc <- doc %>%
    officer::body_add_par("Strengths & Limitations", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Model Settings", style = "heading 2") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Covariate Srttings", style = "heading 2") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Model Development & Evaluation", style = "heading 2") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Analysis Execution Settings", style = "heading 2") %>%
    officer::body_add_par("") 
  #----------------------------------------------------------------------------- 
  
  #============ PROTECTION OF HUMAN SUBJECTS ===================================
  doc <- doc %>%
    officer::body_add_par("Protection of Human Subjects", style = "heading 1")
  #----------------------------------------------------------------------------- 
  
  #============ DISSEMINATING & COMMUNICATING ==================================
  doc <- doc %>%
    officer::body_add_par("Plans for Disseminating & Communicating Study Results", style = "heading 1") %>%
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
    officer::body_add_par("Hydra Version:  ",style="Normal") %>%
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
    officer::body_add_par("References", style = "heading 1")
  #----------------------------------------------------------------------------- 
  
  #   officer::body_add_par(paste0("Num of Target Cohorts:  ",length(json$targetIds)), style = "Normal")  %>%
  #   officer::body_add_par(paste0("Target Cohorts: ",targetCohortNamesList), style = "Normal")   %>%
  #   officer::body_add_par(paste0("Num of Outcome Cohorts:  ",length(json$outcomeIds)), style = "Normal")   %>%
  #   officer::body_add_par(paste0("Outcome Cohorts: ",outcomeCohortNamesList), style = "Normal")   %>%
  #   officer::body_add_par(paste0("Number of TAR: ", length(tar)), style = "Normal") %>%
  #   officer::body_add_par(paste0("TAR:  ", tar), style = "Normal") %>%
  #   officer::body_add_par(paste0("Num of Models:  ",length(json$modelSettings)), style = "Normal") %>%
  #   officer::body_add_par(paste0("Models:  ", paste(lapply(analysisList$modelAnalysisList$models, function(x) x$name), collapse = ', ')), style = "Normal") %>%
  #   officer::body_add_par(paste0("Citation:  ", citation("PatientLevelPrediction")$textVersion), style = "Normal") %>%
  #   officer::body_add_par(paste0("Min Covariate Fraction:  ", analysisList$minCovariateFraction), style = "Normal") %>%
  #   officer::body_add_par(paste0("Train Set:  ", 1-analysisList$testFraction), style = "Normal") %>%
  #   officer::body_add_par(paste0("Test Set:  ", analysisList$testFraction), style = "Normal") %>%
  #   officer::body_add_par(paste0("N Folds:  ", analysisList$nfold), style = "Normal") %>%
  #   officer::body_add_par(paste0("Skeleton Version:  ",json$skeletonType," - ", json$skeletonVersion), style = "Normal") %>%
  #   officer::body_add_table(objective, header = TRUE) %>%
  #   officer::body_add_img(src = 'vignettes/Figure1.png', width = 6.5, height = 2.01, style = "centered")
    

  
  #-----------------------------------------------------------------------------
  
  print(doc, target = file.path(outputLocation))
}
