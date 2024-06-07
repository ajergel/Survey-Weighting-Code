#loading Packages ----
pacman::p_load(tidyverse,dplyr,gtsummary,survey) #Required packages for the below code
#Weighted Survey Analysis With Replicate Weights ----
rep_weights <- colnames(select(FinalData,k5citywt_rep1:k5citywt_rep72)) #Collects all 72 replicate weight names in a list
formula <- as.formula(paste("~", paste(rep_weights, collapse = "+"))) #puts that list into a formula as: k5citywt_rep1 + k5citywt_rep2 ...+ k5citywt_rep72
SvyData <- svrepdesign(repweights = formula, #paste the formula here so its easier to handle the replicate weights
                       weights = ~k5citywt, #must still include the basic survey weights
                       data = FinalData,
                       type = "JK1", #this study uses jackknife. Change if your survey weights are calculated different - found in literature
                       combined.weights = TRUE, #default setting - most will be TRUE but check if you replicate weights already include sampleing weight
                       mse=getOption("survey.replicates.mse")) %>% #default setting
  subset(., cp6intyr != 2014) #Only remove observation in this step - this ensures weights are properly adjusted

##Categorical Variables ----
#svyby is for categorical variables
#use svymean for continuous
results <- svyby(~I(VarOfInterest=="0"), ~ComparionsVar, design = SvyData, svyciprop,
                 vartype = "ci", na.rm = TRUE) #THis has to be calculated level by level for the variable of interest
#vartype = "ci" provides confidence intervals
#Weighted Survey Analysis WITHOUT Replicate Weights ----
#Svy data without replicate weights follows a similar structure although simpler 
SvyData_NoRep <- svydesign(ids = ~idnum,
                           weights = ~k5citywt, # only need basic svy weight
                           data = FinalData) %>%
  subset(., cp6intyr != 2014) 

#when not using replicate weights you can use tbl_svysummary()
#currently tbl_svysummary does not support using replicate weights
#tbl_svysummary produces publication ready tables for svy data
Results <- SvyData_NoRep %>%
  subset(., StrataVar == "No") %>% #optional only need if you are doing a stratified analysis
  tbl_svysummary(
    include = c(VarOfInterest), #list variables of interest here
    statistic = list(all_continuous() ~ "{mean} ({sd})",#request continuous variables be summarized as mean (SD)
                     all_categorical() ~ "{n} ({p}%)"),
    by = ComparionsVar, # specify comparison variable
    missing_text = "(Missing)",
    percent = "row",#can also do column percentages
    digits = list(all_continuous() ~ c(2,2), #specifies decimal places for continuous and categorical variables separately
                  all_categorical() ~ c(0,0))) %>%
  add_ci() %>% #provides confidence intervals
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% # provides p-values at 3 significant figures
  add_overall() # provides overall column