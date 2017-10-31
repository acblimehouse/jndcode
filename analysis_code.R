## ~~~ Pre-analysis Setup ~~~ ##
  ## Pulling in the cl() function from Kevin Goulding's blog:
      ## https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
      ## This is used to cluster the standard errors by Census Region
      cl   <- function(dat, fm, cluster){
                      require(sandwich, quietly = TRUE)
                      require(lmtest, quietly = TRUE)
                      M <- length(unique(cluster))
                      N <- length(cluster)
                      K <- fm$rank
                      dfc <- (M/(M-1))*((N-1)/(N-K))
                      uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
                      vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
                      coeftest(fm, vcovCL)
      }
      
  ## Prepping the Environment
      library(tidyverse)
      library(plm)
      library(knitr)
      library(ggplot2)
      library(stargazer) ## preferred style here is "jpam"
      ## Create WD Object, set as appopriate
      workdir <- "/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data"
      ## Set Working Directory & final filename
      setwd(workdir)
      filename <- "JobsAndDrugs.csv"
      fileURL <- "https://www.dropbox.com/s/hqg08gxch4906p9/JobsAndDrugs.csv?dl=0"
      objectname <- "jobsanddrugs"
      ## Download the dataset:
      if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
      }
      ## Load the data into R
      if (!exists(objectname)){
        jobsanddrugs <- as.data.frame(read.csv(file = filename, header = TRUE,colClasses = "character"))
      }
      
  ## Prepping the data.tables for analysis
      Numeric_Var <- c(7:8, 10:33)
      Factor_Var <- c(1:6, 9, 34:35)
      for (i in Numeric_Var){ jobsanddrugs[,i] <- as.numeric(jobsanddrugs[,i])}
      for (i in Factor_Var){  jobsanddrugs[,i] <- as.factor (jobsanddrugs[,i]) }
      jobsanddrugs <- jobsanddrugs %>% mutate(Unemployment_rate = Unemployment_rate/10) ## to match other rates
      jobsanddrugs <- jobsanddrugs %>% mutate(ln_Med_Inc_2015 = log10(Median_Household_Income_2015))
      jobsanddrugs <- jobsanddrugs %>% mutate(ln_drugdeaths = log10(drug.deaths))
      mainfileprep_drops <- c(7, 23:25, 32)
      jobsanddrugs_a <- jobsanddrugs
      jobsanddrugs_a[,mainfileprep_drops] <- NULL
      
  ## Cleanup
      rm(filename, fileURL, i, Factor_Var, Numeric_Var, objectname, jobsanddrugs, mainfileprep_drops)    
      
## ~~~ Section 3 - Descriptive Statistics ~~~ ##
  
  jobsanddrugs_a <- jobsanddrugs_a %>% group_by(FIPStxt, Year)
            
  ## Independent Variable Sets
      
      ## After Data Prep, how many complete cases of each independent variable are there
      count_indy <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(Unemployment_rate)) %>% 
                                  summarise(Unemp.Rate = n())
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(Estimate..Gini.Index)) %>% 
                                  summarise( GINI = n())
      count_indy <- left_join(count_indy, count_other, by = "Year")
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(ln_Median_Household_Income_2015)) %>% 
                                  summarise( Med.Income = n())
      count_indy <- left_join(count_indy, count_other, by = "Year")
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% distinct(FIPStxt) %>% 
                                  summarise(FIPS = n())
      count_indy <- left_join(count_indy, count_other, by = "Year")
      rm(count_other)
      
  ## Counts of Demographics n()
      count_demo <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(W_PER)) %>% 
                                  summarise(Perc.W = n())
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(H_PER)) %>% 
                                  summarise(Perc.H = n())
      count_demo <- left_join(count_demo, count_other, by = "Year")
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(S_PER)) %>% 
                                  summarise(Perc.M = n())
      count_demo <- left_join(count_demo, count_other, by = "Year")
  
  ## Counts of Dependent vars
      count_dep <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(casecount)) %>% 
                                  summarise(Opioid_Admits = n())
      count_other <- jobsanddrugs_a %>% ungroup() %>% group_by(Year) %>% 
                                  filter(complete.cases(casecount)) %>% 
                                  summarise(Drug.Deaths = n())
      count_dep <- left_join(count_dep, count_other, by = "Year")
      
  ## Bring the counts together
      var_count <- left_join(count_indy, count_demo, by = "Year")
      var_count <- left_join(var_count, count_dep, by = "Year")
      rm(count_demo, count_indy, count_dep, count_other, i)
  
  ## Show off the counts
    n_table <- kable(var_count)
    jobsanddrugs_g <- jobsanddrugs_a %>% ungroup()
    
  ## Show off the opiod.deaths and treatment.admissions by year
    complete.set <- c("Year","FIPStxt","Estimate..Gini.Index", "Unemployment_rate",
                       "W_PER", "H_PER","S_PER", "V_PER", "AvgAge")
    opioid.deaths <- jobsanddrugs_g %>% group_by(Year) %>%
                     filter(complete.cases(Unemployment_rate,W_PER)) %>% 
                     summarise(annual.total = sum(drug.deaths))
    treatment.admissions <- jobsanddrugs_a %>% filter(complete.cases(Estimate..Gini.Index, 
                            Unemployment_rate,W_PER)) %>% ungroup() %>% group_by(Year) %>% 
                            summarise(annual.total = sum(casecount))
      
## ~~~ Section 4 - Regression Analyses - Drug Treatment Commitments & Narcotics Poisonings ~~~ ##
  
  plm_index <- c("Year", "FIPStxt")
  jobsanddrugs_plm <- plm.data(x = jobsanddrugs_a, indexes = plm_index)
  ## Narcotics Poisonings
  opi.death_by_Unemployment_rate.plm <- plm(formula = ln_drugdeaths ~ Unemployment_rate + W_PER + H_PER + S_PER +
                                            V_PER + log(AvgAge), 
                                            data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                                            model = "within",index = plm_index)
  summary(opi.death_by_Unemployment_rate.plm)
  opi.death_by_Median_Income.plm <- plm(formula = ln_drugdeaths ~ ln_Med_Inc_2015 + W_PER + H_PER + 
                                        S_PER + V_PER + log(AvgAge), 
                                        data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                                        model = "within",index = plm_index)
  summary(opi.death_by_Median_Income.plm) ## Median_Income is ommitted
  opi.death_by_GINI <- plm(formula = ln_drugdeaths ~ Estimate..Gini.Index + W_PER + H_PER + 
                           S_PER + V_PER + log(AvgAge), 
                           data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                           model = "within",index = plm_index)
  summary(opi.death_by_GINI)
  opi.death_by_unemp_and_GINI <- plm(formula = ln_drugdeaths ~ Unemployment_rate + Estimate..Gini.Index + 
                                               W_PER + H_PER + S_PER + V_PER + log(AvgAge), data = jobsanddrugs_plm, 
                                               na.action = na.omit, effect = "time", model = "within",
                                               index = plm_index)
  summary(opi.death_by_unemp_and_GINI)
  ## Drug Treatment Admissions
  drugtreat_by_Unemployment_rate.plm <- plm(formula = log(casecount) ~ Unemployment_rate + W_PER + H_PER + S_PER +
                                              V_PER + log(AvgAge), 
                                            data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                                            model = "within",index = plm_index)
  summary(drugtreat_by_Unemployment_rate.plm)
  drugtreat_by_Median_Income.plm <- plm(formula = log(casecount) ~ ln_Med_Inc_2015 + W_PER + H_PER + S_PER +
                                              V_PER + log(AvgAge), 
                                            data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                                            model = "within",index = plm_index)
  summary(drugtreat_by_Median_Income.plm) ## log(median income) excluded again, not sure why
  drugtreat_by_GINI <- plm(formula = log(casecount) ~ Estimate..Gini.Index + W_PER + H_PER + 
                             S_PER + V_PER + log(AvgAge), 
                           data = jobsanddrugs_plm, na.action = na.omit, effect = "time",
                           model = "within",index = plm_index)
  summary(drugtreat_by_GINI)
  drugtreat_by_unemp_and_GINI <- plm(formula = log(casecount) ~ Unemployment_rate + Estimate..Gini.Index + 
                                       W_PER + H_PER + S_PER + V_PER + log(AvgAge), data = jobsanddrugs_plm, 
                                     na.action = na.omit, effect = "time", model = "within",
                                     index = plm_index)
  summary(drugtreat_by_unemp_and_GINI)
  
  
## Display the Regressions
  drug_dep_var <- ""
  drug_cov <- c("GINI Index, Est.", "Unemployment Rate", "Proportion White",
                "Proportion Hispanic", "Proportion Male", "Proportion Veteran",
                "Log of Average Age")
  drugtreat <- stargazer(drugtreat_by_GINI, 
                         drugtreat_by_Unemployment_rate.plm,
                         drugtreat_by_unemp_and_GINI, type = "html", style = "jpam",
                         title = "Log of TEDS - Admissions, Opioids Cases",
                         header = FALSE, dep.var.labels = drug_dep_var,
                         covariate.labels = drug_cov)
  opioiddeath <- stargazer(opi.death_by_GINI, opi.death_by_Unemployment_rate.plm,
                           opi.death_by_unemp_and_GINI, type = "html", style = "jpam",
                           title = "Log of Narcotics Poisonings",
                           header = FALSE, dep.var.labels = drug_dep_var,
                           covariate.labels = drug_cov)
  
  