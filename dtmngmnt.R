## ~~~ Section 1 - Getting ready to work ~~~ ##
## Home git == https://github.com/acblimehouse/jndcode ##
  ## ID & Set Working Directory
    ## Create WD Object, set as appopriate
    workdir <- "/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data"
    ## Set Working Directory & final filename
    setwd(workdir)
    jndflnm <- "JobsAndDrugs.csv"
  ## Setup Packages
    library(xlsx) ## used later for xlsx data sets
    library(tidyverse) ## used later for csv data sets and for data cleaning and joining
    library(R.utils)
  ## Add useful functions
    factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)] 
    
## ~~~ Section 2 - Pulling Data into R ~~~ ##
  ## Pulling Data into R - ~~~ NOTE: URLs not guaranteed current after Nov 2017 ~~~

    ## 2016 FIPS Codes for future data managment
      {filename <- "all-geocodes-v2016.xlsx"
      fileURL <- "https://www2.census.gov/programs-surveys/popest/geographies/2016/all-geocodes-v2016.xlsx"
      objectname <- "FIPScodes"
      ## Download the dataset:
      if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
      }
      ## Load the data into R
      if (!exists(objectname)){
        FIPScodes <- read.xlsx2(file = filename,
                                    sheetIndex = 1, 
                                    startRow = 5,
                                    as.data.frame = TRUE,
                                    stringsasFactors = TRUE,
                                    header = TRUE)
      }
      ## Cleanup
      gc() ## to help improve performance of the code and decrease the chance of rJava erroring out.
      }
    
    ## NBER CBSA to FIPS County Crosswalk
    {## Used in working with the SAMHDA data later on
      filename <- "cbsa2fipsxw.csv"
      fileURL <- "https://www.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv"
      objectname <- "CBSAtoFIPS"
      ## Download the dataset:
      if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
      }
      ## Load the data into R
      if (!exists(objectname)){
        CBSAtoFIPS <- as.data.frame(read.csv2(file = filename, sep = ",", 
                                               header = TRUE,colClasses = "factor"))
      }
      ## Creating County Level FIPS codes
      CBSAtoFIPS$fipsstatecode <- as.character(CBSAtoFIPS$fipsstatecode)
      CBSAtoFIPS$fipscountycode <- as.character(CBSAtoFIPS$fipscountycode)
      CBSAtoFIPS$FIPStxt <- as.factor(with(CBSAtoFIPS, paste0(fipsstatecode, fipscountycode)))
      CBSAtoFIPS <- CBSAtoFIPS %>% filter(cbsacode!="")
      ## Cleanup
      gc() ## to help improve performance of the code and decrease the chance of rJava erroring out.
    }
    
    ## USDA Economic Research Service - Unemployment and median household income for the U.S., States, and counties, 2007-16
      {filename <- "USDA ERS Unemployment.xls"
      fileURL <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.xls?v=42894"
      objectname <- "usda0716unemp"
      ## Download the dataset:
      if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
        }
      ## Load the data into R
      if (!exists(objectname)){
        usda0716unemp <- read.xlsx2(file = filename,
                                    sheetIndex = 1, 
                                    startRow = 10,
                                    as.data.frame = TRUE,
                                    stringsasFactors = TRUE,
                                    header = TRUE)
        }
      ## Defactor the numeric columns
      cols <- c(7:ncol(usda0716unemp))
      usda0716unemp[cols] <- lapply(usda0716unemp[cols], factorToNumeric)
      ## Remove the aggregate state level data
      usda0716unemp <- filter(usda0716unemp,!usda0716unemp$Metro_2013=="")}
      
    ## Create Primary Data Set Object & Clean Up
      {
      jobsanddrugs <- usda0716unemp
      LongVar <- c("Civilian_labor_force", 
                   "Employed", 
                   "Unemployed", 
                   "Unemployment_rate")
      WideVar <- c("Civilian_labor_force_2007","Employed_2007","Unemployed_2007","Unemployment_rate_2007",
                   "Civilian_labor_force_2008","Employed_2008","Unemployed_2008","Unemployment_rate_2008",
                   "Civilian_labor_force_2009","Employed_2009","Unemployed_2009","Unemployment_rate_2009",
                   "Civilian_labor_force_2010","Employed_2010","Unemployed_2010","Unemployment_rate_2010",
                   "Civilian_labor_force_2011","Employed_2011","Unemployed_2011","Unemployment_rate_2011",
                   "Civilian_labor_force_2012","Employed_2012","Unemployed_2012","Unemployment_rate_2012",
                   "Civilian_labor_force_2013","Employed_2013","Unemployed_2013","Unemployment_rate_2013",
                   "Civilian_labor_force_2014","Employed_2014","Unemployed_2014","Unemployment_rate_2014",
                   "Civilian_labor_force_2015","Employed_2015","Unemployed_2015","Unemployment_rate_2015",
                   "Civilian_labor_force_2016","Employed_2016","Unemployed_2016","Unemployment_rate_2016")
      timesvar <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
      jobsanddrugs <- reshape(data = jobsanddrugs,direction = "long", varying = WideVar, v.names = LongVar,
              idvar = "FIPStxt", times = timesvar)
      ## Renaming for later analysis and cleaning up
      colnames(jobsanddrugs)[colnames(jobsanddrugs) == 'time'] <- 'Year'
      jobsanddrugs$Year <- as.factor(jobsanddrugs$Year) 
      rm(usda0716unemp,LongVar, WideVar)
      jobsanddrugs$stringsasFactors <- NULL
      }
      
    ## CDC Multiple Cause of Death Data - focused on Alcolhol and Opiods
        ## Requested and downloaded from https://wonder.cdc.gov/mcd-icd10.html
        ## Not available as a direct download due to government data restrictions
      objectname <- "cdcmcd9915"
      if (!exists(objectname)){
          filename.1 <- "Multiple Cause of Death, 1999-2004.txt"
          filename.2 <- "Multiple Cause of Death, 2005-2010.txt"
          filename.3 <- "Multiple Cause of Death, 2011-2015.txt"
          drops <- c(1,10:11) # to remove Notes, County, crude rate, and crude std. err.
          
          ## CDC Multiple Causes of Death, 1999-2004 / five years
          cdcmcd9904 <- as.data.frame(read.csv2(file = filename.1, sep = "\t", 
                                                header = TRUE, stringsAsFactors = TRUE))
          cdcmcd9904[,drops] <- NULL ## Drops unnecessary columns
          cdcmcd9904$Deaths <- as.numeric(cdcmcd9904$Deaths) ## Compels numeric value
          cdcmcd9904$Population <- as.numeric(cdcmcd9904$Population) ## Compels numeric value
          
          ## CDC Multiple Causes of Death, 2005-2010 / six years
          cdcmcd0510 <- as.data.frame(read.csv2(file = filename.2, sep = "\t", 
                                                header = TRUE, stringsAsFactors = TRUE))
          cdcmcd0510[,drops] <- NULL #Drops unnecessary columns
          cdcmcd0510$Deaths <- as.numeric(cdcmcd0510$Deaths) ## Compels numeric value
          cdcmcd0510$Population <- as.numeric(cdcmcd0510$Population) ## Compels numeric value
          
          ## CDC Multiple Causes of Death, 2011-2015 / five years
          cdcmcd1115 <- as.data.frame(read.csv2(file = filename.3, sep = "\t", 
                                                header = TRUE, stringsAsFactors = TRUE))
          cdcmcd1115[, drops] <- NULL #Drops unnecessary columns
          cdcmcd1115$Deaths <- as.numeric(cdcmcd1115$Deaths) ## Compels numeric value
          cdcmcd1115$Population <- as.numeric(cdcmcd1115$Population) ## Compels numeric value
          
          ## Creating a unified file, CDC Multiple Causes of Death, 1999-2015
          cdcmcd9915 <- union(cdcmcd9904, cdcmcd0510) ## instead of bind.rows
          cdcmcd9915 <- union(cdcmcd9915, cdcmcd1115) ## instead of bind.rows
          cdcmcd9915$Year <- as.factor(cdcmcd9915$Year)
          ## Clean up
          rm(cdcmcd9904,cdcmcd0510, cdcmcd1115, filename.1, filename.2, filename.3, drops)
          gc()

          columnremoves <- c(2:3 , 5, 9:10)
          cdcmcd9915[,columnremoves] <- NULL ## Remove unnecessary column

          ## Widening the CDC Data on Drug, Alcohol, Other-cause Deaths by year
          cdcmcd9915$UCD...Drug.Alcohol.Induced.Causes <- NULL ## Remove unnecessary column
          cdcmcd9915$Year.Code <- NULL ## Remove unnecessary column
            ## Create "Other Deaths" filtered data set
          {
          cdcmcd9915.O <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="O")
          cdcmcd9915.O$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL
          colnames(cdcmcd9915.O)[colnames(cdcmcd9915.O)== 'Deaths'] <- 'other.deaths'
          colnames(cdcmcd9915.O)[colnames(cdcmcd9915.O)== 'Population'] <- 'other.population'
          }
            ## Create "Alcohol Deaths" filtered data set
          {
          cdcmcd9915.A <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="A")
          cdcmcd9915.A$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.A)== 'Deaths'] <- 'alcohol.deaths'
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.A)== 'Population'] <- 'alcohol.population'
          }
            ## Create "Drug Deaths" filtered data set
          {
          cdcmcd9915.D <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="D")
          cdcmcd9915.D$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL

          colnames(cdcmcd9915.D)[colnames(cdcmcd9915.D)== 'Deaths'] <- 'drug.deaths'
          colnames(cdcmcd9915.D)[colnames(cdcmcd9915.D)== 'Population'] <- 'drug.population'
          }
            ## Rejoin the data sets together using Year and County.Code
          {
          joinvar <- c("Year","County.Code")
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.D)== 'Deaths'] <- 'drug.deaths'
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.D)== 'Population'] <- 'drug.population'}
            ## Drop columns that are no longer necessary
          cdcmcd9915[,c(3:5)] <- NULL
            ## Rejoin the data sets together using Year and County.Code
          {joinvar <- c("Year","County")
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.O, by = joinvar)
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.A, by = joinvar)
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.D, by = joinvar)
          rm(cdcmcd9915.O,cdcmcd9915.D,cdcmcd9915.A, joinvar)
          cdcmcd9915$Year.Code <- NULL ##unnecessary duplication
          }
      }
      
    ## Bring jobsanddrugs and the CDC data set together
        ## left_join on Year and FIPStxt
      {
        names(cdcmcd9915)[names(cdcmcd9915) == 'County.Code'] <- 'FIPStxt'
        jobsanddrugs <- left_join(jobsanddrugs,cdcmcd9915,c("FIPStxt","Year")) %>% group_by(FIPStxt)
        rm(cdcmcd9915)
      }
      
    ## SAMHDA (TEDS-A-1992-2012-DS000X) - Treatment Episode Data Set 1992 to 2012 (only downloads 1995 - 2012)
      ## Link to parent series: https://datafiles.samhsa.gov/study/treatment-episode-data-set-admissions-teds-1992-2012-nid13582
      ## Note:  These are very large files, as of 201710 between 500k and 990k records. They take a while to download
      ##        and they take longer to unzip and then to load into R. "GC()" is included after each step to ensure that
      ##     
        ## SAMHDA (TEDS-A-1992-2012-DS0002) - 1995-1999
        {zipname <- "TEDS-A-1992-2012-DS0002-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0002-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0002/TEDS-A-1992-2012-DS0002-bundles-with-study-info/TEDS-A-1992-2012-DS0002-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0002"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0002-bndl-data-tsv/TEDS-A-1992-2012-DS0002-data")
        }
        if (!exists(objectname)){
          TEDS.DS0002 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                             header = TRUE,colClasses = "factor"))
        }
        gc()}
        ## SAMHDA (TEDS-A-1992-2012-DS0003) - 2000-2004
        {setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0003-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0003-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0003/TEDS-A-1992-2012-DS0003-bundles-with-study-info/TEDS-A-1992-2012-DS0003-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0003"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0003-data")
        }
        if (!exists(objectname)){
          TEDS.DS0003 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()}
        ## SAMHDA (TEDS-A-1992-2012-DS0004) - 2005-2009
        {setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0004-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0004-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0004/TEDS-A-1992-2012-DS0004-bundles-with-study-info/TEDS-A-1992-2012-DS0004-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0004"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0004-data")
        }
        if (!exists(objectname)){
          TEDS.DS0004 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()}
        ## SAMHDA (TEDS-A-1992-2012-DS0005) - 2010-2012
        {setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0005-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0005-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0005/TEDS-A-1992-2012-DS0005-bundles-with-study-info/TEDS-A-1992-2012-DS0005-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0005"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0005-data")
        }
        if (!exists(objectname)){
          TEDS.DS0005 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()}
        ## SAMHDA Data Management and Pruning
        OpiodsAdmissions <- c(5:7)
        groupingvars <- c("FIPStxt", "YEAR")
          ## CBSAtoFIPS_SAMHDA_Subset
          {
          objectname <- "CBSAtoFIPS_SAMHDA_Subset"
          if (!exists(objectname)){
            cols <- c(1,13)
            CBSAtoFIPS_SAMHDA_Subset <- CBSAtoFIPS[,cols]
            colnames(CBSAtoFIPS_SAMHDA_Subset)[colnames(CBSAtoFIPS_SAMHDA_Subset) == 'cbsacode'] <- 'CBSA'
            }
        }
          ## Filtering down to Opioid Cases, Left_Joining FIPStxt, and then Aggregating case counts by FIPStxt and Year
            {    
            TEDS.DS0002 <- TEDS.DS0002 %>% filter(SUB1==OpiodsAdmissions)
            TEDS.DS0002 <- left_join(x = TEDS.DS0002, y = CBSAtoFIPS_SAMHDA_Subset, by = "CBSA")
            TEDS.DS0002.g <- TEDS.DS0002 %>% filter(SUB1 != -9, CBSA != -9) %>% 
                             group_by(FIPStxt, CBSA, YEAR) %>% 
                             summarise(casecount = sum(!is.na(SUB1)))
            rm(TEDS.DS0002)
            gc()
            } ## TEDS.DS0002
            {
            TEDS.DS0003 <- TEDS.DS0003 %>% filter(SUB1==OpiodsAdmissions) 
            TEDS.DS0003 <- left_join(x = TEDS.DS0003, y = CBSAtoFIPS_SAMHDA_Subset, by = "CBSA")
            TEDS.DS0003.g <- TEDS.DS0003 %>% filter(SUB1 != -9, CBSA != -9) %>% 
                             group_by(FIPStxt, CBSA, YEAR) %>% 
                             summarise(casecount = sum(!is.na(SUB1)))
            rm(TEDS.DS0003)
            gc()
            } ## TEDS.DS0003
            {
            TEDS.DS0004 <- TEDS.DS0004 %>% filter(SUB1==OpiodsAdmissions)
            TEDS.DS0004 <- left_join(x = TEDS.DS0004, y = CBSAtoFIPS_SAMHDA_Subset, by = "CBSA")
            TEDS.DS0004.g <- TEDS.DS0004 %>% filter(SUB1 != -9, CBSA != -9) %>% 
                             group_by(FIPStxt, CBSA, YEAR) %>% 
                             summarise(casecount = sum(!is.na(SUB1)))
            rm(TEDS.DS0004)
            gc()
            } ## TEDS.DS0004
            {
            TEDS.DS0005 <- TEDS.DS0005 %>% filter(SUB1==OpiodsAdmissions)
            TEDS.DS0005 <- left_join(x = TEDS.DS0005, y = CBSAtoFIPS_SAMHDA_Subset, by = "CBSA")
            TEDS.DS0005.g <- TEDS.DS0005 %>% filter(SUB1 != -9, CBSA != -9) %>% 
                             group_by(FIPStxt, CBSA, YEAR) %>% 
                             summarise(casecount = sum(!is.na(SUB1)))
            rm(TEDS.DS0005)
            gc()
            } ## TEDS.DS0005
            {
            TEDS.DS <- union(TEDS.DS0002.g, TEDS.DS0003.g)
            TEDS.DS <- union(TEDS.DS, TEDS.DS0004.g) %>% union(TEDS.DS, TEDS.DS0005.g)
            TEDS.DS$YEAR <- as.factor(TEDS.DS$YEAR)
            TEDS.DS <- TEDS.DS %>% arrange(FIPStxt, CBSA, YEAR)
            TEDS.DS %>% group_by(YEAR) %>% summarize(anncountavg = mean(casecount))
            gc()} ## Creating TEDS.DS for joining to the larger jobsanddrugs datafile
        rm(TEDS.DS0002.g, TEDS.DS0003.g, TEDS.DS0004.g, TEDS.DS0005.g) ## Clean UP
        gc()
        
    ## Census American Community Survey Data 2000 to 2015
      ## Requested from IPUMS and downloaded on 20171011; 
      ## Complete documentation of the file downloaded is available on GitHub including samples and variables
        {## Steven Ruggles, Katie Genadek, Ronald Goeken, Josiah Grover, and Matthew Sobek. 
         ## Integrated Public Use Microdata Series: Version 7.0 [dataset]. Minneapolis, MN: University of Minnesota, 2017. 
         ##  https://doi.org/10.18128/D010.V7.0
          } ## IPUMS Citation
        setwd(workdir)
        zipname <- "usa_00001.csv.gz"
        filename <- "usa_00001.csv"
        objectname <- "ACS_Data"
        ## Unzipping the GZ
        if (!file.exists(filename)){
          bunzip2(zipname, filename, remove = FALSE, skip = TRUE)
        }
        if (!exists(objectname)){
          ACS_Data <- read.csv(filename, header = TRUE, colClasses = "factor", na.strings = "na")
        }
        
        ## Summarize & transform the ACS_Data
          ACS_Data_Pop <- ACS_Data %>% filter(complete.cases(ACS_Data)) %>% group_by(YEAR, STATEFIP, COUNTYFIPS) %>%
                          summarise(n())
          ACS_Data_White <- ACS_Data %>% filter(complete.cases(ACS_Data)) %>% group_by(YEAR, STATEFIP, COUNTYFIPS) %>%
                          filter(RACE==1) %>% summarise(n())
          
    ## Census American Community Survey Data Aggregate Table - GINI Index by County 2006-2016
          {zipname <- "ACS_County_GINI_Index_2006-2016aff_download.zip"
          Gini.folder <- "/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/ACS_County_GINI_Index_2006-2016aff_download"
          filenames <- c("ACS_06_EST_B19083_with_ann.csv", "ACS_07_1YR_B19083_with_ann.csv", "ACS_08_1YR_B19083_with_ann.csv",
                         "ACS_09_1YR_B19083_with_ann.csv", "ACS_10_1YR_B19083_with_ann.csv", "ACS_11_1YR_B19083_with_ann.csv",
                         "ACS_12_1YR_B19083_with_ann.csv", "ACS_13_1YR_B19083_with_ann.csv", "ACS_14_1YR_B19083_with_ann.csv",
                         "ACS_15_1YR_B19083_with_ann.csv", "ACS_16_1YR_B19083_with_ann.csv")
          objectnames <- c("ACS.06","ACS.07","ACS.08","ACS.09","ACS.10",
                            "ACS.11","ACS.12","ACS.13","ACS.14","ACS.15","ACS.16")
          years <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")} ## Variables names for the GINI object creation 
        ## Create the annual file dataframes, join the tables, create a new complete file
          if (!file.exists(zipname)){
            stop("Download ACS tables B19083 from Census.Gov")
          } ## Error out if the folder isn't available
          setwd(Gini.folder)
          ## Create the ACS.GINI Object
          if(!exists("ACS.GINI")){
          {
          if(!exists(objectnames[1])){
            filename <- filenames[1]
            ACS.06 <- read.csv(filename, skip = 1)
            ACS.06 <- ACS.06 %>% mutate(YEAR = years[1])
            }
          }## 2006
          {
            if(!exists(objectnames[2])){
              filename <- filenames[2]
              ACS.07 <- read.csv(filename, skip = 1)
              ACS.07 <- ACS.07 %>% mutate(YEAR = years[2])
            }
          }## 2007
          {
            if(!exists(objectnames[3])){
              filename <- filenames[3]
              ACS.08 <- read.csv(filename, skip = 1)
              ACS.08 <- ACS.08 %>% mutate(YEAR = years[3])
            }
          }## 2008
          {
            if(!exists(objectnames[4])){
              filename <- filenames[4]
              ACS.09 <- read.csv(filename, skip = 1)
              ACS.09 <- ACS.09 %>% mutate(YEAR = years[4])
            }
          }## 2009      
          {
            if(!exists(objectnames[5])){
              filename <- filenames[5]
              ACS.10 <- read.csv(filename, skip = 1)
              ACS.10 <- ACS.10 %>% mutate(YEAR = years[5])
            }
          }## 2010
          {
            if(!exists(objectnames[6])){
              filename <- filenames[6]
              ACS.11 <- read.csv(filename, skip = 1)
              ACS.11 <- ACS.11 %>% mutate(YEAR = years[6])
            }
          }## 2011
          {
            if(!exists(objectnames[7])){
              filename <- filenames[7]
              ACS.12 <- read.csv(filename, skip = 1)
              ACS.12 <- ACS.12 %>% mutate(YEAR = years[7])
            }
          }## 2012
          {
            if(!exists(objectnames[8])){
              filename <- filenames[8]
              ACS.13 <- read.csv(filename, skip = 1)
              ACS.13 <- ACS.13 %>% mutate(YEAR = years[8])
            }
          }## 2013
          {
            if(!exists(objectnames[9])){
              filename <- filenames[9]
              ACS.14 <- read.csv(filename, skip = 1)
              ACS.14 <- ACS.14 %>% mutate(YEAR = years[9])
            }
          }## 2014
          {
            if(!exists(objectnames[10])){
              filename <- filenames[10]
              ACS.15 <- read.csv(filename, skip = 1)
              ACS.15 <- ACS.15 %>% mutate(YEAR = years[10])
            }
          }## 2015
          {
            if(!exists(objectnames[11])){
              filename <- filenames[11]
              ACS.16 <- read.csv(filename, skip = 1)
              ACS.16 <- ACS.16 %>% mutate(YEAR = years[11])
            }
          }## 2016
          ACS.GINI <- union(ACS.06,ACS.07)
          ACS.GINI <- union(ACS.GINI,ACS.08)
          ACS.GINI <- union(ACS.GINI,ACS.09)
          ACS.GINI <- union(ACS.GINI,ACS.10)
          ACS.GINI <- union(ACS.GINI,ACS.11)
          ACS.GINI <- union(ACS.GINI,ACS.12)
          ACS.GINI <- union(ACS.GINI,ACS.13)
          ACS.GINI <- union(ACS.GINI,ACS.14)
          ACS.GINI <- union(ACS.GINI,ACS.15)
          ACS.GINI <- union(ACS.GINI,ACS.16)
          ACS.GINI$YEAR <- as.factor(ACS.GINI$YEAR)
          setwd(workdir)
          filename <- "ACS_06_16_B19083_with_ann.csv"
          write_csv(ACS.GINI,path = filename)
          rm(ACS.06, ACS.07, ACS.08, ACS.09, ACS.10, ACS.11, ACS.12, ACS.13, ACS.14, ACS.15, ACS.16, 
            objectnames, filenames, Gini.folder, years)
          } ## Creates the ACS.GINI object out of the 11 files from Census.Gov
          ## Clean up ACS.GINI
          ACS.GINI$Id <- NULL
          names(ACS.GINI)[names(ACS.GINI) == 'Id2'] <- 'FIPStxt'
          names(ACS.GINI)[names(ACS.GINI) == 'YEAR'] <- 'Year'
          ACS.GINI$FIPStxt <- as.character(ACS.GINI$FIPStxt)
          jobsanddrugs <- left_join(jobsanddrugs,ACS.GINI,c("FIPStxt","Year"))
          