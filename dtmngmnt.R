## ~~~ Section 1 - Getting ready to work ~~~ ##
## Home git == https://github.com/acblimehouse/jndcode ##
  ## ID & Set Working Directory
    ## Create WD Object, set as appopriate
    workdir <- "/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data"
    ## Set Working Directory
    setwd(workdir)
  ## Setup Packages
    library(xlsx) ## used later for xlsx data sets
    library(tidyverse) ## used later for csv data sets and for data cleaning and joining
  ## Add useful functions
    factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)] 
    
## ~~~ Section 2 - Pulling Data into R ~~~ ##
  ## Pulling Data into R - ~~~ NOTE: URLs not guaranteed current after Nov 2017 ~~~
    
    ## 2016 FIPS Codes for future data managment
      filename <- "all-geocodes-v2016.xlsx"
      fileURL <- "https://www2.census.gov/programs-surveys/popest/geographies/2016/all-geocodes-v2016.xlsx"
      objectname <- "FIPScodes"
      ## Download the dataset:
      if (!file.exists(filename)){
        download.file(fileURL, filename, method="curl")
      }
      ## Load the data into R
      if (!exists(objectname)){
        FIPScodes <- read.xlsx2(file = filename, sheetIndex = 1, startRow = 5,
                                as.data.frame = TRUE, header = TRUE)
      }
      ## Cleanup
      gc() ## to help performance of the XLSX package

    ## USDA Economic Research Service - Unemployment and median household income for the U.S., States, and counties, 2007-16
      filename <- "USDA ERS Unemployment.xls"
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
                                    StringsAsFactors = TRUE,
                                    header = TRUE)
        usda0716unemp$StringsAsFactors <- NULL
        }
      ## Defactor the numeric columns
      cols <- c(7:ncol(usda0716unemp))
      usda0716unemp[cols] <- lapply(usda0716unemp[cols], factorToNumeric)
      rm(cols)
      ## Remove the aggregate state level data
      usda0716unemp <- filter(usda0716unemp,!usda0716unemp$Metro_2013=="")
      
    ## Create Primary Data Set Object & Clean Up
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
      
    ## CDC Multiple Cause of Death Data - focused on Alcolhol and Opiods
        ## Requested and downloaded from https://wonder.cdc.gov/mcd-icd10.html
        ## Not available as a direct download due to government data restrictions
      objectname <- "cdcmcd9915"
      if (!exists(objectname)){ 
          filename.1 <- "Multiple Cause of Death, 1999-2004.txt"
          filename.2 <- "Multiple Cause of Death, 2005-2010.txt"
          filename.3 <- "Multiple Cause of Death, 2011-2015.txt"
          drops <- c(1) # to remove Notes, County, crude rate, and crude std. err.
          
          ## CDC Multiple Causes of Death, 1999-2004 / five years
          cdcmcd9904 <- as.data.frame(read.csv2(file = filename.1, sep = "\t", 
                                                header = TRUE,colClasses = "factor"))
          cdcmcd9904[,drops] <- NULL ## Drops unnecessary columns
          cdcmcd9904$Deaths <- as.numeric(cdcmcd9904$Deaths) ## Compels numeric value
          cdcmcd9904$Population <- as.numeric(cdcmcd9904$Population) ## Compels numeric value
          
          ## CDC Multiple Causes of Death, 2005-2010 / six years
          cdcmcd0510 <- as.data.frame(read.csv2(file = filename.2, sep = "\t", 
                                                header = TRUE, colClasses = "factor"))
          cdcmcd0510[,drops] <- NULL #Drops unnecessary columns
          cdcmcd0510$Deaths <- as.numeric(cdcmcd0510$Deaths) ## Compels numeric value
          cdcmcd0510$Population <- as.numeric(cdcmcd0510$Population) ## Compels numeric value
          
          ## CDC Multiple Causes of Death, 2011-2015 / five years
          cdcmcd1115 <- as.data.frame(read.csv2(file = filename.3, sep = "\t", 
                                                header = TRUE, colClasses = "factor"))
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
## Doesn't work yet /fixme ##
          ## Widening the CDC Data on Drug, Alcohol, Other-cause Deaths by year
            ## Create "Other Deaths" filtered data set
          cdcmcd9915.O <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="O")
          cdcmcd9915.O$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL
          colnames(cdcmcd9915.O)[colnames(cdcmcd9915.O)== 'Deaths'] <- 'other.deaths'
          colnames(cdcmcd9915.O)[colnames(cdcmcd9915.O)== 'Population'] <- 'other.population'
            ## Create "Alcohol Deaths" filtered data set
          cdcmcd9915.A <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="A")
          cdcmcd9915.A$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.A)== 'Deaths'] <- 'alcohol.deaths'
          colnames(cdcmcd9915.A)[colnames(cdcmcd9915.A)== 'Population'] <- 'alcohol.population'
            ## Create "Drug Deaths" filtered data set
          cdcmcd9915.D <- filter(cdcmcd9915, UCD...Drug.Alcohol.Induced.Causes.Code=="D")
          cdcmcd9915.D$UCD...Drug.Alcohol.Induced.Causes.Code <- NULL
          colnames(cdcmcd9915.D)[colnames(cdcmcd9915.D)== 'Deaths'] <- 'drug.deaths'
          colnames(cdcmcd9915.D)[colnames(cdcmcd9915.D)== 'Population'] <- 'drug.population'
      
            ## Rejoin the data sets together using Year and County.Code
          joinvar <- c("Year","County.Code")
          cdcmcd9915[,c(3:5)] <- NULL
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.O, by = joinvar)
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.A, by = joinvar)
          cdcmcd9915 <- full_join(cdcmcd9915,cdcmcd9915.D, by = joinvar)
          columnremoves <- c(3:5, 7)
          rm(cdcmcd9915.O,cdcmcd9915.D,cdcmcd9915.A)
      }
      ## Bring jobsanddrugs and the CDC data set together
        ## full join on Year and FIPStxt
      
##Fix ME      
    ## SAMHDA (TEDS-A-1992-2012-DS000X) - Treatment Episode Data Set 1992 to 2012 (only downloads 1995 - 2012)
        ## SAMHDA (TEDS-A-1992-2012-DS0002)
        zipname <- "TEDS-A-1992-2012-DS0002-bndl-data-tsv.zip"
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
        gc()
        ## SAMHDA (TEDS-A-1992-2012-DS0003)
        setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0003-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0003-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0003/TEDS-A-1992-2012-DS0003-bundles-with-study-info/TEDS-A-1992-2012-DS0003-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0003"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0003-bndl-data-tsv/TEDS-A-1992-2012-DS0003-data")
        }
        if (!exists(objectname)){
          TEDS.DS0002 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()
        
        ## SAMHDA (TEDS-A-1992-2012-DS0004)
        setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0004-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0004-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0004/TEDS-A-1992-2012-DS0004-bundles-with-study-info/TEDS-A-1992-2012-DS0004-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0004"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0004-bndl-data-tsv/TEDS-A-1992-2012-DS0004-data")
        }
        if (!exists(objectname)){
          TEDS.DS0004 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()
        
        ## SAMHDA (TEDS-A-1992-2012-DS0005)
        setwd(workdir)
        zipname <- "TEDS-A-1992-2012-DS0002-bndl-data-tsv.zip"
        filename <- "TEDS-A-1992-2012-DS0002-data-excel.tsv"
        fileURL <- "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/TEDS-A-1992-2012/TEDS-A-1992-2012-datasets/TEDS-A-1992-2012-DS0005/TEDS-A-1992-2012-DS0005-bundles-with-study-info/TEDS-A-1992-2012-DS0005-bndl-data-tsv.zip"
        objectname <- "TEDS.DS0005"
        if (!file.exists(zipname)){
          download.file(fileURL, zipname, method="curl")
        }
        if (!file.exists(filename)) { 
          unzip(zipname)
          setwd("/Users/adamlimehouse/Desktop/Dropbox/03 Projects Folder/Economic and Policy Analysis/Jobs and Drugs Poster/jndcode/JnD Data/TEDS-A-1992-2012-DS0005-bndl-data-tsv/TEDS-A-1992-2012-DS0005-data")
        }
        if (!exists(objectname)){
          TEDS.DS0005 <- as.data.frame(read.csv2(file = filename, sep = "\t", 
                                                 header = TRUE,colClasses = "factor"))
        }
        gc()