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
  ## Create Primary Data Set Object
    jobsanddrugs 
    
## ~~~ Section 2 - Pulling Data into R ~~~ ##
  ## Pulling Data into R - ~~~ NOTE: URLs not guaranteed current after Nov 2017 ~~~
    ## USDA Economic Research Service - Unemployment and median household income for the U.S., States, and counties, 2007-16
      filename <- "USDA ERS Unemployment.xls"
      fileURL <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.xls?v=42894"
      objectname <- "usda0716unemp"
      columnclasses <- "character, character, character, character, character, character, "
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
      cols <- c(1, 7:ncol(usda0716unemp))
      usda0716unemp[cols] <- lapply(usda0716unemp[cols], factorToNumeric)
      ## Slice out the aggregate state data
      usda0716unemp <- filter(usda0716unemp,!usda0716unemp$Metro_2013=="")
    ## Create Primary Data Set Object & Clean Up
      jobsanddrugs <- usda0716unemp
      rm(usda0716unemp)
    ## CDC Multiple Cause of Death Data - focused on Alcolhol and Opiods
        ## Requested and downloaded from https://wonder.cdc.gov/mcd-icd10.html
        ## Not available as a direct download due to government data restrictions
      filename.1 <- "Multiple Cause of Death, 1999-2004.txt"
      filename.2 <- "Multiple Cause of Death, 2005-2010.txt"
      filename.3 <- "Multiple Cause of Death, 2011-2015.txt"
      objectname <- "cdcmcd9915"
      if (!exists(objectname)){ ## Doesn't work yet /fixme ##
          cdcmcd9915 <- as.data.frame(read.csv2(file = filename.1,
                                   sep = "\t",
                                  header = TRUE))
            cdcmcd9915 <- setdiff(cdcmcd9915, 
                                as.data.frame(
                                  read.csv2(file = filename.2,
                                            sep = "\t",
                                            header = TRUE)))
            cdcmcd9915 <- filter(cdcmcd9915, cdcmcd9915$Notes=="")
            cdcmcd9915 <- setdiff(cdcmcd9915, 
                                  as.data.frame(
                                    read.csv2(file = filename.3,
                                              sep = "\t",
                                              header = TRUE)))
            }
          
      
      