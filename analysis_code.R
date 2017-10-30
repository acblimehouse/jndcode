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
    { 
      library(tidyverse)
      library(plm)
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
      ## Cleanup
      gc() ## to help improve performance of the code and decrease the chance of rJava erroring out.
    }
## ~~~ Section 3 - Analysis Dependent Var #1 - Narcotics Poisonings ~~~ ##

## ~~~ Section 4 - Analysis Dependent Var #2 - Drug Treatment Commitments ~~~ ##