## DataLoader takes a filename, a fileurl, and a preferred object name
## the function passes back data.table with all values as character to
## allow for the user to identify their data types moving forward.
## If the user is using xlsx or text files other than .csv, the function assumes
## that, for xls/x data, the user wants the first sheet and that, for text data,
## the preferred seperator is '/t' or tab-seperated data.
##
## Function works best with single URL -> <<decompression>> -> file -> object
## relationships. But should work with tapply if the same URL is paired with
## multiple file -> object relationships.
##
## The function requires the following packages to function:
##      - R.utils ## for unzipping tar and gz files
##      - xlsx ## for Microsoft Excel files
##        - which requires, rJava
##      - tidyverse ## for outputting a data.table
##      - pacman ## for package management
##
## Function variables:
##  - filename: the file specific path to be downloaded and then read
##  - fileurl: the URL where the file can be found
##  - objectname: the datatable name into which the file will be imported
##  - xlsxsheet: number indicating which sheet R should import into the datatable
##        - defaults to "1".
##  - seperator: the specific seperator for a text file be imported
##        - defaults to tab ('/t')
##  - filename2: a second file specific path to be used in cases where
##               the downloaded file is unzipped and the child file needs
##               to be imported under a different name.
##
## If using DataLoader on a table, use mapply
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DataLoader <- function(filename, fileurl, objectname, 
                       xlsxsheet = 1, seperator = '/t', 
                       filename2 = ""){
  ## check packages and install if necessary
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(R.utils, xlsx, tidyverse)
  ## Download the dataset
  if (!file.exists(filename)){
    download.file(fileurl, filename, method = "curl")
  }
  ## Parse whether the file needs to be unzipped
  if(file_ext(filename)="zip"|file_ext(filename)=)
  
  
  if(!exists(objectname)){
    ## Parse what kind of file has been passed in.
    
    ## Read the file if its a basic csv
    
    ## Read the file if its a basic xlsx
    
    ## Read the file if its a tsv or other text file
    
  }
  
}