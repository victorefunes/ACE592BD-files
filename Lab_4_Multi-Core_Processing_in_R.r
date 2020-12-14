
## clean the workspace's current variables
rm(list=ls())

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages using 'pkgTest' function
packages <- c("readxl", "data.table", "ggmap", "ggplot2")
lapply(packages, pkgTest)
    
## the output should be multiple "NULL"s

## find the current working directory
getwd()

## Read the input files under the current working directory
US_P <- readRDS("United_States_Property.rds")
US_D <- readRDS("United_States_Daily.rds")
print(nrow(US_D))

## Now we can examine the US Properties Dataset by printing the first 5 elements.
head(US_P)

## We can examine the US Bookings Dataset by printing the first 5 elements.
head(US_D)

## Creating the city & state field
US_P$City_State <- paste(US_P$City,"_", US_P$State, sep="")

## Select all unique markets using unique City X State pairs
Markets  <- unique(US_P$City_State, incomparables=FALSE)

## For this excercise we will only use the first 10 markets
Markets1 <- Markets[c(1,3,4,5,6)]
print(Markets1)

## We can demonstrate the serial process by selecting one specific city
y <- Markets[1]
y

## Choose market
TH_P <- subset(US_P, City_State==y)
head(TH_P)

## Subset Daily Data by Market
TH_ID <- as.vector(TH_P$Property.ID)
TH_D <- US_D[which(US_D$Property.ID %in% TH_ID),]
head(TH_D)

# Define Market Characteristics
City <- TH_P$City[1]
State <- TH_P$State[1]

## Modify date format
TH_D$date <- as.Date(as.character(TH_D$Date), format="%Y-%m-%d")
TH_D$datepos <- as.POSIXlt(TH_D$date)
print("final form of TH_D")
head(TH_D)

## Compute Property Level Potential Revenue
TH_D$PotentialRevenue <- 0
TH_D$PotentialRevenue[which(TH_D$Status=="A" | TH_D$Status=="R")] <- TH_D$Price[which(TH_D$Status=="A" | TH_D$Status=="R")]

## Compute Property Level Revenue
TH_D$Revenue <- 0
TH_D$Revenue[which(TH_D$Status=="R")] <- TH_D$Price[which(TH_D$Status=="R")]

## Compute Market Level Potential Revenue
PotentialRevenue <- aggregate(PotentialRevenue ~ date, data=TH_D, FUN=sum, na.rm=TRUE)

## Compute Market Level Revenue
Revenue <- aggregate(Revenue ~ date, data=TH_D, FUN=sum, na.rm=TRUE)

## Merge Revenue Data
TH_Daily <- merge(PotentialRevenue,Revenue, by = c("date"))
TH_Daily$RevenueFraction <- as.numeric(TH_Daily$Revenue/TH_Daily$PotentialRevenue)
TH_Daily$City <- City
TH_Daily$State <- State

head(TH_Daily)

## SaveRDS
data_name = paste(file.path(path.expand('~'),'outputs','Lab4'), "/", y, ".rds", sep="")  
saveRDS(TH_Daily, data_name)

# Plot Potential and Actual Revenue by Date
ggplot() +
geom_line(aes(TH_Daily$date, TH_Daily$PotentialRevenue), colour='black') +
geom_line(aes(TH_Daily$date, TH_Daily$Revenue), colour='red')

# Print file
graph_name = paste(file.path(path.expand('~'),'outputs','Lab4'), "/Revenue_", y, ".png", sep="")
ggsave(graph_name, width = 8, height = 5)

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages using 'pkgTest' function
packages <- c("foreach", "doParallel")
lapply(packages, pkgTest)

findModel <- function(y, US_P, US_D){  

  print(paste0(y, " beginning"))

  ## Choose market
  TH_P <- subset(US_P, City_State==y)
  
  ## Subset Daily Data by Market
  TH_ID <- as.vector(TH_P$Property.ID)
  TH_D <- US_D[which(US_D$Property.ID %in% TH_ID),]
  
  if(nrow(TH_D) == 0){
    print(paste0(y," had 0 terms"))
  } else {

  # Define Market Characteristics
  City <- TH_P$City[1]
  State <- TH_P$State[1]
  
  ## Modify date format
  TH_D$date <- as.Date(as.character(TH_D$Date), format="%Y-%m-%d")
  TH_D$datepos <- as.POSIXlt(TH_D$date)
  
  ## Compute Property Level Potential Revenue
  TH_D$PotentialRevenue <- 0
  TH_D$PotentialRevenue[which(TH_D$Status=="A" | TH_D$Status=="R")] <- TH_D$Price[which(TH_D$Status=="A" | TH_D$Status=="R")]
  
  ## Compute Property Level Revenue
  TH_D$Revenue <- 0
  TH_D$Revenue[which(TH_D$Status=="R")] <- TH_D$Price[which(TH_D$Status=="R")]
  
  ## Compute Market Level Potential Revenue
  PotentialRevenue <- aggregate(PotentialRevenue ~ date, data=TH_D, FUN=sum, na.rm=TRUE)
  
  ## Compute Market Level Revenue
  Revenue <- aggregate(Revenue ~ date, data=TH_D, FUN=sum, na.rm=TRUE)
  
  ## Merge Revenue Data
  TH_Daily <- merge(PotentialRevenue,Revenue, by = c("date"))
  TH_Daily$RevenueFraction <- as.numeric(TH_Daily$Revenue/TH_Daily$PotentialRevenue)
  TH_Daily$City <- City
  TH_Daily$State <- State
  
  ## SaveRDS
  data_name = paste(file.path(path.expand('~'),'outputs','Lab4'), "/", y, ".rds", sep="")
  saveRDS(TH_Daily, data_name)
  
  # Plot Potential and Actual Revenue by Date
  ggplot() +
    geom_line(aes(TH_Daily$date, TH_Daily$PotentialRevenue), colour='black') +
    geom_line(aes(TH_Daily$date, TH_Daily$Revenue), colour='red')
  
  # print file
  graph_name = paste(file.path(path.expand('~'),'outputs','Lab4'), "/Revenue_", y, ".png", sep="")
  ggsave(graph_name, width = 8, height = 5)
  }
  # print finished city
  print(y)

}

np <- detectCores()
np

## Set the number of cores to 4 for testing purposes, on a real run, you will need to use (np-1) number of cores
num_cores <- 4

## making this many copy of work
cl <- makeForkCluster(num_cores)

## register this many number of cores
registerDoParallel(cores=(num_cores))

## Import the number of cores
clusterExport(cl, c())

## Display the starting time
Sys.time()
print("starting parallel")

## Run the foreach parallelization
## NOTE: In the real run, we will be using the entire Markets object instead of only 5 markets in the Markets1 variable.
foreach(i=1:length(Markets1), .combine='c') %dopar% {
  findModel(Markets1[i], US_P, US_D)
}

## End the parallelization
stopCluster(cl)

## Show the ending time
Sys.time()
print("ended parallel")
### Set the job name
#PBS -N Snow Project Trial 2

### Use the bourne shell
#PBS -S /bin/bash

### To send email when the job is completed:
### be --- before execution
### ae --- after execution
#PBS -m ae
#PBS -M zhang303@illinois.edu

### Optionally set the destination for your program's output
### Specify localhost and an NFS filesystem to prevent file copy errors.
#PBS -e localhost:/projects/hackonomics/snowproject/job_parallel.err
#PBS -o localhost:/projects/hackonomics/snowproject/job_parallel.log

### Specify the number of CPU cores for your job
#PBS -l nodes=1:ppn=20

### Tell PBS how much memory you expect to use. Use units of 'b','kb', 'mb' or 'gb'.
#PBS -l mem=200gb

### Tell PBS the anticipated run-time for your job, where walltime=HH:MM:SS
### Your job will be terminated if it exceeds this time
#PBS -l walltime=48:00:00

### loading the module of R and gdal
module load R

Rscript /projects/hackonomics/snowproject/MarketRevenue_parallel.R
## Set the number of cores to 4 for testing purposes, on a real run, you will need to use (np-1) number of cores.
num_cores <- 4 

## TODO: You can use your googled example here or just use this excerise, but make sure you only use 4 cores at most!
## You can learn how to write these from secion 3.2 in this excerise, the process should be very similar.
