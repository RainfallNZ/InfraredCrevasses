#Script to view Campbell Data logger files
#This also sorts out the 

#Load libraries
if ("plotly" %in% rownames(installed.packages()) == FALSE) { install.packages("plotly")};library(plotly)
if ("data.table" %in% rownames(installed.packages()) == FALSE) { install.packages("data.table")};library(data.table)
if ("stats" %in% rownames(installed.packages()) == FALSE) { install.packages("stats")};library(stats)
if ("padr" %in% rownames(installed.packages()) == FALSE) { install.packages("padr")};library(padr)
if ("dplyr" %in% rownames(installed.packages()) == FALSE) { install.packages("dplyr")};library(dplyr)
if ("tools" %in% rownames(installed.packages()) == FALSE) { install.packages("tools")};library(tools)

#Function to import Campbell Data logger data. From https://www.campbellsci.com/blog/tool-to-import-data-to-r
#Modified to guess the data delimiter

importCSdata <- function(filename,RetOpt="data"){
  
  if ("data.table" %in% rownames(installed.packages()) == FALSE) { install.packages("data.table")};library(data.table)
  if(RetOpt=="info"){
    # bring in entire header of CSI TOA5 data file for metadata
    stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
    return(stn.info)
  } else {
    # second line of header contains variable names
    #header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
    header <- unname(unlist(fread(file=filename,skip=1,header=FALSE, na.strings=c("NAN"),sep="auto",nrows=1)))
    # bring in data
    #stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
    stn.data <- fread(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep="auto")
    names(stn.data) <- header
    # add column of R-formatted date/timestamps
    #stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
    return(stn.data)}
}

#Function to find the closest match of one sequence with another from https://stackoverflow.com/questions/73513271/how-to-search-for-most-similar-sequence-between-two-datasets-in-r
closest_match <- function(needle, haystack) {
  ln <- length(needle)
  dist <- sapply(seq(length(haystack) - ln + 1) - 1, function(i) {
    sqrt(sum((haystack[i + seq(ln)] - needle)^2))
  })
  list(index = which.min(dist), 
       closest_sequence = haystack[which.min(dist) + seq(ln) -1])
}


#Set file locations
DataDirectory <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses\\Crevasse Temperature Variability\\Data"

#Main event
#Get the Campbell data logger ".dat. file names
LoggerFiles <- list.files(DataDirectory, pattern = ".*dat$", full.names = TRUE)

#Load the data
LoggerData <- lapply(LoggerFiles, importCSdata)
LoggerData <- lapply(LoggerData, as.data.frame)
names(LoggerData) <- file_path_sans_ext(basename(LoggerFiles))

#View the ten minute data
fig <- plot_ly(LoggerData$Blue_CR300_ten_min, x = ~TIMESTAMP, y = ~Air_temp_upper_Avg, type = 'scatter', mode = 'lines',line=list(color="blue"))

fig

fig <- plot_ly(LoggerData$Red_CR300_ten_min, x = ~TIMESTAMP, y = ~Air_temp_upper_Avg, type = 'scatter', mode = 'lines',line=list(color="red"))

fig

#Find continuous time periods in the Blue one minute data
#Start by adding in rows for missing time stamps
#Create a continuous 1-minute time sequence from the start of B through to the end of B
BFull1MinTimeSeries <- seq.POSIXt(LoggerData$Blue_CR300_One_min$TIMESTAMP[1],tail(LoggerData$Blue_CR300_One_min$TIMESTAMP,1), by="min")

FullSeriesWithGaps <- LoggerData$Blue_CR300_One_min %>% pad

#Work through the two temperature parameters and each contiguous non-gap period from the B data 
#to find the time difference that provides the best match
for (parameter in c("Air_temp_lower_Avg","Air_temp_upper_Avg")){
  print(parameter)
  #Find the longest period without gaps
  LongestGapFreeData <- na.contiguous(FullSeriesWithGaps[,parameter])
  #Initialise a series of data yet to process
  RemainingData <- FullSeriesWithGaps
  
  while (length(LongestGapFreeData) > 360){
     print(length(LongestGapFreeData))
    SampleData <- RemainingData[attr(LongestGapFreeData,"tsp")[1]:attr(LongestGapFreeData,"tsp")[2],]
    
    #Figure out the estimated time difference when matching this non-gap data to the full red series
    MatchedSections <- closest_match(needle = SampleData[,parameter],
                                     haystack = LoggerData$Red_CR300_One_min[,parameter])
    
    TimeDiff <- difftime(SampleData$TIMESTAMP[1],LoggerData$Red_CR300_One_min$TIMESTAMP[MatchedSections$index],units="mins")
    print(TimeDiff)
    #Remove the section that has been tested and repeat
    RemainingData[attr(LongestGapFreeData,"tsp")[1]:attr(LongestGapFreeData,"tsp")[2],parameter] <- NA
    LongestGapFreeData <- na.contiguous(RemainingData[,parameter])
  } #end of gap free sections
} #end of parameters

#The above resulted in a difference of 12165754 minutes
Red1MinDataV2 <- LoggerData$Red_CR300_One_min
Red1MinDataV2$TIMESTAMP <- Red1MinDataV2$TIMESTAMP + 12165754 * 60

Red10MinDataV2 <- LoggerData$Red_CR300_ten_min
Red10MinDataV2$TIMESTAMP <- Red10MinDataV2$TIMESTAMP + 12165754 * 60

Output1MinData <- Red1MinDataV2
Output1MinData$TIMESTAMP <- format(Output1MinData$TIMESTAMP,"%Y-%m-%d %H:%M:%S")
write.csv(Output1MinData,file=file.path(DataDirectory,"Red_CR300_One_minV2.csv"),row.names=FALSE)

Output10minData <- Red10MinDataV2
Output10minData$TIMESTAMP <- format(Output10minData$TIMESTAMP,"%Y-%m-%d %H:%M:%S")
write.csv(Output10minData,file=file.path(DataDirectory,"Red_CR300_ten_minV2.csv"),row.names=FALSE)

#Do a plot to check it is all allright
PlotData <- LoggerData$Blue_CR300_One_min[,c("TIMESTAMP","Air_temp_upper_Avg")]
PlotData <- merge(PlotData,Red1MinDataV2[,c("TIMESTAMP","Air_temp_upper_Avg")],by=c("TIMESTAMP"),all=TRUE,suffixes = c("_blue","_red"))

fig <- plot_ly(PlotData, x = ~TIMESTAMP, y = ~Air_temp_upper_Avg_blue,line=list(color="blue"), type = 'scatter', mode = 'lines') %>%
  add_lines(y=~Air_temp_upper_Avg_red,line=list(color="red"))
fig
