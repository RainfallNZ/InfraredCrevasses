#R script to orthorectify images

#This script calls the external "Stereo-pipelines" functions to process images

#Set libraries
#Check for and load required libraries and packages
list.of.packages <- c("R.matlab","lubridate","tidyr","tools","terra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directories and filenames
ProjectsDirectory <- "D:\\Projects"
if (Sys.info()["sysname"]=="Linux") {
  ProjectsDirectory <- "/media/drizzle/70DAC0494B655316/Projects"
  ASP_bin_directory <- "/home/drizzle/StereoPipeline-3.4.0-alpha-2024-03-05-x86_64-Linux/bin" 
}
ProjectDirectory  <- file.path(ProjectsDirectory,"UC","Heather Purdie","Purdie-TasmanSaddle-Crevasses")

UCMirrorDirectory <- file.path(ProjectDirectory,"CopiesFromUC")
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability","Data")
MatlabFile        <- file.path(UCMirrorDirectory,"IR camera stuff","data analysis from Raj for 2020","Tier_4_timeseries.mat")
DEMFile           <- file.path(DataDirectory,"GIS","DEM_AOI_and_TG_NZTM_50cm.tif")
DEMFile          <- file.path(UCMirrorDirectory,"TG_2020_GIS","TG_2020_demNZTM.tif")
CameraFile        <- file.path(DataDirectory,"imgV11.tsai")
ImageMaskFile     <- file.path(DataDirectory,"GIS","IRCameraAOI.shp")

#An auxiliary function to convert Matlab date numbers to R date numbers
Matlab2RdateTime <- function(val) as.POSIXct((val-1)*86400, origin = "0000-01-01", tz = "UTC")

#Load image data
ImageData <- readMat(MatlabFile)

#Throw out all the missing data
MissingDataIndices <- which(is.na(ImageData$mean.timeseries))
ImageData$timeseries<- ImageData$timeseries[,,-MissingDataIndices]
ImageData$mean.timeseries <- ImageData$mean.timeseries[-MissingDataIndices]
ImageData$time <- ImageData$time[-MissingDataIndices]

#Get image date times
#Convert, but force TimeZone name to be NZST
RDateTimes <- Matlab2RdateTime(ImageData$time) %>% force_tz(tzone = "NZ")

#Load the image mask file
ImageMask <- terra::vect(ImageMaskFile)

# # Open the 20th image
# TestImage <- terra::rast(ImageData$timeseries[,,20])
# #Create a new image where the cell values are the same as the cell numbers.
# #When this is projected, it can be used to back-project surface characteristic data
# TestImageCellNumbers <- terra::cells(TestImage)
# TestValuesImage <- TestImage
# TestValuesImage[] <- TestImageCellNumbers
# #Save it for later use
# ValuesImageFileName <- file.path("/home","drizzle","Downloads","ValuesImage.tif")
# terra::writeRaster(TestValuesImage,ValuesImageFileName)
# #Specify the proected filename
# ValuesOrthoImageFileName <- file.path("/home","drizzle","Downloads","ValuesOrthoImage.tif")
# #Now project it
# ASP_mappproject <- file.path(ASP_bin_directory,"mapproject")
# ASP_Cmd <- paste0(ASP_mappproject," '",
#                   DEMFile,"' '",
#                   ValuesImageFileName,"' '",
#                   CameraFile,"' '",
#                   ValuesOrthoImageFileName,"'")
# system(ASP_Cmd)
# 
# #Load the resulting orthorectified file and clip to the mask area
# ClippedOrthoImageFileName <- sub("\\.tif$","_clipped.tif",ValuesOrthoImageFileName)
# OrthoImageClipped <- terra::rast(ValuesOrthoImageFileName) %>% terra::project(crs(ImageMask)) %>% terra::mask(ImageMask) %>% terra::crop(ImageMask)
# terra::writeRaster(OrthoImageClipped,ClippedOrthoImageFileName,overwrite=TRUE)

#Process each image in turn
lapply(seq(length(RDateTimes)), function(ImageNo){
#lapply(seq(3), function(ImageNo){
  DateTimeOfImage <- RDateTimes[ImageNo]
  ImageOfInterest <- terra::rast(ImageData$timeseries[,,ImageNo])
  
  TempRawImageFileName <- tempfile(pattern="RawImage",fileext=".tif")
  
  #On linux I am having issues writing files to the D:/drive, so in the interim I'll write to the C:/ drive
  #OrthoImageFileName <- file.path(DataDirectory,"GIS","OrthoImages",paste0("OrthoImage_",sprintf("%02d",ImageNo),"_",format(DateTimeOfImage,"%Y%m%d_%H_%M"),".tif"))
  OrthoImageFileName <- file.path("/home","drizzle","Downloads","OrthoImages",paste0("OrthoImage_",sprintf("%02d",ImageNo),"_",format(DateTimeOfImage,"%Y%m%d_%H_%M"),".tif"))
  TempOrthoImageFileName <- tempfile(pattern = "OrthoImages",fileext=".tif")
  
    terra::writeRaster(ImageOfInterest,TempRawImageFileName)
  
  #Create StereoPipeline command
  #ASP_Cmd <- paste0("cp '",TempRawImageFileName,"' '",OrthoImageFileName,"'") #for testing
  ASP_mappproject <- file.path(ASP_bin_directory,"mapproject")
  ASP_Cmd <- paste0(ASP_mappproject,
                    " --mpp 2 '",
                   DEMFile,"' '",
                   TempRawImageFileName,"' '",
                   CameraFile,"' '",
                   TempOrthoImageFileName,"'")
  system(ASP_Cmd)

  #Load the resulting orthorectified file and clip to the mask area
  ClippedOrthoImageFileName <- sub("\\.tif$","_clipped.tif",OrthoImageFileName)
  OrthoImageClipped <- terra::rast(TempOrthoImageFileName) %>% terra::project(crs(ImageMask)) %>% terra::mask(ImageMask) %>% terra::crop(ImageMask)
  terra::writeRaster(OrthoImageClipped,ClippedOrthoImageFileName,overwrite=TRUE)
  writeLines(paste("Processed Image",ImageNo,"of",length(RDateTimes),"for",format(DateTimeOfImage,"%Y-%m-%d %H:%M"),"to: \n",ClippedOrthoImageFileName))
  return()
})
