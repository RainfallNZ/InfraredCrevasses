#R Scripts with functions created for the Infra-red Crevasse observation paper

#Check for and load required libraries and packages
list.of.packages <- c("terra","tidyr","dplyr","tidyterra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#' A function to resample a crevasse classification
#'
#' CrevasseClassifierLowRes() Resamples and re-calculates a high resolution crevasse/no-crevasse 
#' classification raster to match a supplied raster's resolution
#'@param HiResCrevasseClassification A SpatRaster object of a binary crevasse classification
#'where 1 = crevasse pixel, 0 = not a crevasse pixel
#'@param AreaOfInterest A SpatVector polygon object of the area of interest
#'@param Viewshed A SpatRaster object mask or areas not to be considered.
#'@param LowResRaster A SpatRaster object in the resolution required for the output
#' and intersecting the area of HiResCrevsaseClassification
#'@author Tim Kerr
#'@return A terra::SpatRaster object

CrevasseClassifierLowRes <- function(HiResCrevasseClassification,
                                     AreaOfInterest,
                                     Viewshed,
                                     LowResRaster){

#load the crevasse classification data resampled to match the orhtho image resolution
CrevasseMask <- HiResCrevasseClassification %>% 
  terra::resample(LowResRaster, method="med") %>%
  terra::mask(AreaOfInterest) %>%
  terra::mask(Viewshed,maskvalues=c(NA,0))

BufferedCrevasses <- HiResCrevasseClassification %>% 
  terra::resample(LowResRaster, method="max") %>%
  terra::ifel(. < 1, NA, .) %>%
  terra::buffer(3)%>%
  terra::mask(AreaOfInterest) 

#Combine the non-crevassed mask with the crevasse mask to give a three class raster
#0 = near crevasses, 1 = crevasses, 2 = not crevassed
CrevNoCrev <- terra::ifel(BufferedCrevasses == 1,yes=CrevasseMask,no= 2)
names(CrevNoCrev) <- "Class"

return(CrevNoCrev)
}