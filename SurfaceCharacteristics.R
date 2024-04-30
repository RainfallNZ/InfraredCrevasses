#R script

#This script has been prepared to calculate a variety of surface characteristics.

#So far it calculates and saves to a file:
#aspect, angle surface faces with respect to North, in degrees
#slope, +ve angle surface faces with respect a horizontal plane, where 0 is flat, in degrees
#Viewshed, binary raster of whether a surface pixel is within view of the camera point
#AltitudeAngle, angle below the horizontal of a surface point with respect the camera, in degrees
#Azimuthangle, angle, with respect North between camera and surface point, in degrees
#Distance from camera
#These are done in the resoution of the orthorectified image

#******
#Yet to do
#the tangential aspect ratio of a surface
#If the slope and aspect face the distant point, then the aspect ratio is 1.
#If the surface is in shadow from the point, then the aspect ratio is 0.
#Unsure how to calculate the aspect ratio otherwise
#with respect a distant point. However I haven't been able to figure it out.
#I think I need to work in spherical coordinates to sort it out.
#The skyview of each pixel
#The Ozzie Dust distribution
#***********

#These surface characteristics are to be used to test how they relate to the infrared camera temperatures

#Check for and load required libraries and packages
list.of.packages <- c("terra","sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directories and filenames
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
UCMirrorDirectory <- file.path(ProjectDirectory,"CopiesFromUC")
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")

DEMFile           <- file.path(UCMirrorDirectory,"TG_2020_GIS","TG_2020_demNZTM.tif")
OrthoImageFile    <- file.path(DataDirectory,"GIS","OrthoImages","OrthoImage_20_20200224_22_30_clipped.tif")

#Specify the x,y,z of the point of interest
#POI_xyz <- c(1385274.926,5178740.551,2363.246) #From "Geo-XH_TS_Corrected.xlsx"
POI_xyz <- c(1385274.926,5178740.551,2377.424) #From "Geo-XH_TS_Corrected.xlsx but elevation from elipsoid
CameraPoint <- st_point(POI_xyz[1:2])

#Load the DEM
DEM <- terra::rast(DEMFile)

#Load the example OrthoImage
ExampleOrthoImage <- terra::rast(OrthoImageFile) %>% terra::project(y="epsg:2193")
BigExample <- terra::extend(ExampleOrthoImage,DEM)

#Resample the DEM to match the example ortho image resolution
DEM <- terra::project(DEM,y=BigExample)

#calculate the slope and aspect
DEM[DEM < 0] <- NA 
DEM <- terra::trim(DEM)

Slope <- terra::terrain(DEM, v="slope")
Aspect <- terra::terrain(DEM, v="aspect")
Slope_radians <- terra::terrain(DEM, v="slope",unit="radians")
Aspect_radians <- terra::terrain(DEM, v="aspect",unit="radians")

#Initialise some rasters based on the DEM, but to be populated with new values later on
EastingsRaster <- NorthingsRaster <- DEM

#These next calculations are caried out on each cell of the raster. For efficiency the raster has been converted to a vector.
#When the calculations are complete, they are used to update the values of the related raster.
#Get all the values of the DEM
DEMValues <- terra::values(DEM)

#Get all the coordinates 
DEMCoordinates <- terra::xyFromCell(DEM,1:ncell(DEM))

#Calculate the vertical angle between each cell and the point of interest
AltitudeAngleValues <- atan((DEMValues - POI_xyz[3])/((DEMCoordinates[,'y']-POI_xyz[2])^2 + (DEMCoordinates[,'x']-POI_xyz[1])^2)^0.5) * 180 / pi

AltitudeAngle <- AzimuthAngle <- DistanceFromCamera <- DEM
values(AltitudeAngle) <- AltitudeAngleValues
#Calculate the horizontal angle between each cell and the point of interest where directly north is 0 degrees
RawAzimuthAngle <- atan((DEMCoordinates[,'x']-POI_xyz[1])/(DEMCoordinates[,'y']-POI_xyz[2])) * 180 / pi
BottomRightCorner <- ((DEMCoordinates[,'x']-POI_xyz[1]) > 0) * (((DEMCoordinates[,'y']-POI_xyz[2]) < 0))
BottomLeftCorner <- ((DEMCoordinates[,'x']-POI_xyz[1]) < 0) * (((DEMCoordinates[,'y']-POI_xyz[2]) < 0))
TopLeftCorner <- ((DEMCoordinates[,'x']-POI_xyz[1]) < 0) * (((DEMCoordinates[,'y']-POI_xyz[2]) > 0))
QuadrantOffsets    <- (BottomRightCorner | BottomLeftCorner) * 180 + TopLeftCorner * 360
AzimuthAngleValues <- RawAzimuthAngle + QuadrantOffsets
values(AzimuthAngle) <- AzimuthAngleValues

#Figure out the aspect ratio of the surface face to the point, given the slope, aspect, Altitude angle and azimuth angle
#Start with DEM aspect vs camera azimuth
AspectDifference <- abs(Aspect - ((AzimuthAngle + 180)%% 360))

#Get the distance from the camera.
DistanceFromCameraValues <- ((DEMCoordinates[,'x']-POI_xyz[1])^2 + (DEMCoordinates[,'y']-POI_xyz[2])^2 + (DEMValues - POI_xyz[3])^2)^0.5
values(DistanceFromCamera) <- DistanceFromCameraValues

#Get the camera viewshed
Viewshed <- terra::viewshed(DEM,loc = POI_xyz[1:2],observer = 1.5) 

#Get the sky view from each pixel
#Yet to be done....

#Save them all to a file
writeRaster(DEM,file.path(DataDirectory,"GIS","Elevation.tif"),overwrite=TRUE)
writeRaster(Aspect,file.path(DataDirectory,"GIS","Aspect.tif"),overwrite=TRUE)
writeRaster(Slope,file.path(DataDirectory,"GIS","Slope.tif"),overwrite=TRUE)
writeRaster(AzimuthAngle,file.path(DataDirectory,"GIS","AzimuthAngle.tif"),overwrite=TRUE)
writeRaster(AltitudeAngle,file.path(DataDirectory,"GIS","AltitudeAngle.tif"),overwrite=TRUE)
writeRaster(DistanceFromCamera,file.path(DataDirectory,"GIS","DistanceFromCamera.tif"),overwrite=TRUE)
writeRaster(Viewshed,file.path(DataDirectory,"GIS","Viewshed.tif"),overwrite=TRUE)


