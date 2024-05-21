#R Script to adjust orthoimage temperatures using the difference between the lower AWS
#surface temperature (from the outgoing longwave radiation) and the camera value nearby.

#*****************************************
#* WARNING WARNING
#* Much of this script is duplicated in CorrectedTemperaturePlot.R
#* If anything is changed that affects the temperature correction of the images,
#* then CorrectedTemperaturePlot.R should also be edited.
#* Some re-factoring should be done to resolve this
#* ***************************************************

#Check for and load required libraries and packages
list.of.packages <- c("terra","tidyr","dplyr","tidyterra","stringr","xts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
UCDirectory       <- file.path(ProjectDirectory,"CopiesFromUC") 
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
GISDirectory      <- file.path(DataDirectory,"GIS")
OrthoFileDirectory<- file.path(GISDirectory,"OrthoImages")
OrthoFileNames <- list.files(OrthoFileDirectory,full.names = TRUE)
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
CrevasseClassFile <- file.path(GISDirectory,"CrevasseClassified.tif")
ViewshedFile <- file.path(GISDirectory,"Viewshed.tif")
AreaOfInterestFile <- file.path(GISDirectory,"IRCameraAOI.shp")
WeatherStaionSitesFile <- file.path(GISDirectory,"GEO-XH.shp")
LowerWeatherStationDataFile <- file.path(UCDirectory,"TAS_2020","cr300 data","Raw data_CR300","CR300Series-ts-lower_ten_min_latest.dat")

#Load the data
AreaOfInterest <- terra::vect(AreaOfInterestFile)
WeatherStations <- terra::vect(WeatherStaionSitesFile) %>% tidyterra::filter(Comment %in% c("aws lower","aws top"))

#Crevasse sample site coordinates in NZTM. SUbjectively selected from viewing the ortho imagery in GIS together
#with the crevasse-classified layer, and the viewshed layer
CrevasseSite <- data.frame("y"=5178632,"x"=1385608) %>% terra::vect(geom=c("x","y"),crs = crs(WeatherStations))

#Load the ortho images from near the start to the break in data
OrthoDateTimesText <- basename(OrthoFileNames) %>% str_extract("2020022[4-7]_[0-9]{2}_[0-9]{2}") 
OrthoDateTimes <- OrthoDateTimesText %>% as.POSIXct(tz="NZ",format="%Y%m%d_%H_%M")

#Limit the number of files to load to just go up to the first data gap (i.e. after 24 hours)
#and miss the first image as it was during the setup and includes repositioning of the tripod
#which resulted in a distorted 10 minute average image.
ImageRange <- c(2:160)
OrthoRasters <- terra::rast(OrthoFileNames[order(OrthoDateTimes)][ImageRange])
OrthoDateTimes <- OrthoDateTimes[order(OrthoDateTimes)][ImageRange]
names(OrthoRasters) <- OrthoDateTimes

#Load the Long Wave data from the lower weather station and extract the period that matches the thermal imagery
Lower_AWS_Metadata <- readLines(con=LowerWeatherStationDataFile,n=4)
Lower_AWS_Data <- read.csv(LowerWeatherStationDataFile, skip=4,header = FALSE)
names(Lower_AWS_Data) <- Lower_AWS_Metadata[2] %>% gsub('\"','',.) %>% strsplit(",") %>% unlist()
Lower_AWS_Data$TIMESTAMP <- as.POSIXct(Lower_AWS_Data$TIMESTAMP,format = "%Y-%m-%d %H:%M:%S", tz="NZ")

#Restrict the AWS data to just the OrthoImage time period + 1 hour either side
Lower_AWS_Data <- Lower_AWS_Data %>% dplyr::filter(TIMESTAMP > (OrthoDateTimes[1] - 20*60), TIMESTAMP < (tail(OrthoDateTimes,1)+ 20 * 60))

#Calculate the surface temperature under the weather stations using the long wave 
# out radiation and the Stefan Boltzmann's law
StefanBoltzmannConstant <- 5.670374419 * 10^-8 #w /m2/K4
SnowEmissivity <- 0.99 #Need a citation for this, maybe Griggs (1968) Griggs, M., Emissivities of natural surfaces in the 8- to 14-micron spectral region, J. Geophys. Res., 73. 7545-7551, 1968. 
#But see Waren (1982) section M, figure 18 (b) which provides directional emissivities indicating a variation from 0.98 at 80deg to 0.995 for 0deg, though a strong dropoff above 10 um
#T= (M/sigma/emissivity)^0.25 where M is the radiated energy in watts per square metre, sigma is the staffan-Boltzmann constant, and T is the surface temperature in Kelvin
Lower_AWS_surfaceTdegK <- (Lower_AWS_Data$outgoingLW_Avg / StefanBoltzmannConstant / SnowEmissivity)^0.25
Lower_AWS_surfaceTdegC <- Lower_AWS_surfaceTdegK - 273.15
#Offset the AWS data so that the maximum is 0degC
Lower_AWS_surface_offset_TdegC <- Lower_AWS_surfaceTdegC - (max(Lower_AWS_surfaceTdegC) - 0)
#Convert to time series xts objects
Lower_AWS_surfaceTdegC_xts <- xts(cbind(Lower_AWS_surfaceTdegC,Lower_AWS_surface_offset_TdegC), order.by = Lower_AWS_Data$TIMESTAMP)

#Sample the Ortho SpatRasters at the weather station site. Sample a 3 x 3 pixel area 
#and make sure it is reasonably flat and not crevassed.
LowerAWSLocation <- terra::vect(WeatherStaionSitesFile) %>% terra::subset(.$Comment == 'aws lower')
#Get the coordinates of the Climate station and move 10 m south to get into a similar area but away from the station
LowerAWSCoords10mS <- terra::shift(LowerAWSLocation,dy=-10) 

#Create a polygon that buffers this point by at least 1.5 the resolution of the raster
LowerSamplePolygon <- terra::buffer(LowerAWSCoords10mS, width = ceiling(1.5 * terra::res(OrthoRasters)))
CrevasseSamplePolygon <- terra::buffer(CrevasseSite, width = ceiling(1.5 * terra::res(OrthoRasters)))

IR_SampleAtLowerAWS <- terra::extract(OrthoRasters,y = LowerSamplePolygon,fun = mean,ID=FALSE) %>% unlist()
IR_SampleAtCrevasse <- terra::extract(OrthoRasters,y = CrevasseSamplePolygon,fun = mean,ID=FALSE) %>% unlist()

#Create a time series object with xts
IR_SampleLowerAWS_xts <- xts(IR_SampleAtLowerAWS, order.by = OrthoDateTimes)
IR_Crevasse_xts       <- xts(IR_SampleAtCrevasse, order.by = OrthoDateTimes)
#Sample/interpolate the AWS data at the ortho-image times
InfraRedSamples <- merge(CameraAtLowerAWS_Raw = IR_SampleLowerAWS_xts,lowerAWS=Lower_AWS_surfaceTdegC_xts,Crevasse_Raw=IR_Crevasse_xts)
InfraRedSamples$InterpolatedAWS <- na.approx(InfraRedSamples$Lower_AWS_surface_offset_TdegC)
InfraRedSamples <- InfraRedSamples[!is.na(InfraRedSamples$CameraAtLowerAWS_Raw),]

#Calculate a difference
InfraRedSamples$Diff <- InfraRedSamples$CameraAtLowerAWS_Raw - InfraRedSamples$InterpolatedAWS

#Temperature correct all the orthorasters, and save to an rds file for later
TemperatureCorrectedOrthoRaster <- OrthoRasters - as.vector(InfraRedSamples$Diff[46])
#writeRaster(TemperatureCorrectedOrthoRaster,file.path(OrthoFileDirectory,"TemperatureCorrected","DeleteMe.tif"),overwrite=TRUE)
saveRDS(TemperatureCorrectedOrthoRaster,file.path(OrthoFileDirectory,"TemperatureCorrected","TemperatureCorrectedOrthoImages.rds"))
