#R Script to create publication-ready plot for paper of temperature time series for
#the lower weather station's outgoing long-wave sensor, the camera-based-temperatures of a weather-station proximal
#site, and a before and after correction of a representative crevasse site.
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be single column (86 mm) wide and about 100 mm high.

#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("imager","magick","terra","tidyr","dplyr","ggplot2","ggtext",
                      "tidyterra","gridExtra","grid","gtable","rasterVis","stringr","xts")
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
ExampleIROrthoImageFile  <- file.path(GISDirectory,"OrthoImages","OrthoImage_49_20200225_03_20_clipped.tif")

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
# #Clip the AWS data so that the maximum is 0degC
# Lower_AWS_surface_clipped_TdegC <- pmin(0,Lower_AWS_surfaceTdegC)
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

#Apply corrections to the infra-red data
InfraRedSamples$CameraAtLowerAWS_Corrected <- InfraRedSamples$CameraAtLowerAWS_Raw - InfraRedSamples$Diff
InfraRedSamples$IR_Crevasse_Corrected <- InfraRedSamples$Crevasse_Raw - InfraRedSamples$Diff
InfraRedSamples$Flat_Crevasse_Diff <-  InfraRedSamples$CameraAtLowerAWS_Corrected - InfraRedSamples$IR_Crevasse_Corrected

#Do a timeseries plot showing the flat temperature, crevasse temperature and difference temperature.

PlotData <- InfraRedSamples %>% ggplot2::fortify()
AWSPlotBase <- PlotData %>%
  ggplot() +
  theme_bw() + 
  theme(text=element_text(size=9),
        axis.text = element_text(size=9),
        #axis.text.x=element_blank(),
        legend.text = element_text(size=9),
        legend.key.height = unit(0.3, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(color = NA, fill = NA),
        legend.box.background = element_rect(color=NA,fill = "transparent"),
        legend.position = c(0.52,0.45),
        axis.title.y.left = element_markdown(),
        axis.title.y.right = element_markdown(),
        axis.title.x = element_blank(),
        legend.justification = c(0,1),
        panel.background = element_rect(colour=NA, fill = "transparent"), 
        plot.background = element_rect(colour=NA, fill = "transparent"),
        plot.title = element_text(hjust = 0.1,vjust = -10,size=9))

AWSPlotUpperTemperature <- AWSPlotBase +
  geom_line(aes(x = Index, y = CameraAtLowerAWS_Corrected,linetype = "Non-crevassed"),linewidth=1) +
  geom_line(aes(x = Index, y = IR_Crevasse_Corrected,linetype="Crevasse"),linewidth=1) +
  geom_line(aes(x = Index, y = Flat_Crevasse_Diff,linetype="Difference"),linewidth=1) +
  scale_linetype_manual("", 
                      breaks = c("Difference", "Crevasse", "Non-crevassed"),
                      values = c("Crevasse"=1, "Difference"=3, 
                                 "Non-crevassed"=2)) +
  ylab("Surface temperature<br>(<sup>o</sup>C)") +
  scale_x_datetime(date_labels = '%H:%M\n%d %b') +
  theme(axis.text = element_text(size=9))

AWSPlotUpperTemperature


#Now do the 3 am example spatial plot
IRExample  <- terra::rast(ExampleIROrthoImageFile)
TemperatureCorrectedIRExample <- IRExample - as.vector(InfraRedSamples$Diff[46])

#Create a plot of the IR image
IRPlot <- ggplot() +
  geom_spatraster(data=TemperatureCorrectedIRExample) +
  geom_spatvector(data = LowerSamplePolygon,fill=NA,linewidth=1) +
  geom_spatvector_text(data = LowerSamplePolygon,label = "Non-crevassed",vjust="top",hjust="centre",nudge_y = -10) +
  geom_spatvector(data = CrevasseSamplePolygon,fill=NA,linewidth=1) +
  geom_spatvector_text(data = CrevasseSamplePolygon,label = "Crevasse",vjust="bottom",hjust="centre",nudge_y = 10) +
  #  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,1,0,'cm')) +
  scale_fill_gradientn(colours = c("#30123b","#2fb2f4","#a7fc3a","#fc8524","#7e0502"),
                       values=c(0,0.25,0.5,0.75,1),
                       labels = c(-10, -8,-6,-4,-2),
                       breaks = c(-10,-8,-6,-4,-2),
                       na.value=NA,
                       limits=c(-10,-2),
                       name="Temperature (<sup>o</sup>C)",
                       guide=guide_colorbar(title.position = "left",ticks = FALSE,title.vjust = 0.8),
                       oob=scales::squish)+
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

IRPlot

#Combine using gtables, which enables the spatial plot to fill up the space
Plot1 <- ggplotGrob(IRPlot)
Plot2 <- ggplotGrob(AWSPlotUpperTemperature)

#create a 1x2 empty gtable. Set the row and column sizes explicitly to get alignment correct
#This was a real hack!!
tg <- gtable(widths = unit(c(86),c("mm")),heights = unit(c(100,60),c("mm")))
#Add each grob to it's allocated place.
HalfPlot <- gtable_add_grob(tg,Plot1, t=1,b=1,l=1,z=Inf,clip="off")

FullPlot <- gtable_add_grob(HalfPlot,Plot2, t=2,b=2,l=1,r=1,z=Inf,clip="off")
grid.newpage()
grid.draw(FullPlot)

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"TemperatureCorrectedOrthoExample.pdf"),FullPlot,width = 86,units="mm",height = 156, dpi=300, device = "pdf")

ggsave(file.path(outputDirectory,"TemperatureCorrectedOrthoExample.tif"),FullPlot,width = 86, height = 156,units="mm", dpi=300, device = "tiff")
