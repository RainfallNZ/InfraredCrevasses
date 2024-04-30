#R Script to create publication-ready surface temperature time-series plots for a paper.
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be double column (178 mm) wide and about 205 mm high.
#The colorscale is based on that used in Purdie et al. (2022) Figure 8.
#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("lubridate","tidyr","terra","ggplot2","cowplot","ggtext","tidyterra","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
OrthoFileDirectory<- file.path(DataDirectory,"GIS","OrthoImages")

#DEMFile           <- file.path(DataDirectory,"GIS","DEM_AOI_and_TG_WGS84_50cm.tif")
ViewshedFile      <- file.path(DataDirectory,"GIS","Viewshed.tif")
WeatherStaionSitesFile <- file.path(DataDirectory,"GIS","GEO-XH.shp")
CameraLocationFile <- file.path(DataDirectory,"GIS","IRCamera.shp")
AreaOfInterestFile<- file.path(DataDirectory,"GIS","IRCameraAOI.shp")

Camera <- terra::vect(CameraLocationFile)
WeatherStations <- terra::vect(WeatherStaionSitesFile) %>% tidyterra::filter(Comment %in% c("aws lower","aws top"))

SurfaceCharacteristics <- c("Elevation","Slope","Aspect","AzimuthAngle","AltitudeAngle","DistanceFromCamera")
SurfaceCharacterFiles <- lapply(SurfaceCharacteristics, function(x) {
  file.path(DataDirectory,"GIS",paste0(x,".tif"))
})
names(SurfaceCharacterFiles) <- SurfaceCharacteristics

#Specify plot legend labels and limits for each surface characteristic
SurfaceCharacterLegendTitles <- c("Elevation<br>(metres above elipsoid)",
                                  "Slope<br>(degrees above horizontal)",
                                  "Aspect<br>(degrees from north)",
                                  "Horizontal angle from camera<br>(degrees from north)",
                                  "Vertical angle from camera<br>(degrees above horizontal)",
                                  "Distance to camera<br>(m)")
names(SurfaceCharacterLegendTitles) <- SurfaceCharacteristics
SurfaceCharacterPlotLimits <- list(c(2250,2400),c(0,90),c(0,360),c(70,150),c(-25,0),c(150,700))
names(SurfaceCharacterPlotLimits) <- SurfaceCharacteristics

#Load the OrthoImages into a terra::SpatRaster object
OrthoFileNames <- list.files(OrthoFileDirectory,full.names = TRUE)
OrthoDateTimesText <- basename(OrthoFileNames) %>% str_extract("2020022[4-7]_[0-9]{2}_[0-9]{2}") 
OrthoDateTimes <- OrthoDateTimesText %>% as.POSIXct(tz="NZ",format="%Y%m%d_%H_%M")

#Limit the number of files to load during testing
ImageRange <- c(2:160)
OrthoRasters <- terra::rast(OrthoFileNames[order(OrthoDateTimes)][ImageRange])
OrthoDateTimes <- OrthoDateTimes[order(OrthoDateTimes)][ImageRange]
names(OrthoRasters) <- OrthoDateTimes

#Mask them by the viewshed
Mask <- terra::rast(ViewshedFile) %>% terra::project(OrthoRasters,method="near") %>% terra::resample(OrthoRasters, method = "near")
OrthoRasters <- terra::mask(OrthoRasters, Mask, maskvalues=1, inverse=TRUE)

#Calculate some Christen variables.
#ftrend, the temporal departure of the instantaneous temperature of a pixel from its
#temporal average temperature
#mpattern, the spatial departure of the temporally averaged temperature of a pixel from the entire spatiotemporal average of the time sequence
#mtotal, the spatiotemporal average of all images
#mtrend
#fpattern
#ftotal

averagetemperatureseries <- terra::global(OrthoRasters, fun="mean", na.rm=TRUE) %>% unlist()
mtotal <- averagetemperatureseries %>% mean(na.rm=TRUE)
mtrend <- averagetemperatureseries - mtotal
fpattern <- OrthoRasters - averagetemperatureseries
ftrend   <- OrthoRasters - terra::mean(OrthoRasters)
ftotal   <- ftrend - mtrend

#Find the standard deviation of the fpattern data, plot it, and manually select classification limits
fpatternSD <- terra::app(fpattern,"std")

# #If required, plot using plot_ly to enable manual selection of classification limits
# #Start by converting the rasters into matrices of xyz
# ImageMatrix <- terra::as.matrix(fpatternSD,wide=TRUE)
# testdata <- reshape2::melt(ImageMatrix)
# vals <- unique(scales::rescale(c(testdata[[1]])))
# o <- order(vals, decreasing = FALSE)
# cols <- scales::col_numeric("Blues", domain = NULL)(vals)
# colz <- setNames(data.frame(vals[o], cols[o]), NULL)
# plot_ly(testdata, y = ~(-Var1), x = ~Var2,z = ~value) %>%
#   layout(xaxis = list(range = c(-160,0,150)),
#          yaxis = list(range=c(-140,0))) %>%
#   add_rasterly_heatmap()

# #An alternative plot of the SD values
# terra::plot(fpatternSD)
# #A plot of the probability distribution of the SD values. Not bi-modal
# density(fpatternSD)

#So it's not very bi-modal, but I could simply threshold on below 0.55, and above 0.7
#Select percentile range that covers the high standard deviation values
#Select a percentile range that covers the low standard deviation range
#Select a random sample of pixels from the low SD and high SD sets

SampleNo <- 10
TimeStep <- 6
fpatternsample <- fpattern[[seq(1, dim(fpattern)[3],by=TimeStep)]]
#Sample the high variation pixels
highSDPixels <-fpatternSD %>% terra::clamp(lower = 0.7, values=FALSE) %>% not.na()
highSDPixels[highSDPixels < 1] <- NA
#sample the cells
HighSDPixelsSampleCells <- terra::spatSample(highSDPixels, size = SampleNo, method="random", replace=FALSE,cells=TRUE,na.rm=TRUE)
#Initialise the sample raster
HighSDPixelSamples <- init(highSDPixels,fun=NA)
#Set all the sampled cells to 1
HighSDPixelSamples[HighSDPixelsSampleCells$cell] <- 1
HighSDSamplePoints <- terra::as.points(HighSDPixelSamples)
#Plot if you want
{
terra::plot(highSDPixels, col="black")
terra::points(HighSDSamplePoints, col="green",pch=1,cex=1.1)
}

DifferenceToImagePlot <- ggplot() +
    geom_spatraster(data=fpatternSD) +
    geom_spatvector(data = Camera, shape = 19,size=4) +
    geom_spatvector(data = WeatherStations, shape = 17, size=4) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text=element_text(size=9),
          legend.text = element_text(size=9),
          panel.border = element_blank(),
          legend.key.height = unit(1, 'cm'),
          legend.title = element_markdown(),
          legend.position=c(-0.25,0.55),
          legend.background = element_rect(fill="transparent"),
          legend.justification = c(0,0.5),
          plot.margin=margin(0,0,0,2,'cm')) +
    scale_fill_gradientn(colours = c("#5153a6","#c7a53f","#cc7833","#e8481c","#ff0004"),
                         values=c(0,0.25,0.5,0.75,1),
                         na.value=NA,
                         #limits=c(0,10),
                         name="difference-to-image SD") +
    ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

ClassifiedPlot <- ggplot() +
  geom_spatraster(data=highSDPixels, colour = "orange") +
  geom_spatraster(data=LowSDPixels, colour = "purple") +
  geom_spatvector(data = Camera, shape = 19,size=4) +
  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(1, 'cm'),
        legend.title = element_markdown(),
        legend.position=c(-0.25,0.55),
        legend.background = element_rect(fill="transparent"),
        legend.justification = c(0,0.5),
        plot.margin=margin(0,0,0,2,'cm')) +
  scale_fill_gradientn(colours = c("#5153a6","#c7a53f","#cc7833","#e8481c","#ff0004"),
                       values=c(0,0.25,0.5,0.75,1),
                       na.value=NA,
                       #limits=c(0,10),
                       name="difference-to-image SD") +
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"fpatternSpatialPlot.pdf"),DifferenceToImagePlot,width = 86, height = 100,units="mm", dpi=300, device = "pdf")
ggsave(file.path(outputDirectory,"fpatternSpatialPlot.tif"),FullPlot,width = 86, height = 100,units="mm", dpi=300, device = "tiff")


#Sample the low variation pixels
LowSDPixels <- fpatternSD %>% terra::clamp(upper = 0.55, values=FALSE) %>% not.na()
LowSDPixels[LowSDPixels < 1] <- NA

#sample the cells
LowSDPixelsSampleCells <- terra::spatSample(LowSDPixels, size = SampleNo, method="random", replace=FALSE,cells=TRUE,na.rm=TRUE)
#Initialise a raster with all NA's
LowSDPixelSamples <- terra::init(LowSDPixels,fun=NA)
#Set all the sampled cells to 1
LowSDPixelSamples[LowSDPixelsSampleCells$cell] <- 1
LowSDSamplePoints <- terra::as.points(LowSDPixelSamples)
# #Plot if you want
# {
#   terra::plot(LowSDPixels, col="black")
#   terra::points(LowSDSamplePoints, col="green",pch=1,cex=1.1) 
# }

#Add the surface characteristic data to fpattern data
fpatternHighSDSamples <- terra::extract(fpatternsample,HighSDSamplePoints,raw=TRUE,ID=FALSE) %>% t() %>% as.data.frame()
fpatternHighSDSamples$SD <- "High"
fpatternHighSDSamples$DateTime <- as.POSIXct(names(fpatternsample))
#Convert to long format
PlotDataHigh <- tidyr::pivot_longer(fpatternHighSDSamples, cols=starts_with("V"),names_to="SampleID",values_to="fpattern")

#Sample the surface characteristics at the same locations and add to the plot data
for(SurfaceCharacter in SurfaceCharacteristics){
  CharacterData <- terra::rast(SurfaceCharacterFiles[[SurfaceCharacter]])
  Samples <- terra::extract(CharacterData,HighSDSamplePoints,raw=TRUE,ID=TRUE) %>% as.data.frame() %>% dplyr::mutate(ID = paste0("V",ID))
  PlotDataHigh[SurfaceCharacter] <- Samples[,2][match(PlotDataHigh$SampleID,Samples$ID)]
}

#Now do the other SD class
fpatternLowSD <- terra::extract(fpatternsample,LowSDSamplePoints,raw=TRUE,ID=FALSE) %>% t() %>% as.data.frame()
fpatternLowSD$DateTime <- as.POSIXct(names(fpatternsample))
fpatternLowSD$SD <- "Low"
#Convert to long format
PlotDataLow <- tidyr::pivot_longer(fpatternLowSD, cols=starts_with("V"),names_to="SampleID",values_to="fpattern")

#Sample the surface characteristics at the same locations and add to the plot data
for(SurfaceCharacter in SurfaceCharacteristics){
  CharacterData <- terra::rast(SurfaceCharacterFiles[[SurfaceCharacter]])
  Samples <- terra::extract(CharacterData,LowSDSamplePoints,raw=TRUE,ID=TRUE) %>% as.data.frame() %>% dplyr::mutate(ID = paste0("V",ID))
  PlotDataLow[SurfaceCharacter] <- Samples[,2][match(PlotDataLow$SampleID,Samples$ID)]
}

#Combine the high and Low SD plot data
PlotData <- rbind(PlotDataHigh,PlotDataLow)
PlotData <- PlotData[order(PlotData$DateTime),]
#Only use 1 in every 3 values (values from every 30 minutes) for clarity
#PlotData <- PlotData[seq(1, nrow(PlotData),by=6),]

#Now prepare the plot
SDPlots <- lapply(SurfaceCharacteristics, function(SurfaceCharacteristic){
  SinglePlot <- PlotData %>% 
    ggplot(aes(x = DateTime, y = fpattern, fill = .data[[SurfaceCharacteristic]])) +
    geom_point(aes(shape = SD),size=3,stroke=NA,alpha=4/5) +
    scale_shape_manual(values = c(25,21)) +
    guides(shape='none')+
    theme_bw() + 
    #theme(text=element_text(family="Arial",size=9),
    theme(text=element_text(size=9),
          axis.text = element_text(size=9),
          legend.text = element_text(size=9),
          legend.key.height = unit(0.3, 'cm'),
          legend.title = element_markdown(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.justification = c(0,1)) + 
    scale_fill_gradientn(colours = c("#5153a6","#c7a53f","#cc7833","#e8481c","#ff0004"),
                         values=c(0,0.25,0.5,0.75,1),
                         na.value=NA,
                         #limits=SurfaceCharacterPlotLimits[[SurfaceCharacteristic]],
                         name=SurfaceCharacterLegendTitles[SurfaceCharacteristic])

  #For all except the last characteristic, remove the x-axis tick text
  if(!identical(SurfaceCharacteristic, tail(SurfaceCharacteristics,1))){
    SinglePlot <- SinglePlot +
      theme(axis.text.x = element_blank())
  }
  return(SinglePlot)
})


#Create a gridded plot, but it is missing a common y axis label
plot <- do.call("plot_grid",c(SDPlots,list(ncol=1,rel_heights = c(1,1,1,1,1,1.1),align = "v")))

#Create the common y-axis label
yAxisLabelGridObject <- gridtext::richtext_grob("<i>fpattern</i><br>Pixel temperature difference from image average (<sup>o</sup>C)",rot=90)

FullPlot <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot, left = yAxisLabelGridObject))
FullPlot
#Save as pdf

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"fpatternPlot.pdf"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "pdf")
ggsave(file.path(outputDirectory,"fpatternPlot.tif"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "tiff")

# 

# 
# #Load the rasters
# SurfaceCharacterData <- lapply(SurfaceCharacterFiles, function(SurfaceCharacterFile) {
#   #Load the data file, mask to the viewshed and clip to the area of interest
#   SingleCharactersData <- terra::rast(SurfaceCharacterFile) %>%
#     terra::crop(Viewshed) %>%
#     terra::mask(Viewshed,inverse=TRUE, maskvalues=1) %>%
#     terra::mask(AOI) %>%
#     terra::crop(AOI)
# })
# 
# SurfaceCharacteristicPlots <- lapply(SurfaceCharacteristics, function(SurfaceCharacteristic){
#   SinglePlot <- ggplot() +
#     geom_spatraster(data=SurfaceCharacterData[[SurfaceCharacteristic]]) +
#     geom_spatvector(data = Camera, shape = 19,size=4) +
#     geom_spatvector(data = WeatherStations, shape = 17, size=4) +
#     theme_classic() +
#     theme(axis.line = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           text=element_text(size=9),
#           legend.text = element_text(size=9),
#           panel.border = element_blank(),
#           legend.key.height = unit(1, 'cm'),
#           legend.title = element_markdown(),
#           legend.position=c(-0.25,0.55),
#           legend.background = element_rect(fill="transparent"),
#           legend.justification = c(0,0.5),
#           plot.margin=margin(0,0,0,2,'cm')) +
#     scale_fill_gradientn(colours = c("#5153a6","#c7a53f","#cc7833","#e8481c","#ff0004"),
#                          values=c(0,0.25,0.5,0.75,1),
#                          na.value=NA,
#                          limits=SurfaceCharacterPlotLimits[[SurfaceCharacteristic]],
#                          name=SurfaceCharacterLegendTitles[SurfaceCharacteristic]) +
#     ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)
#   return(SinglePlot)
# })
# 
# #Create a gridded plot - takes 30 seconds
# FullPlot <- do.call("plot_grid",c(SurfaceCharacteristicPlots,list(ncol=2,align = "v")))
# 
# #Save as pdf for Overleaf, and tif for Word
# ggsave(file.path(outputDirectory,"SurfaceCharacteristicsPlot.pdf"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "pdf")
# ggsave(file.path(outputDirectory,"SurfaceCharacteristicsPlot.tif"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "tiff")
