#R Script to create publication-ready surface characteristics plot for paper.
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be double column (178 mm) wide and about 205 mm high.
#The colorscale is based on that used in Purdie et al. (2022) Figure 8.
#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("lubridate","tidyr","terra","ggplot2","cowplot","ggtext","tidyterra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")

#DEMFile           <- file.path(DataDirectory,"GIS","DEM_AOI_and_TG_WGS84_50cm.tif")
ViewshedFile      <- file.path(DataDirectory,"GIS","Viewshed.tif")
WeatherStaionSitesFile <- file.path(DataDirectory,"GIS","GEO-XH.shp")
CameraLocationFile <- file.path(DataDirectory,"GIS","IRCamera.shp")
AreaOfInterestFile<- file.path(DataDirectory,"GIS","IRCameraAOI.shp")

SurfaceCharacteristics <- c("DEM_AOI_and_TG_WGS84_50cm","Slope","Aspect","AzimuthAngle","AltitudeAngle","DistanceFromCamera")
SurfaceCharacterFiles <- lapply(SurfaceCharacteristics, function(x) {
  file.path(DataDirectory,"GIS",paste0(x,".tif"))
})
names(SurfaceCharacterFiles) <- SurfaceCharacteristics

#Load the viewshed raster
Viewshed <- terra::rast(ViewshedFile)

#Load the spatial vector data
AOI <- terra::vect(AreaOfInterestFile)
Camera <- terra::vect(CameraLocationFile)
WeatherStations <- terra::vect(WeatherStaionSitesFile) %>% tidyterra::filter(Comment %in% c("aws lower","aws top"))

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

#Load the rasters
SurfaceCharacterData <- lapply(SurfaceCharacterFiles, function(SurfaceCharacterFile) {
  #Load the data file, mask to the viewshed and clip to the area of interest
  SingleCharactersData <- terra::rast(SurfaceCharacterFile) %>%
    terra::crop(Viewshed) %>%
    terra::mask(Viewshed,inverse=TRUE, maskvalues=1) %>%
    terra::mask(AOI) %>%
    terra::crop(AOI)
})

SurfaceCharacteristicPlots <- lapply(SurfaceCharacteristics, function(SurfaceCharacteristic){
  SinglePlot <- ggplot() +
    geom_spatraster(data=SurfaceCharacterData[[SurfaceCharacteristic]]) +
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
                         limits=SurfaceCharacterPlotLimits[[SurfaceCharacteristic]],
                         name=SurfaceCharacterLegendTitles[SurfaceCharacteristic]) +
    ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)
  return(SinglePlot)
})

#Create a gridded plot - takes 30 seconds
FullPlot <- do.call("plot_grid",c(SurfaceCharacteristicPlots,list(ncol=2,align = "v")))

#Save as pdf
ggsave(file.path(outputDirectory,"SurfaceCharacteristicsPlot.pdf"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "pdf")
