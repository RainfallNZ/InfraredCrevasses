#R Script to create publication-ready hovmoeller plot for paper of surface characteristic (y axis) 
#time series (x axis) of temperature (colour)

#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be single column (86 mm) wide and about 100 mm high.

#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("imager","magick","terra","tidyr","dplyr","ggplot2","ggtext",
                      "tidyterra","gridExtra","grid","gtable","rasterVis","stringr","xts","lubridate","tibble")
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

#Load the data
#Load the temperature-corrected-ortho images
OrthoData <- readRDS(file.path(OrthoFileDirectory,"TemperatureCorrected","TemperatureCorrectedOrthoImages.rds"))
OrthoDateTimes <- names(OrthoData) %>% as.POSIXct(tz="NZ",format="%Y-%m-%d %H:%M")

#load the crevasse classification data
CrevasseClass <- terra::rast(CrevasseClassFile) %>% terra::resample(OrthoData[[1]], method="max")
#resample to match the orhtho image resolution

BufferedCrevasses <- terra::ifel(CrevasseClass < 1, NA, CrevasseClass) %>%
  terra::buffer(3)
NonCrevasseMask <- terra::ifel(BufferedCrevasses == 1, NA, BufferedCrevasses) %>%
  terra::mask(OrthoData[[1]])

#Load surface characteristics
SurfaceCharacteristics <- c("Elevation","Slope","Aspect","AzimuthAngle","AltitudeAngle","DistanceFromCamera")
SurfaceCharacterFiles <- lapply(SurfaceCharacteristics, function(x) {
  file.path(DataDirectory,"GIS",paste0(x,".tif"))
})
names(SurfaceCharacterFiles) <- SurfaceCharacteristics

SurfaceCharacterData <- lapply(SurfaceCharacterFiles, function(SurfaceCharacterFile) {
  #Load the data file, mask to the viewshed and clip to the area of interest
  SingleCharactersData <- terra::rast(SurfaceCharacterFile) %>%
    terra::resample(OrthoData[[1]],method="near") %>%
    terra::mask(AreaOfInterest) %>%
    terra::mask(NonCrevasseMask)
  SingleCharactersData
})

SurfaceCharacterYAxisTitles <- c("Elevation<br>(m)",
                                  "Slope",
                                  "Aspect",
                                  "Horizontal angle<br>from camera",
                                  "Vertical angle<br>from camera",
                                  "Distance<br>to camera")

#I now need to break the surface characteristic into 19 bins

ClassedSurfaceData <- lapply(SurfaceCharacterData, function(SingleCharacter) {
  Breaks <- pretty(terra::minmax(SingleCharacter) %>% as.vector(),20)
  ReClassed = terra::classify(SingleCharacter, rcl = Breaks)
  return(list(Breaks=Breaks,ReClassed=ReClassed))
})

#I can then use them to get zonal statistics.
ZonedData <- lapply(ClassedSurfaceData, function(SingleClassData) {
  ZonedData <- terra::zonal(SingleClassData$ReClassed,x=OrthoData,fun="mean",na.rm=TRUE)
})

#Create level plots for each surface characteristic using ggplot as described https://stackoverflow.com/questions/58064728/how-to-make-a-level-plot-with-ggplot-with-same-look-of-the-density-plot
#and https://r-graph-gallery.com/79-levelplot-with-ggplot2.html

DefaultAxis <-  ggplot() +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        #axis.text.x = element_blank(),
        #axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_markdown(vjust = 0.8),
        legend.position = "bottom",
        legend.margin = margin(0,0,0,0,'cm'),
        legend.box.margin=margin(-10,-10,-10,-10),
        plot.margin=margin(0,0,0,0,'cm')) +
  scale_fill_gradientn(colours = c("#30123b","#2fb2f4","#a7fc3a","#fc8524","#7e0502"),
                       values=c(0,0.25,0.5,0.75,1),
                       labels = c(-12, -8,-4,0,4,8),
                       breaks = c(-12,-8,-4,0,4,8),
                       na.value=NA,
                       limits=c(-12,8),
                       name="Temperature (<sup>o</sup>C)",
                       guide=guide_colorbar(title.position = "left",ticks = FALSE),
                       oob=scales::squish)

SurfaceCharacterTemperaturePlots <- lapply(seq_along(SurfaceCharacteristics), function(SurfaceCharacteristicIndex){
  
  SingleCharacterData <- ZonedData[[SurfaceCharacteristicIndex]]
  row.names(SingleCharacterData) <- SingleCharacterData[,1]
  SingleCharacterData <- SingleCharacterData[,2:ncol(SingleCharacterData)]
  
  PlotData <- SingleCharacterData %>%
    as_tibble() %>%
    rowid_to_column(var="Y") %>%
    gather(key="X", value="Z", -1) %>%
    mutate(X=as.POSIXct(X) %>% lubridate::round_date("10 minutes")) %>%
    mutate(Y = ClassedSurfaceData[[SurfaceCharacteristicIndex]]$Breaks[Y]+ (ClassedSurfaceData[[SurfaceCharacteristicIndex]]$Breaks[2] - ClassedSurfaceData[[SurfaceCharacteristicIndex]]$Breaks[1])/2)#
  
  SinglePlot <- DefaultAxis +
    geom_tile(data = PlotData,aes(X, Y, fill=Z)) +
    ylab(SurfaceCharacterYAxisTitles[[SurfaceCharacteristicIndex]]) +
    scale_x_datetime(date_labels = '%H:%M\n%d %b') #+
  if(!SurfaceCharacteristicIndex %in% c(5,6)) SinglePlot <- SinglePlot +
    theme(axis.text.x = element_blank(),)

  return(SinglePlot)
})
#Stick together as a panel plot
#Create a gridded plot - takes 30 seconds
legend_b <- get_legend(SurfaceCharacterTemperaturePlots[[1]])
#Strip the legends from the plots
NoLegendPlots <- lapply(SurfaceCharacterTemperaturePlots, function(x) {
  x + theme(legend.position = "none")
})
FullPlot <- do.call("plot_grid",c(NoLegendPlots,list(ncol=2,
                                                     align = "hv",
                                                     labels =c("a","b","c","d","e","f"), 
                                                     label_fontface="plain",
                                                     label_size = 9)))

FullPlotWithLegend <- plot_grid(FullPlot, legend_b,ncol=1,rel_heights = c(1,0.2))
FullPlotWithLegend


#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"SurfaceCharacteristicTemperaturePlot.pdf"),FullPlotWithLegend,width = 178,units="mm",height = 205, dpi=300, device = "pdf")

ggsave(file.path(outputDirectory,"SurfaceCharacteristicTemperaturePlot.tif"),FullPlot,width = 178, height = 205,units="mm", dpi=300, device = "tiff")
