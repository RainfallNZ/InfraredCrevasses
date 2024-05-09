#R Script to create publication-ready infrared image orthorectification example plot for paper.
#Show RGB ortho image above an example thermal ortho image. Use the same colour palette as for the Camera plot.
#Plot is to be formatted for Journal of Glaciology.
#Guidelines are here:https://www.cambridge.org/core/services/aop-file-manager/file/5b431f1d2c1c7a5063243b24/jglac-instructionsforauthors-11Apr2019.pdf
#Journal of Glaciology have an Overleaf template to assist with preparing a publication.
# https://www.overleaf.com/project/6601af499e691ff65feea2ec
#This plot is intended to be single column (86 mm) wide and about 100 mm high.

#The plot is prepared using the ggplot2 plotting library and related packages

#Check for and load required libraries and packages
list.of.packages <- c("imager","magick","terra","tidyr","dplyr","ggplot2","ggtext","tidyterra","gridExtra","grid","gtable","ggsn")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

#Set directory and file names
ProjectDirectory  <- "D:\\Projects\\UC\\Heather Purdie\\Purdie-TasmanSaddle-Crevasses"
UCDirectory       <- file.path(ProjectDirectory,"CopiesFromUC") 
DataDirectory     <- file.path(ProjectDirectory,"Crevasse Temperature Variability\\Data")
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
RGBOrthoImageFile <- file.path(DataDirectory,"GIS","TG_20202_orthoNZTM_1mres.tif")
ExampleIROrthoImageFile  <- file.path(DataDirectory,"GIS","OrthoImages","OrthoImage_49_20200225_03_20_clipped.tif")

#Load the two images, resizing the camera view photo to 86 mm wide at 300 dpi

IRExample  <- terra::rast(ExampleIROrthoImageFile)
RGBImage   <- terra::rast(RGBOrthoImageFile) %>% terra::project(IRExample) %>%  terra::resample(IRExample) %>% terra::mask(IRExample)
WeatherStaionSitesFile <- file.path(DataDirectory,"GIS","GEO-XH.shp")
CameraLocationFile <- file.path(DataDirectory,"GIS","IRCamera.shp")

Camera <- terra::vect(CameraLocationFile)
WeatherStations <- terra::vect(WeatherStaionSitesFile) %>% tidyterra::filter(Comment %in% c("aws lower","aws top"))

#Create a plot of the IR image
IRPlot <- ggplot() +
  geom_spatraster(data=IRExample) +
  geom_spatvector(data = Camera, shape = 19,size=4) +
#  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
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
                       #values=c(0,0.5,1),
                       labels = c("warmer","colder"),
                       breaks = c(-6,-18),
                       na.value=NA,
                       limits=c(-18,-6),
                       name="Temperature",
                       guide=guide_colorbar(title.position = "left",ticks = FALSE,title.vjust = 0.8),
                       oob=scales::squish)+
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)

IRPlot

OrthoImagePlot <-  ggplot() +
  terrainr::geom_spatial_rgb(data=RGBImage,
                             mapping = aes(x = x,
                                           y = y,
                                           r = red,
                                           g = green,
                                           b = blue)) +
  coord_sf(crs = 2193) +
  geom_spatvector(data = Camera, shape = 19,size=4) +
#  geom_spatvector(data = WeatherStations, shape = 17, size=4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
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
  ggspatial::annotation_scale(style="ticks",text_cex=0.8, tick_height=0)


  
OrthoImagePlot

aligned <- cowplot::align_plots(IRPlot,OrthoImagePlot, align = "vh", axis="blrt")

FullPlot = cowplot::plot_grid(aligned[[1]], aligned[[2]], ncol=1)

#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"OrthoExample.pdf"),FullPlot,width = 86,units="mm",height = 125, dpi=300, device = "pdf")

ggsave(file.path(outputDirectory,"OrthoExample.tif"),FullPlot,width = 86, height = 125,units="mm", dpi=300, device = "tiff")
