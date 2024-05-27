#R Script to create publication-ready violin plot for paper of surface temperature 
#for dust and non-dust areas

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
OrthoFileNames    <- list.files(OrthoFileDirectory,full.names = TRUE)
outputDirectory   <- file.path(ProjectDirectory,"Reports","TablesAndFigures")
CrevasseClassFile <- file.path(GISDirectory,"CrevasseClassified.tif")
AreaOfInterestFile<- file.path(GISDirectory,"IRCameraAOI.shp")
ViewshedFile      <- file.path(GISDirectory,"Viewshed.tif")
DustClassFile     <- file.path(GISDirectory,"DustPolygon.shp") 

#Load the data
AreaOfInterest <- terra::vect(AreaOfInterestFile)

#Load the temperature-corrected-ortho images
OrthoData <- readRDS(file.path(OrthoFileDirectory,"TemperatureCorrected","TemperatureCorrectedOrthoImages.rds")) %>%
  terra::project(terra::crs(AreaOfInterest)) %>%
  terra::mask(AreaOfInterest)

OrthoDateTimes <- names(OrthoData) %>% as.POSIXct(tz="NZ",format="%Y-%m-%d %H:%M")

Viewshed   <- terra::rast(ViewshedFile)%>% 
  terra::project(OrthoData[[1]],method="near") %>%  
  terra::resample(OrthoData[[1]],method="near") %>% 
  terra::mask(OrthoData[[1]])

#load the dust classification polygon
DustClass <- terra::vect(DustClassFile) %>%
  rasterize(y= OrthoData[[1]],background = 0)

#load the crevasse classification data resampled to match the ortho image resolution
CrevasseMask <- terra::rast(CrevasseClassFile) %>% 
  terra::resample(OrthoData[[1]], method="med") %>%
  #terra::ifel(. < 1, NA, .) %>%
  terra::mask(AreaOfInterest) %>%
  terra::mask(Viewshed,maskvalues=c(NA,0))

BufferedCrevasses <- terra::rast(CrevasseClassFile) %>% 
  terra::resample(OrthoData[[1]], method="max") %>%
  terra::ifel(. < 1, NA, .) %>%
  terra::buffer(3)%>%
  terra::mask(AreaOfInterest) 
 
#Combine the non-crevassed mask with the crevasse mask to give a three class raster
#0 = near crevasses, 1 = crevasses, 2 = not crevassed
CrevNoCrev <- terra::ifel(BufferedCrevasses == 1,yes=CrevasseMask,no= 2)
names(CrevNoCrev) <- "Class"

#Add the dust class as well.
#0 = near crevasses no dust, 1 = near crevasses dust, 2 = crevasses no dust,3 = crevasses dust, 
#4 = not crevassed no dust, 5=not crevassed dust
CrevNoCrevDust <- CrevNoCrev*2 + DustClass

#Get the temperatures within the crevassed areas from the 3 am and 3 pm orthoImages
ImageIndices <- c(46,118) #46 = 3am, 118 = 3 pm
ImageValues <- values(c(OrthoData[[ImageIndices]],Class=CrevNoCrevDust),dataframe=TRUE) %>%
  terra::na.omit() %>%
  mutate(Class=factor(Class,levels = c(0,1,2,3,4,5), labels = c("DMZNoDust","DMZDust","CrevassedNoDust","CrevassedDust","Not-crevassedNoDust","Not-crevassedDust")))

#Filter out the "DMZ" class and convert to long for use as plot data
PlotData <- ImageValues %>%
  filter(Class %in% c("Not-crevassedNoDust","Not-crevassedDust")) %>%
  pivot_longer(cols = !Class,names_to = "DateTime", values_to = "Temperature") #%>%
  #mutate(DateTime = as.POSIXct(DateTime))


#Calculate sample size
sample_size = PlotData %>% group_by(DateTime,Class) %>% summarise(num=n())

#See https://r-graph-gallery.com/violin_and_boxplot_ggplot2.html
#to add box and whisker

#also see https://ggplot2.tidyverse.org/reference/geom_violin.html to include a
#time aesthetic so that two violins per time can be plotted.

ViolinBoxPlot <- ggplot(data=PlotData, aes(x=DateTime,y=Temperature, fill=Class)) +
  geom_violin(width = 1.5) +
  geom_boxplot(width=0.2,color="black", alpha=0.2,position=position_dodge(width=1.5),show.legend = FALSE)+
  scale_fill_manual(labels=c("No Dust","Dust"),
                      values = c("white","tan"))+
  ylab("Temperature (<sup>o</sup>C)")+
  xlab("Observation time") +
  scale_x_discrete(labels=c("03:00\n25 Feb","15:00\n25 Feb"))+
  #scale_x_datetime(date_labels = '%H:%M\n%d %b') +

  
theme_classic() +
  theme(text=element_text(size=9),
        panel.grid.major = element_line(),
        legend.text = element_text(size=9),
        legend.key.height = unit(1,"cm"),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.8,0.3),
        axis.title = element_markdown(),
        axis.text=element_text(size=9),
        panel.border = element_blank())

ViolinBoxPlot

ThreeAM.t.test <- t.test(x=PlotData %>% filter(Class == "Not-crevassedDust",DateTime == "2020-02-25 03:00:00") %>% select(Temperature),
                  y=PlotData %>% filter(Class == "Not-crevassedNoDust",DateTime == "2020-02-25 03:00:00")%>% select(Temperature),
                  alternative="two.sided")

ThreePM.t.test <- t.test(x=PlotData %>% filter(Class == "Not-crevassedDust",DateTime == "2020-02-25 15:00:00") %>% select(Temperature),
                  y=PlotData %>% filter(Class == "Not-crevassedNoDust",DateTime == "2020-02-25 15:00:00")%>% select(Temperature),
                  alternative="two.sided")#Save as pdf for Overleaf, and tif for Word
ggsave(file.path(outputDirectory,"DustNoDust.pdf"),ViolinBoxPlot,width = 86,units="mm",height = 86, dpi=300, device = "pdf")

ggsave(file.path(outputDirectory,"DustNoDust.tif"),ViolinBoxPlot,width = 86, height = 86,units="mm", dpi=300, device = "tiff")
