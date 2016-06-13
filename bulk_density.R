#load pachages for reading different files
library(readxl)
library(foreign)

#load packages for manipulating data
library(dplyr)
library(tidyr)

library(ggplot2)

#read fiel via browser 
##soil <- read.csv(choose.files())
soil <- read.csv("https://raw.githubusercontent.com/giorgichi/CSCAP_DWM/master/Soil_Properties.csv")
plot <- read.csv("https://raw.githubusercontent.com/giorgichi/CSCAP_DWM/master/Plots.csv")

#rename plot IDs
levels(soil$plotid) <- c(paste("S",rep(1:8, each = 3),sep = ""),"NE","NW","SE","SW",rep(c("WN","WS"),each = 3))

#remove DWM1 (No-Drainage plot) data
soil <- soil[soil$drainage!="DWM1",]
#remove rows with empty cells in SOIL1 (bulk density)
soil <- soil[soil$SOIL1!="",]


#drop unused levels after reducing the data
soil <- droplevels(soil)

# #replace DNC and N/A with NA in the whole sheet
# soil[soil == "DNC"] <- NA
# soil[soil == "N/A"] <- NA
# #creat vector of Soil Bulk Density data
# soil$SOIL1 <- as.numeric(as.character(soil$SOIL1))


#3 codes above can be replaced by this one 
soil$bd <- as.numeric(levels(soil$SOIL1))[soil$SOIL1]
soil$WR0.1 <- as.numeric(levels(soil$SOIL30))[soil$SOIL30]*100
soil$por <- (1-soil$bd/2.65)*100
soil$gw <- soil$por - soil$WR0.1


#analyzing soil bulk density
#overall mean soil dry bulk density
mean_bd <- mean(soil$bd, na.rm = TRUE)
mean_wr0.1 <- mean(soil$WR0.1, na.rm = TRUE)
mean_por <- mean(soil$por, na.rm = TRUE)
mean_gw <- mean(soil$gw, na.rm = TRUE)

#mean bulk density, porositym and gw by site
soil %>%
  group_by(site) %>%
  summarise(bd = mean(bd, na.rm = TRUE), por = mean(por, na.rm = TRUE), gw = mean(gw, na.rm = TRUE)) -> by_site

#mean bulk density, porositym and gw by site and depth
soil %>%
  group_by(site,depth) %>%
  summarise(bd = mean(bd, na.rm = TRUE), por = mean(por, na.rm = TRUE), 
            wr0.1 = mean(WR0.1, na.rm = TRUE), gw = mean(gw, na.rm = TRUE)) -> by_site_depth

##spreading the output for presenting in table   
##bd <- as.data.frame(spread(by_site_depth,key=site,value=bd))
reshape(as.data.frame(by_site_depth), idvar = 'depth', timevar = 'site', direction = 'wide')

#calculate porosity
site_depth <- as.data.frame(by_site_depth)
site_depth_long <- gather(site_depth, key = variables, value = values, bd:gw)
head(site_depth_long)
head(site_depth)

qplot(depth, values, data= site_depth_long, size = I(4), color = site, geom = "point", facets = ~ variables)
qplot(depth, bd, data= site_depth, size = I(4), geom = "point", facets = ~ site)
qplot(depth, por, data= site_depth, size = I(4), geom = "point", facets = ~ site)
qplot(depth, gw, data= site_depth, size = I(4), geom = "point", facets = ~ site)

#mean bulk density by plot and depth
soil %>%
  group_by(site,plotid,depth) %>%
  summarise(bd = mean(bd, na.rm = TRUE), por = mean(por, na.rm = TRUE), 
            wr0.1 = mean(WR0.1, na.rm = TRUE), gw = mean(gw, na.rm = TRUE)) -> by_plot_depth
##spreading the output for presenting in table   
##spread(mean_bd_plot_depth,key=plotid,value=bd)
plot_depth <- as.data.frame(by_plot_depth)

qplot(depth, bd, data = by_plot_depth, size = I(4), color = site, facets = ~ plotid)+ 
  ggtitle("Soil Bulk Density by Depth") + 
  theme(plot.title = element_text(face = "plain", size = 20)) +
  theme(axis.text.x = element_text(angle = 90))

qplot(depth, por, data = by_plot_depth, size = I(4), color = site, facets = ~ plotid)+ 
  ggtitle("Soil Porosity by Depth") + 
  theme(plot.title = element_text(face = "plain", size = 20)) +
  theme(axis.text.x = element_text(angle = 90))

qplot(depth, gw, data = by_plot_depth, size = I(4), color = site, facets = ~ plotid)+ 
  ggtitle("Gravitational Water by Depth") + 
  theme(plot.title = element_text(face = "plain", size = 20)) +
  theme(axis.text.x = element_text(angle = 90))



#mean bulk density by DWM and depth
soil %>%
  group_by(site, drainage, depth) %>%
  summarise(bd = mean(bd, na.rm = TRUE), por = mean(por, na.rm = TRUE), 
            wr0.1 = mean(WR0.1, na.rm = TRUE), gw = mean(gw, na.rm = TRUE))-> by_dwm_depth
#spreading the output for presenting in table   
#spread(by_dwm_depth,key=drainage,value=bd)

qplot(depth, bd, data = by_dwm_depth, size = I(4), facets = ~ drainage)
qplot(depth, bd, data = by_dwm_depth, size = I(4), shape = site, color = site, facets =  ~ drainage)


##write.table(site_depth, "C:/Users/gio/Documents/CSCAP/Analysis/DWM paper/bd.txt", sep = ",", qmethod = "double", row.names = FALSE)
write.table(format(site_depth, digits = 3), choose.files(), sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)