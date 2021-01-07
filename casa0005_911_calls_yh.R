#install.packages("spgwr")
library(sf)
library(tmap)
library(sp)
library(spdep)
library(spgwr)
library(ggplot2) 
library(dplyr)
library(ggsn)

#Load in the following packages


oregon_data <- "D://Abroad//term1//casa0005//assignment//data(1)//data//911Calls.shp" # loading the file from directory
file <- st_read(oregon_data)

current_style <- tmap_style("col_blind") # assign color theme+
#-------------------------------------------------------------------
# OLS Regression Model....

fit_1 <- lm(Calls ~ Pop+Jobs+LowEduc+Dst2UrbCen, data=file)
summary(fit_1)


file$res_fit1 <- residuals(fit_1)
file$fitted_fit1 <- fitted(fit_1)

file$sd_breaks <- scale(file$res_fit1)[,1] # because scale is made for matrices, we just need to get the first column using [,1]
# this is equal to (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1)) / sd(ncovr_sf$res_fit1)
summary(file$sd_breaks)

tm_shape(file,unit = "mi") + 
  tm_fill("sd_breaks", title = "Residuals", style = "cont", midpoint = NA,
                    palette = "-RdBu") +
  tm_borders(alpha = 0.3) +
  tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
            legend.position = c("left", "bottom"), legend.title.size = 0.8)+
   tm_compass(type = "8star", position = c("right", "top")) +
   tm_scale_bar(breaks = c(0, 1, 2), text.size = 1)



#-----------------------------------------------------------

#We coerce the sf object into a new sp object
ncovr_sp <- as(file, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w <- poly2nb(ncovr_sp, row.names=ncovr_sp$FIPSNO)
summary(w)
wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')

lm.morantest(fit_1,rwm, alternative="two.sided")


#---------------------------------------------------------------------

#Geographicaly Weighted Regression


#In order to calculate an optimal bandwidth in R, use the command gwr.sel(). The default method is cross-validation


gwr.b3<-gwr.sel(Calls ~ Pop+Jobs+LowEduc+Dst2UrbCen, data=ncovr_sp, adapt = TRUE)

gwr.b3

gwr.fit3<-gwr(Calls ~ Pop+Jobs+LowEduc+Dst2UrbCen, data=ncovr_sp,
              adapt=gwr.b3, se.fit=T, hatmatrix=T)

gwr.fit3



# Create an object with the value of Quasi-global R2
globalR2 <- (1 - (gwr.fit3$results$rss/gwr.fit3$gTSS))


# get spatial spatialpolygondataframe from regression results + convert it into sf object. The spatial object brings the regressions results within it's data component
sp <- gwr.fit3$SDF
sf <- st_as_sf(sp)  


# map local R2
ggplot() + geom_sf(data = sf, aes(fill=localR2)) +
  geom_sf() +
   north(sf, location = "topright", symbol = 1) +
  scalebar(sf, dist = 4, dist_unit = "mi",location = "bottomright",
           transform = FALSE, model = "WGS84")+
  coord_sf() +
  #theme_map() +
  ggtitle(paste("Local R2")) +
  labs(subtitle = paste("Global R2:", round(globalR2, 2) ) ) 

# map residuals gwr.e
ggplot() + geom_sf(data = sf, aes(fill=gwr.e)) +
  geom_sf() +
   north(sf, location = "topright", symbol = 1) +
  scalebar(sf, dist = 4, dist_unit = "mi",location = "bottomright",
           transform = FALSE, model = "WGS84")+
   coord_sf() +
  #theme_map() +
  ggtitle(paste("Residuals"))

  





















