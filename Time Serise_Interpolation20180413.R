

#Time Series plotting projet
#Different interpolation methods: linear interpolation, polynomial, Akima interpolation

#laod required package
install.packages("rasterVis")
install.packages("ggplot2")
library(rasterVis)
library(raster)
library(sp)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(rgdal)
library(spatial)
library(akima)
library(ggplot2)

#create list of NDVI path 
all_ndvi<- list.files("C:/Users/s364033/Desktop/01 Project/output", full.names = TRUE, pattern = ".tif$")

#create a time seriese stack- not ordered 
NDVI_stack<- stack(all_ndvi)

#plot NNDVI stack 
plot(NDVI_stack, zlim=c(.15,1, nc=1))

#using level plot 
levelplot(NDVI_stack, main="data NDVI")


#load color brewer to give yello and green value for the NDVI
cols<- colorRampPalette(brewer.pal(9,"YlGn"))

#now create level plot with color brewer
levelplot(NDVI_stack, main= "Improved color NDVI", col.regions= cols)

#pattern matching replacement 
rasterNames<- gsub("NDVI_land_m", "Day", names(NDVI_stack))
Doy<- gsub("Day", "", rasterNames)
Doy<- as.numeric(Doy)
rank(Doy)

#order the NDVI time steps according there acquisition data 
NDVI_stack_ordered<- subset(NDVI_stack, order(rank(Doy)))

#renaming the landsat images 
rasterNames_ordered<- gsub("NDVI_land_m","Day", names(NDVI_stack_ordered))


#ploting the ordered time sereis
levelplot(NDVI_stack_ordered, 
          layout=c(3,3), #create 3*3 layout for the data 
        col.regions=cols, #add a color ramp
        main="sentinel data Julian days of 2015",
       names.attr=rasterNames_ordered,
       scales=list(draw=FALSE)) #remove axis labels and ticks



#linear interpolation
names(NDVI_stack_ordered) <- as.numeric(gsub("Day","", rasterNames_ordered))
val <- getValues(NDVI_stack_ordered)
time = as.numeric(gsub("X","", names(NDVI_stack_ordered)))
time.out <- seq(time[1], time[length(time)])#, by =1

#Interpolation of cloud covered area
val.int.fill <- apply(val, MARGIN = 1, FUN = function(x, t = time){
          if(length(which(is.na(x) == T)) <= 4){
          approx(t,x,n=length(x),method ="linear")$y
          } else{
          x
          }
})
r.int.fill <- setValues(NDVI_stack_ordered,t(val.int.fill))

#plot the result of linear interpolation 
levelplot(r.int.fill, 
          layout=c(3,3), #create 3*3 layout for the data 
          col.regions=cols, #add a color ramp
          main="sentinel data Julian days of 2015",
          names.attr=rasterNames_ordered,
          scales=list(draw=FALSE)) #remove axis labels and ticks

names(r.int.fill)<- (rasterNames)
plot(r.int.fill)

#Akima Interpolation

akima.int.fill <- apply(val, MARGIN = 1, FUN = function(x, t = time){
          if(length(which(is.na(x) == T)) <= 4){
          aspline(t,x,n=length(x),ties = mean, method ="original",degree=3)$y
          } else{
          x
          }
})
akima.int.fill <- setValues(NDVI_stack_ordered,t(akima.int.fill))

#plot the result of akima interpolation 
levelplot(akima.int.fill, 
          layout=c(3,3), #create 3*3 layout for the data 
          col.regions=cols, #add a color ramp
          main="sentinel data Julian days of 2015",
          names.attr=rasterNames_ordered,
          scales=list(draw=FALSE)) #remove axis labels and ticks

plot(akima.int.fill)
#Day of the year interplation
val.days<- getValues(r.int.fill)
timeday.out <- seq(time[1], time[length(time)], by =30)

val.int.days <- apply(val.days, MARGIN = 1, FUN = function(x, t = time, x.out = timeday.out){
         if(length(which(is.na(x) == T)) <= 4){
          approx(t,x,xout = x.out, method ="linear")$y
          } else{
          x
          }
          })
r.int.days <- setValues(NDVI_stack_ordered,t(val.int.days))

#choose the right name for result of interpolation
names(r.int.days)<- (timeday.out)
rasterNames_ordered_time<- gsub("X","Day",names(r.int.days))
names(r.int.days)<- (rasterNames_ordered_time)


#plot the result of akima interpolation 
levelplot(r.int.days, 
          layout=c(3,3), #create 3*3 layout for the data 
          col.regions=cols, #add a color ramp
          main="Interploted day of the year",
          names.attr=rasterNames_ordered_time,
          scales=list(draw=FALSE)) #remove axis labels and ticks

plot(r.int.days)






