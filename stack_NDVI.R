##Packages----------------------------------------------------------------------

require(terra)

##Option(s)---------------------------------------------------------------------

resolution_auto   = FALSE
res_x_if_not_auto = 0.019 #[m]
res_y_if_not_auto = 0.019 #[m]

##Path--------------------------------------------------------------------------

path_dossier_NDVI = "step_8_NDVI"
path_out_stck     = paste(path_dossier_NDVI,"stck_NDVI.TIF",sep="/")
path_out_mean     = paste(path_dossier_NDVI,"mean_NDVI.TIF",sep="/")

files_NDVI        = list.files(path_dossier_NDVI,pattern="DJI",full.names = T)

##GIS parameters----------------------------------------------------------------

xmin     = ext(rast(files_NDVI[1]))[1][[1]]
xmax     = ext(rast(files_NDVI[1]))[2][[1]]
ymin     = ext(rast(files_NDVI[1]))[3][[1]]
ymax     = ext(rast(files_NDVI[1]))[4][[1]]

crs_NDVI = crs(rast(files_NDVI[1]))

res_NDVI = c(res_x_if_not_auto,res_y_if_not_auto)
if(resolution_auto){
  res_NDVI = res(rast(files_NDVI[1]))
}

for(i in seq(2,length(files_NDVI))){
  xmin = min(xmin,ext(rast(files_NDVI[i]))[1][[1]])
  xmax = max(xmax,ext(rast(files_NDVI[i]))[2][[1]])
  ymin = min(ymin,ext(rast(files_NDVI[i]))[3][[1]])
  ymax = max(ymax,ext(rast(files_NDVI[i]))[4][[1]])
}

ext_NDVI = c(xmin,xmax,ymin,ymax)

NDVI_merge = rast()

ext(NDVI_merge) = ext_NDVI
crs(NDVI_merge) = crs_NDVI
res(NDVI_merge) = res_NDVI

#Stacking-----------------------------------------------------------------------

list_NDVI <- lapply(files_NDVI, function(file) {
  #print((match(file, files_NDVI) * 100) / length(files_NDVI))
  resample(rast(file), NDVI_merge, method = "average")
})

stck_NDVI = rast(list_NDVI)
mean_NDVI = mean(stck_NDVI,na.rm=T)

#TIF writing--------------------------------------------------------------------

writeRaster(stck_NDVI,path_out_stck,overwrite=T)
writeRaster(mean_NDVI,path_out_mean,overwrite=T)