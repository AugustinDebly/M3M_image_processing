library(terra)

path_dossier_NDVI = "step_8_NDVI"
path_out_stck     = paste(path_dossier_NDVI,"stck_NDVI.TIF",sep="/")
path_out_mean     = paste(path_dossier_NDVI,"mean_NDVI.TIF",sep="/")

files_NDVI = list.files(path_dossier_NDVI,pattern="DJI",full.names = T)

xmin = ext(rast(files_NDVI[1]))[1][[1]]
xmax = ext(rast(files_NDVI[1]))[2][[1]]
ymin = ext(rast(files_NDVI[1]))[3][[1]]
ymax = ext(rast(files_NDVI[1]))[4][[1]]
crs_NDVI = crs(rast(files_NDVI[1]))
#res_NDVI = res(rast(files_NDVI[1]))
res_NDVI = c(0.019,0.019)

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

list_NDVI <- lapply(files_NDVI, function(file) {
  print((match(file, files_NDVI) * 100) / length(files_NDVI))
  resample(rast(file), NDVI_merge, method = "average")
})

stck_NDVI = rast(list_NDVI)
mean_NDVI = mean(stck_NDVI,na.rm=T)

writeRaster(stck_NDVI,path_out_stck,overwrite=T)
writeRaster(mean_NDVI,path_out_mean,overwrite=T)