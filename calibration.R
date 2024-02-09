##Packages--------------------------------------------------------------------------------------------------------
library(exiftoolr)
library(terra)
system("python -m pip install numpy")
system("python -m pip install opencv-python")

##Path------------------------------------------------------------------------------------------------------------
#Path of the raw images for each bands
path_G = "example_images/DJI_20240202122522_0001_MS_G.TIF"
path_NIR = "example_images/DJI_20240202122522_0001_MS_NIR.TIF"
path_R = "example_images/DJI_20240202122522_0001_MS_R.TIF"
path_RE = "example_images/DJI_20240202122522_0001_MS_RE.TIF"

#Path of directories where images will be stored after each corrections
path_dir_out_vignetting = "step_1_vignetting"
path_dir_out_distorsion = "step_2_distorsion"
path_dir_out_alignment  = "step_3_alignment"
path_dir_out_ECC        = "step_4_ECC_alignment"
path_dir_out_homogeni   = "step_5_homogenization"

#Path of the python scripts used for opencv functions
path_script_python_distorsion = "distorsion_calibration.py"
path_script_python_alignment = "alignment_calibration.py"
path_script_python_ECC_alignment = "ECC_alignment_calibration.py"

#Name of the images
name_image_G = strsplit(path_G,"/")[[1]][length(strsplit(path_G,"/")[[1]])]
name_image_NIR = strsplit(path_NIR,"/")[[1]][length(strsplit(path_NIR,"/")[[1]])]
name_image_R = strsplit(path_R,"/")[[1]][length(strsplit(path_R,"/")[[1]])]
name_image_RE = strsplit(path_RE,"/")[[1]][length(strsplit(path_RE,"/")[[1]])]

#Path of images stored after vignetting correction
path_vignetting_G = paste(path_dir_out_vignetting,paste(substr(name_image_G,1,nchar(name_image_G)-4),"_vignetting_corrected.TIF",sep = ""),sep = "/")
path_vignetting_NIR = paste(path_dir_out_vignetting,paste(substr(name_image_NIR,1,nchar(name_image_NIR)-4),"_vignetting_corrected.TIF",sep = ""),sep = "/")
path_vignetting_R = paste(path_dir_out_vignetting,paste(substr(name_image_R,1,nchar(name_image_R)-4),"_vignetting_corrected.TIF",sep = ""),sep = "/")
path_vignetting_RE = paste(path_dir_out_vignetting,paste(substr(name_image_RE,1,nchar(name_image_RE)-4),"_vignetting_corrected.TIF",sep = ""),sep = "/")

#Path of images stored after distorsion correction
path_distorsion_G = paste(path_dir_out_distorsion,paste(substr(name_image_G,1,nchar(name_image_G)-4),"_distorsion_corrected.TIF",sep = ""),sep = "/")
path_distorsion_NIR = paste(path_dir_out_distorsion,paste(substr(name_image_NIR,1,nchar(name_image_NIR)-4),"_distorsion_corrected.TIF",sep = ""),sep = "/")
path_distorsion_R = paste(path_dir_out_distorsion,paste(substr(name_image_R,1,nchar(name_image_R)-4),"_distorsion_corrected.TIF",sep = ""),sep = "/")
path_distorsion_RE = paste(path_dir_out_distorsion,paste(substr(name_image_RE,1,nchar(name_image_RE)-4),"_distorsion_corrected.TIF",sep = ""),sep = "/")

#Path of images stored after alignment correction
path_alignment_G = paste(path_dir_out_alignment,paste(substr(name_image_G,1,nchar(name_image_G)-4),"_alignment_corrected.TIF",sep = ""),sep = "/")
path_alignment_NIR = paste(path_dir_out_alignment,paste(substr(name_image_NIR,1,nchar(name_image_NIR)-4),"_alignment_corrected.TIF",sep = ""),sep = "/")
path_alignment_R = paste(path_dir_out_alignment,paste(substr(name_image_R,1,nchar(name_image_R)-4),"_alignment_corrected.TIF",sep = ""),sep = "/")
path_alignment_RE = paste(path_dir_out_alignment,paste(substr(name_image_RE,1,nchar(name_image_RE)-4),"_alignment_corrected.TIF",sep = ""),sep = "/")

#Path of images stored after ECC alignment correction
path_ECC_G = paste(path_dir_out_ECC,paste(substr(name_image_G,1,nchar(name_image_G)-4),"_ECC_corrected.TIF",sep = ""),sep = "/")
path_ECC_NIR = paste(path_dir_out_ECC,paste(substr(name_image_NIR,1,nchar(name_image_NIR)-4),"_ECC_corrected.TIF",sep = ""),sep = "/")
path_ECC_R = paste(path_dir_out_ECC,paste(substr(name_image_R,1,nchar(name_image_R)-4),"_ECC_corrected.TIF",sep = ""),sep = "/")
path_ECC_RE = paste(path_dir_out_ECC,paste(substr(name_image_RE,1,nchar(name_image_RE)-4),"_ECC_corrected.TIF",sep = ""),sep = "/")

#Path of images stored after homogenization
path_homogenized_G = paste(path_dir_out_homogeni,paste(substr(name_image_G,1,nchar(name_image_G)-4),"_homogenized.TIF",sep = ""),sep = "/")
path_homogenized_NIR = paste(path_dir_out_homogeni,paste(substr(name_image_NIR,1,nchar(name_image_NIR)-4),"_homogenized.TIF",sep = ""),sep = "/")
path_homogenized_R = paste(path_dir_out_homogeni,paste(substr(name_image_R,1,nchar(name_image_R)-4),"_homogenized.TIF",sep = ""),sep = "/")
path_homogenized_RE = paste(path_dir_out_homogeni,paste(substr(name_image_RE,1,nchar(name_image_RE)-4),"_homogenized.TIF",sep = ""),sep = "/")

##Importing exif--------------------------------------------------------------------------------------------------
exif_G = exif_read(path_G)
exif_NIR = exif_read(path_NIR)
exif_R = exif_read(path_R)
exif_RE = exif_read(path_RE)

##Getting parameters from exif------------------------------------------------------------------------------------
height_G = exif_G$ImageHeight
width_G = exif_G$ImageWidth
height_NIR = exif_NIR$ImageHeight
width_NIR = exif_NIR$ImageWidth
height_R = exif_R$ImageHeight
width_R = exif_R$ImageWidth
height_RE = exif_RE$ImageHeight
width_RE = exif_RE$ImageWidth

Cx_G = exif_G$VignettingCenter[[1]][1]
Cy_G = exif_G$VignettingCenter[[1]][2]
Cx_NIR = exif_NIR$VignettingCenter[[1]][1]
Cy_NIR = exif_NIR$VignettingCenter[[1]][2]
Cx_R = exif_R$VignettingCenter[[1]][1]
Cy_R = exif_R$VignettingCenter[[1]][2]
Cx_RE = exif_RE$VignettingCenter[[1]][1]
Cy_RE = exif_RE$VignettingCenter[[1]][2]

c1_G = exif_G$VignettingPolynomial[[1]][1]
c2_G = exif_G$VignettingPolynomial[[1]][2]
c3_G = exif_G$VignettingPolynomial[[1]][3]
c4_G = exif_G$VignettingPolynomial[[1]][4]
c5_G = exif_G$VignettingPolynomial[[1]][5]
c6_G = exif_G$VignettingPolynomial[[1]][6]

c1_NIR = exif_NIR$VignettingPolynomial[[1]][1]
c2_NIR = exif_NIR$VignettingPolynomial[[1]][2]
c3_NIR = exif_NIR$VignettingPolynomial[[1]][3]
c4_NIR = exif_NIR$VignettingPolynomial[[1]][4]
c5_NIR = exif_NIR$VignettingPolynomial[[1]][5]
c6_NIR = exif_NIR$VignettingPolynomial[[1]][6]

c1_R = exif_R$VignettingPolynomial[[1]][1]
c2_R = exif_R$VignettingPolynomial[[1]][2]
c3_R = exif_R$VignettingPolynomial[[1]][3]
c4_R = exif_R$VignettingPolynomial[[1]][4]
c5_R = exif_R$VignettingPolynomial[[1]][5]
c6_R = exif_R$VignettingPolynomial[[1]][6]

c1_RE = exif_RE$VignettingPolynomial[[1]][1]
c2_RE = exif_RE$VignettingPolynomial[[1]][2]
c3_RE = exif_RE$VignettingPolynomial[[1]][3]
c4_RE = exif_RE$VignettingPolynomial[[1]][4]
c5_RE = exif_RE$VignettingPolynomial[[1]][5]
c6_RE = exif_RE$VignettingPolynomial[[1]][6]

distorsion_info_G = as.numeric(strsplit(exif_G$DewarpData,"[;,]")[[1]])
distorsion_info_NIR = as.numeric(strsplit(exif_NIR$DewarpData,"[;,]")[[1]])
distorsion_info_R = as.numeric(strsplit(exif_R$DewarpData,"[;,]")[[1]])
distorsion_info_RE = as.numeric(strsplit(exif_RE$DewarpData,"[;,]")[[1]])
fx_G = distorsion_info_G[2]
fy_G = distorsion_info_G[3]
cx_G = distorsion_info_G[4]
cy_G = distorsion_info_G[5]
k1_G = distorsion_info_G[6]
k2_G = distorsion_info_G[7]
p1_G = distorsion_info_G[8]
p2_G = distorsion_info_G[9]
k3_G = distorsion_info_G[10]
fx_NIR = distorsion_info_NIR[2]
fy_NIR = distorsion_info_NIR[3]
cx_NIR = distorsion_info_NIR[4]
cy_NIR = distorsion_info_NIR[5]
k1_NIR = distorsion_info_NIR[6]
k2_NIR = distorsion_info_NIR[7]
p1_NIR = distorsion_info_NIR[8]
p2_NIR = distorsion_info_NIR[9]
k3_NIR = distorsion_info_NIR[10]
fx_R = distorsion_info_R[2]
fy_R = distorsion_info_R[3]
cx_R = distorsion_info_R[4]
cy_R = distorsion_info_R[5]
k1_R = distorsion_info_R[6]
k2_R = distorsion_info_R[7]
p1_R = distorsion_info_R[8]
p2_R = distorsion_info_R[9]
k3_R = distorsion_info_R[10]
fx_RE = distorsion_info_RE[2]
fy_RE = distorsion_info_RE[3]
cx_RE = distorsion_info_RE[4]
cy_RE = distorsion_info_RE[5]
k1_RE = distorsion_info_RE[6]
k2_RE = distorsion_info_RE[7]
p1_RE = distorsion_info_RE[8]
p2_RE = distorsion_info_RE[9]
k3_RE = distorsion_info_RE[10]

alignment_info_G = as.numeric(strsplit(exif_G$CalibratedHMatrix,"[;,]")[[1]])
alignment_info_NIR = as.numeric(strsplit(exif_NIR$CalibratedHMatrix,"[;,]")[[1]])
alignment_info_R = as.numeric(strsplit(exif_R$CalibratedHMatrix,"[;,]")[[1]])
alignment_info_RE = as.numeric(strsplit(exif_RE$CalibratedHMatrix,"[;,]")[[1]])
M11_G = alignment_info_G[1]
M12_G = alignment_info_G[2]
M13_G = alignment_info_G[3]
M21_G = alignment_info_G[4]
M22_G = alignment_info_G[5]
M23_G = alignment_info_G[6]
M31_G = alignment_info_G[7]
M32_G = alignment_info_G[8]
M33_G = alignment_info_G[9]
M11_NIR = alignment_info_NIR[1]
M12_NIR = alignment_info_NIR[2]
M13_NIR = alignment_info_NIR[3]
M21_NIR = alignment_info_NIR[4]
M22_NIR = alignment_info_NIR[5]
M23_NIR = alignment_info_NIR[6]
M31_NIR = alignment_info_NIR[7]
M32_NIR = alignment_info_NIR[8]
M33_NIR = alignment_info_NIR[9]
M11_R = alignment_info_R[1]
M12_R = alignment_info_R[2]
M13_R = alignment_info_R[3]
M21_R = alignment_info_R[4]
M22_R = alignment_info_R[5]
M23_R = alignment_info_R[6]
M31_R = alignment_info_R[7]
M32_R = alignment_info_R[8]
M33_R = alignment_info_R[9]
M11_RE = alignment_info_RE[1]
M12_RE = alignment_info_RE[2]
M13_RE = alignment_info_RE[3]
M21_RE = alignment_info_RE[4]
M22_RE = alignment_info_RE[5]
M23_RE = alignment_info_RE[6]
M31_RE = alignment_info_RE[7]
M32_RE = alignment_info_RE[8]
M33_RE = alignment_info_RE[9]

BlackLevel_G = exif_G$BlackLevel
BlackLevel_NIR = exif_NIR$BlackLevel
BlackLevel_R = exif_R$BlackLevel
BlackLevel_RE = exif_RE$BlackLevel

BPS_G = exif_G$BitsPerSample
BPS_NIR = exif_NIR$BitsPerSample
BPS_R = exif_R$BitsPerSample
BPS_RE = exif_RE$BitsPerSample

exposure_time_G = exif_G$ExposureTime
exposure_time_NIR = exif_NIR$ExposureTime
exposure_time_R = exif_R$ExposureTime
exposure_time_RE = exif_RE$ExposureTime

gain_G = exif_G$SensorGain
gain_NIR = exif_NIR$SensorGain
gain_R = exif_R$SensorGain
gain_RE = exif_RE$SensorGain

radiance_down_G = exif_G$Irradiance
radiance_down_NIR = exif_NIR$Irradiance
radiance_down_R = exif_R$Irradiance
radiance_down_RE = exif_RE$Irradiance

sensor_gain_adjustment_G = exif_G$SensorGainAdjustment
sensor_gain_adjustment_NIR = exif_NIR$SensorGainAdjustment
sensor_gain_adjustment_R = exif_R$SensorGainAdjustment
sensor_gain_adjustment_RE = exif_RE$SensorGainAdjustment

##Importing images------------------------------------------------------------------------------------------------
raw_rast_G = rast(path_G)
raw_rast_NIR = rast(path_NIR)
raw_rast_R = rast(path_R)
raw_rast_RE = rast(path_RE)

##Functions-------------------------------------------------------------------------------------------------------
#Vignetting
polynomial_correction <- function(x,y,Cx,Cy,c1,c2,c3,c4,c5,c6){
  r = sqrt((x-Cx)^2+(y-Cy)^2)
  return(1+c1*(r^1)+c2*(r^2)+c3*(r^3)+c4*(r^4)+c5*(r^5)+c6*(r^6))
}
polynomial_correction_G <- function(x,y){
  return(polynomial_correction(x,y,Cx_G,Cy_G,c1_G,c2_G,c3_G,c4_G,c5_G,c6_G))
}
polynomial_correction_NIR <- function(x,y){
  return(polynomial_correction(x,y,Cx_NIR,Cy_NIR,c1_NIR,c2_NIR,c3_NIR,c4_NIR,c5_NIR,c6_NIR))
}
polynomial_correction_R <- function(x,y){
  return(polynomial_correction(x,y,Cx_R,Cy_R,c1_R,c2_R,c3_R,c4_R,c5_R,c6_R))
}
polynomial_correction_RE <- function(x,y){
  return(polynomial_correction(x,y,Cx_RE,Cy_RE,c1_RE,c2_RE,c3_RE,c4_RE,c5_RE,c6_RE))
}

#Distorsion
distorsion_correction_python <- function(path_script_python_distorsion,path_image_in,path_image_out,fx,fy,cx,cy,k1,k2,p1,p2,k3){
  system(paste("python",path_script_python_distorsion, path_image_in, path_image_out, fx, fy, cx, cy, k1, k2, p1, p2, k3))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
  return(image_out_matrix)
}

#Alignment
alignment_correction_python <- function(path_script_python_alignment,path_image_in,path_image_out,M11,M12,M13,M21,M22,M23,M31,M32,M33){
  system(paste("python",path_script_python_alignment, path_image_in, path_image_out, M11 ,M12 ,M13 ,M21 ,M22 ,M23 ,M31 ,M32 ,M33))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
}

#ECC
ECC_correction_python <- function(path_script_python_ECC_alignment,path_image_in_1,path_image_in_2,path_image_out){
  system(paste("python", path_script_python_ECC_alignment, path_image_in_1, path_image_in_2, path_image_out))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
}

##Vignetting correction-------------------------------------------------------------------------------------------
x_mat_G = matrix(rep(1:width_G,each=height_G),ncol=width_G)
y_mat_G = matrix(rep(1:height_G,times=width_G),ncol=width_G)
matrice_vignetting_G = polynomial_correction_G(x_mat_G,y_mat_G)
image_corrected_vignetting_G = raw_rast_G*rast(matrice_vignetting_G)
max_raw_G = max(values(raw_rast_G))
min_raw_G = min(values(raw_rast_G))
max_corrected_G = max(values(image_corrected_vignetting_G))
min_corrected_G = min(values(image_corrected_vignetting_G))
a_G = (max_raw_G-min_raw_G)/(max_corrected_G-min_corrected_G)
b_G = min_raw_G - a_G*min_corrected_G
image_corrected_vignetting_G = a_G * image_corrected_vignetting_G + b_G
values(image_corrected_vignetting_G) = as.integer(values(image_corrected_vignetting_G))
writeRaster(image_corrected_vignetting_G,path_vignetting_G,datatype = datatype(raw_rast_G),overwrite=T)

x_mat_NIR = matrix(rep(1:width_NIR,each=height_NIR),ncol=width_NIR)
y_mat_NIR = matrix(rep(1:height_NIR,times=width_NIR),ncol=width_NIR)
matrice_vignetting_NIR = polynomial_correction_NIR(x_mat_NIR,y_mat_NIR)
image_corrected_vignetting_NIR = raw_rast_NIR*rast(matrice_vignetting_NIR)
max_raw_NIR = max(values(raw_rast_NIR))
min_raw_NIR = min(values(raw_rast_NIR))
max_corrected_NIR = max(values(image_corrected_vignetting_NIR))
min_corrected_NIR = min(values(image_corrected_vignetting_NIR))
a_NIR = (max_raw_NIR-min_raw_NIR)/(max_corrected_NIR-min_corrected_NIR)
b_NIR = min_raw_NIR - a_NIR*min_corrected_NIR
image_corrected_vignetting_NIR = a_NIR * image_corrected_vignetting_NIR + b_NIR
values(image_corrected_vignetting_NIR) = as.integer(values(image_corrected_vignetting_NIR))
writeRaster(image_corrected_vignetting_NIR,path_vignetting_NIR,datatype = datatype(raw_rast_NIR),overwrite=T)

x_mat_R = matrix(rep(1:width_R,each=height_R),ncol=width_R)
y_mat_R = matrix(rep(1:height_R,times=width_R),ncol=width_R)
matrice_vignetting_R = polynomial_correction_R(x_mat_R,y_mat_R)
image_corrected_vignetting_R = raw_rast_R*rast(matrice_vignetting_R)
max_raw_R = max(values(raw_rast_R))
min_raw_R = min(values(raw_rast_R))
max_corrected_R = max(values(image_corrected_vignetting_R))
min_corrected_R = min(values(image_corrected_vignetting_R))
a_R = (max_raw_R-min_raw_R)/(max_corrected_R-min_corrected_R)
b_R = min_raw_R - a_R*min_corrected_R
image_corrected_vignetting_R = a_R * image_corrected_vignetting_R + b_R
values(image_corrected_vignetting_R) = as.integer(values(image_corrected_vignetting_R))
writeRaster(image_corrected_vignetting_R,path_vignetting_R,datatype = datatype(raw_rast_R),overwrite=T)

x_mat_RE = matrix(rep(1:width_RE,each=height_RE),ncol=width_RE)
y_mat_RE = matrix(rep(1:height_RE,times=width_RE),ncol=width_RE)
matrice_vignetting_RE = polynomial_correction_RE(x_mat_RE,y_mat_RE)
image_corrected_vignetting_RE = raw_rast_RE*rast(matrice_vignetting_RE)
max_raw_RE = max(values(raw_rast_RE))
min_raw_RE = min(values(raw_rast_RE))
max_corrected_RE = max(values(image_corrected_vignetting_RE))
min_corrected_RE = min(values(image_corrected_vignetting_RE))
a_RE = (max_raw_RE-min_raw_RE)/(max_corrected_RE-min_corrected_RE)
b_RE = min_raw_RE - a_RE*min_corrected_RE
image_corrected_vignetting_RE = a_RE * image_corrected_vignetting_RE + b_RE
values(image_corrected_vignetting_RE) = as.integer(values(image_corrected_vignetting_RE))
writeRaster(image_corrected_vignetting_RE,path_vignetting_RE,datatype = datatype(raw_rast_RE),overwrite=T)

##Distortion correction-------------------------------------------------------------------------------------------
image_corrected_distorsion_G = distorsion_correction_python(path_script_python_distorsion,path_vignetting_G,path_distorsion_G,fx_G,fy_G,Cx_G+cx_G,Cy_G+cy_G,k1_G,k2_G,p1_G,p2_G,k3_G)
image_corrected_distorsion_NIR = distorsion_correction_python(path_script_python_distorsion,path_vignetting_NIR,path_distorsion_NIR,fx_NIR,fy_NIR,Cx_NIR+cx_NIR,Cy_NIR+cy_NIR,k1_NIR,k2_NIR,p1_NIR,p2_NIR,k3_NIR)
image_corrected_distorsion_R = distorsion_correction_python(path_script_python_distorsion,path_vignetting_R,path_distorsion_R,fx_R,fy_R,Cx_R+cx_R,Cy_R+cy_R,k1_R,k2_R,p1_R,p2_R,k3_R)
image_corrected_distorsion_RE = distorsion_correction_python(path_script_python_distorsion,path_vignetting_RE,path_distorsion_RE,fx_RE,fy_RE,Cx_RE+cx_RE,Cy_RE+cy_RE,k1_RE,k2_RE,p1_RE,p2_RE,k3_RE)

##Alignment correction-------------------------------------------------------------------------------------------
image_corrected_alignment_G = alignment_correction_python(path_script_python_alignment,path_distorsion_G,path_alignment_G,M11_G,M12_G,M13_G,M21_G,M22_G,M23_G,M31_G,M32_G,M33_G)
image_corrected_alignment_NIR = alignment_correction_python(path_script_python_alignment,path_distorsion_NIR,path_alignment_NIR,M11_NIR,M12_NIR,M13_NIR,M21_NIR,M22_NIR,M23_NIR,M31_NIR,M32_NIR,M33_NIR)
image_corrected_alignment_R = alignment_correction_python(path_script_python_alignment,path_distorsion_R,path_alignment_R,M11_R,M12_R,M13_R,M21_R,M22_R,M23_R,M31_R,M32_R,M33_R)
image_corrected_alignment_RE = alignment_correction_python(path_script_python_alignment,path_distorsion_RE,path_alignment_RE,M11_RE,M12_RE,M13_RE,M21_RE,M22_RE,M23_RE,M31_RE,M32_RE,M33_RE)

##ECC correction-------------------------------------------------------------------------------------------------
file.copy(path_alignment_NIR,path_ECC_NIR,overwrite = T)
image_corrected_ECC_alignment_NIR = t(matrix(rast(path_ECC_NIR),ncol=height_NIR))
image_corrected_ECC_alignment_G = ECC_correction_python(path_script_python_ECC_alignment,path_alignment_NIR,path_alignment_G,path_ECC_G)
image_corrected_ECC_alignment_R = ECC_correction_python(path_script_python_ECC_alignment,path_alignment_NIR,path_alignment_R,path_ECC_R)
image_corrected_ECC_alignment_RE = ECC_correction_python(path_script_python_ECC_alignment,path_alignment_NIR,path_alignment_RE,path_ECC_RE)

##Homogenization-------------------------------------------------------------------------------------------------
G_test = image_corrected_ECC_alignment_G<=min_raw_G
NIR_test = image_corrected_ECC_alignment_NIR<=min_raw_NIR
R_test = image_corrected_ECC_alignment_R<=min_raw_R
RE_test = image_corrected_ECC_alignment_RE<=min_raw_RE
indices_test = which(G_test|NIR_test|R_test|RE_test)

image_corrected_ECC_homogenized_G = image_corrected_ECC_alignment_G
image_corrected_ECC_homogenized_NIR = image_corrected_ECC_alignment_NIR
image_corrected_ECC_homogenized_R = image_corrected_ECC_alignment_R
image_corrected_ECC_homogenized_RE = image_corrected_ECC_alignment_RE

image_corrected_ECC_homogenized_G[indices_test]=NaN
image_corrected_ECC_homogenized_NIR[indices_test]=NaN
image_corrected_ECC_homogenized_R[indices_test]=NaN
image_corrected_ECC_homogenized_RE[indices_test]=NaN

writeRaster(rast(image_corrected_ECC_homogenized_G),path_homogenized_G,datatype = datatype(raw_rast_G),overwrite=T)
writeRaster(rast(image_corrected_ECC_homogenized_NIR),path_homogenized_NIR,datatype = datatype(raw_rast_NIR),overwrite=T)
writeRaster(rast(image_corrected_ECC_homogenized_R),path_homogenized_R,datatype = datatype(raw_rast_R),overwrite=T)
writeRaster(rast(image_corrected_ECC_homogenized_RE),path_homogenized_RE,datatype = datatype(raw_rast_RE),overwrite=T)

##Radiance-----------------------------------------------------------------------------------------------------
radiance_G = ((rast(path_homogenized_G)-BlackLevel_G)/(2^BPS_G))/(gain_G*exposure_time_G)
radiance_NIR = ((rast(path_homogenized_NIR)-BlackLevel_NIR)/(2^BPS_NIR))/(gain_NIR*exposure_time_NIR)
radiance_R = ((rast(path_homogenized_R)-BlackLevel_R)/(2^BPS_R))/(gain_R*exposure_time_R)
radiance_RE = ((rast(path_homogenized_RE)-BlackLevel_RE)/(2^BPS_RE))/(gain_RE*exposure_time_RE)

##Reflectance/rhoNIR---------------------------------------------------------------------------------------------
reflectance_G = radiance_G*sensor_gain_adjustment_G/radiance_down_G
reflectance_NIR = radiance_NIR*sensor_gain_adjustment_NIR/radiance_down_NIR
reflectance_R = radiance_R*sensor_gain_adjustment_R/radiance_down_R
reflectance_RE = radiance_RE*sensor_gain_adjustment_RE/radiance_down_RE

##rhoNIR Computation---------------------------------------------------------------------------------------------
Q_bas = 0.95
Q_bas_G = quantile(as.vector(values(reflectance_G)),Q_bas,na.rm=T)[[1]]
Q_bas_NIR = quantile(as.vector(values(reflectance_NIR)),Q_bas,na.rm=T)[[1]]
Q_bas_R = quantile(as.vector(values(reflectance_R)),Q_bas,na.rm=T)[[1]]
Q_bas_RE = quantile(as.vector(values(reflectance_RE)),Q_bas,na.rm=T)[[1]]

Seuil_bas_G = reflectance_G>Q_bas_G
Seuil_bas_NIR = reflectance_NIR>Q_bas_NIR
Seuil_bas_R = reflectance_R>Q_bas_R
Seuil_bas_RE = reflectance_RE>Q_bas_RE

x_min_spectralon = 1000
x_max_spectralon = 1300
y_min_spectralon = 500
y_max_spectralon = 1000

Seuil_x = rast(x_mat_G)<x_max_spectralon & rast(x_mat_G)>x_min_spectralon
Seuil_y = rast(1945-y_mat_G)<y_max_spectralon & rast(1945-y_mat_G)>y_min_spectralon

spectralon = Seuil_bas_G & Seuil_bas_NIR & Seuil_bas_R & Seuil_bas_RE & Seuil_x & Seuil_y

reflectance_G_spectralon = reflectance_G
values(reflectance_G_spectralon)[which(values(!spectralon))] = NaN
reflectance_NIR_spectralon = reflectance_NIR
values(reflectance_NIR_spectralon)[which(values(!spectralon))] = NaN
reflectance_R_spectralon = reflectance_R
values(reflectance_R_spectralon)[which(values(!spectralon))] = NaN
reflectance_RE_spectralon = reflectance_RE
values(reflectance_RE_spectralon)[which(values(!spectralon))] = NaN

mean_reflectance_spectralon_G = mean(values(reflectance_G_spectralon),na.rm=T)
mean_reflectance_spectralon_NIR = mean(values(reflectance_NIR_spectralon),na.rm=T)
mean_reflectance_spectralon_R = mean(values(reflectance_R_spectralon),na.rm=T)
mean_reflectance_spectralon_RE = mean(values(reflectance_RE_spectralon),na.rm=T)

rhoNIR_G = 0.985/mean_reflectance_spectralon_G
rhoNIR_NIR = 0.985/mean_reflectance_spectralon_NIR
rhoNIR_R = 0.985/mean_reflectance_spectralon_R
rhoNIR_RE = 0.985/mean_reflectance_spectralon_RE