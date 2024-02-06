##Packages--------------------------------------------------------------------------------------------------------
library(exiftoolr)
library(terra)
system("python -m pip install numpy",intern = T)
system("python -m pip install opencv-python",intern= T)

##Path------------------------------------------------------------------------------------------------------------
#Path of the raw images for each bands
path_G = "example_images/example_MS_G.TIF"
path_NIR = "example_images/example_MS_NIR.TIF"
path_R = "example_images/example_MS_R.TIF"
path_RE = "example_images/example_MS_RE.TIF"

#Path of directories where images will be stored after each corrections
path_dir_out_vignetting = "step_1_vignetting"
path_dir_out_distorsion = "step_2_distorsion"

#Path of the python script used for distorsion correction
path_script_python_distorsion = "distorsion_calibration.py"

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

##Importing exif--------------------------------------------------------------------------------------------------
if(!(exists("exif_G") && exists("exif_NIR") && exists("exif_R") && exists("exif_RE"))){
  exif_G = exif_read(path_G)
  exif_NIR = exif_read(path_NIR)
  exif_R = exif_read(path_R)
  exif_RE = exif_read(path_RE)
}

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

vignetting_flag_G = exif_G$VignettingFlag
vignetting_flag_NIR = exif_NIR$VignettingFlag
vignetting_flag_R = exif_R$VignettingFlag
vignetting_flag_RE = exif_RE$VignettingFlag

distorsion_flag_G = exif_G$DewarpFlag
distorsion_flag_NIR = exif_NIR$DewarpFlag
distorsion_flag_R = exif_R$DewarpFlag
distorsion_flag_RE = exif_RE$DewarpFlag

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

##Importing images------------------------------------------------------------------------------------------------
if(!(exists("raw_rast_G") && exists("raw_rast_NIR") && exists("raw_rast_R") && exists("raw_rast_RE"))){
  raw_rast_G = rast(path_G)
  raw_rast_NIR = rast(path_NIR)
  raw_rast_R = rast(path_R)
  raw_rast_RE = rast(path_RE)
}

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
  system(paste("python",path_script_python_distorsion, path_image_in, path_image_out, fx, fy, cx, cy, k1, k2, p1, p2, k3), intern = T)
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
  return(image_out_matrix)
}

##Vignetting correction-------------------------------------------------------------------------------------------
x_mat_G = matrix(rep(1:width_G,each=height_G),ncol=width_G)
y_mat_G = matrix(rep(1:height_G,times=width_G),ncol=width_G)
matrice_vignetting_G = polynomial_correction_G(x_mat_G,y_mat_G)
image_corrected_vignetting_G = raw_rast_G*rast(matrice_vignetting_G)
writeRaster(image_corrected_vignetting_G,path_vignetting_G)

x_mat_NIR = matrix(rep(1:width_NIR,each=height_NIR),ncol=width_NIR)
y_mat_NIR = matrix(rep(1:height_NIR,times=width_NIR),ncol=width_NIR)
matrice_vignetting_NIR = polynomial_correction_NIR(x_mat_NIR,y_mat_NIR)
image_corrected_vignetting_NIR = raw_rast_NIR*rast(matrice_vignetting_NIR)
writeRaster(image_corrected_vignetting_NIR,path_vignetting_NIR)

x_mat_R = matrix(rep(1:width_R,each=height_R),ncol=width_R)
y_mat_R = matrix(rep(1:height_R,times=width_R),ncol=width_R)
matrice_vignetting_R = polynomial_correction_R(x_mat_R,y_mat_R)
image_corrected_vignetting_R = raw_rast_R*rast(matrice_vignetting_R)
writeRaster(image_corrected_vignetting_R,path_vignetting_R)

x_mat_RE = matrix(rep(1:width_RE,each=height_RE),ncol=width_RE)
y_mat_RE = matrix(rep(1:height_RE,times=width_RE),ncol=width_RE)
matrice_vignetting_RE = polynomial_correction_RE(x_mat_RE,y_mat_RE)
image_corrected_vignetting_RE = raw_rast_RE*rast(matrice_vignetting_RE)
writeRaster(image_corrected_vignetting_RE,path_vignetting_RE)

##Distortion correction-------------------------------------------------------------------------------------------
image_corrected_distorsion_G = distorsion_correction_python(path_script_python_distorsion,path_vignetting_G,path_distorsion_G,fx_G,fy_G,Cx_G+cx_G,Cy_G+cy_G,k1_G,k2_G,p1_G,p2_G,k3_G)
image_corrected_distorsion_NIR = distorsion_correction_python(path_script_python_distorsion,path_vignetting_NIR,path_distorsion_NIR,fx_NIR,fy_NIR,Cx_NIR+cx_NIR,Cy_NIR+cy_NIR,k1_NIR,k2_NIR,p1_NIR,p2_NIR,k3_NIR)
image_corrected_distorsion_R = distorsion_correction_python(path_script_python_distorsion,path_vignetting_R,path_distorsion_R,fx_R,fy_R,Cx_R+cx_R,Cy_R+cy_R,k1_R,k2_R,p1_R,p2_R,k3_R)
image_corrected_distorsion_RE = distorsion_correction_python(path_script_python_distorsion,path_vignetting_RE,path_distorsion_RE,fx_RE,fy_RE,Cx_RE+cx_RE,Cy_RE+cy_RE,k1_RE,k2_RE,p1_RE,p2_RE,k3_RE)
