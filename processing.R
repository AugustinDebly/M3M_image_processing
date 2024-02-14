##Library-----------------------------------------------------------------------

library(exiftoolr)
library(terra)
system("python -m pip install numpy")
system("python -m pip install opencv-python")

##Keep steps stored-------------------------------------------------------------
#Just for storage, steps will still be processed and temporarily stored even if FALSE

keep_step_1_vignetting     = TRUE
keep_step_2_distorsion     = TRUE
keep_step_3_alignment      = TRUE
keep_step_4_ECC_alignment  = TRUE
keep_step_5_homogenization = TRUE

##Path--------------------------------------------------------------------------

path_in                          = "example_images"

path_out_1                       = "step_1_vignetting"
path_out_2                       = "step_2_distorsion"
path_out_3                       = "step_3_alignment"
path_out_4                       = "step_4_ECC_alignment"
path_out_5                       = "step_5_homogenization"

path_script_python_distorsion    = "distorsion_calibration.py"
path_script_python_alignment     = "alignment_calibration.py"
path_script_python_ECC_alignment = "ECC_alignment_calibration.py"

name_images                      = list.files(path_in,pattern = ".TIF")

path_images_in                   = paste(path_in,name_images,sep="/")
path_images_out_1                = paste(path_out_1,name_images,sep="/")
path_images_out_2                = paste(path_out_2,name_images,sep="/")
path_images_out_3                = paste(path_out_3,name_images,sep="/")
path_images_out_4                = paste(path_out_4,name_images,sep="/")
path_images_out_5                = paste(path_out_5,name_images,sep="/")

##Functions---------------------------------------------------------------------

polynomial_correction <- function(x,y,Cx,Cy,c1,c2,c3,c4,c5,c6){
  r = sqrt((x-Cx)^2+(y-Cy)^2)
  return(1+c1*(r^1)+c2*(r^2)+c3*(r^3)+c4*(r^4)+c5*(r^5)+c6*(r^6))
}

distorsion_correction_python <- function(path_script_python_distorsion,path_image_in,path_image_out,fx,fy,cx,cy,k1,k2,p1,p2,k3){
  system(paste("python",path_script_python_distorsion, path_image_in, path_image_out, fx, fy, cx, cy, k1, k2, p1, p2, k3))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
  return(image_out_matrix)
}

alignment_correction_python <- function(path_script_python_alignment,path_image_in,path_image_out,M11,M12,M13,M21,M22,M23,M31,M32,M33){
  system(paste("python",path_script_python_alignment, path_image_in, path_image_out, M11 ,M12 ,M13 ,M21 ,M22 ,M23 ,M31 ,M32 ,M33))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
}

ECC_correction_python <- function(path_script_python_ECC_alignment,path_image_in_1,path_image_in_2,path_image_out){
  system(paste("python", path_script_python_ECC_alignment, path_image_in_1, path_image_in_2, path_image_out))
  image_out_rast = rast(path_image_out)
  image_out_matrix = t(matrix(image_out_rast,nrow=ext(image_out_rast)[2]))
}

##Loop--------------------------------------------------------------------------

for(i in seq(length(path_images_in)/4)){
  pattern    = paste("_",formatC(i,3,flag = "0",format="d"),"_",sep="")
  p_in       = path_images_in[which(grepl(pattern,path_images_in))]
  p_out_1    = path_images_out_1[which(grepl(pattern,path_images_out_1))]
  p_out_2    = path_images_out_2[which(grepl(pattern,path_images_out_2))]
  p_out_3    = path_images_out_3[which(grepl(pattern,path_images_out_3))]
  p_out_4    = path_images_out_4[which(grepl(pattern,path_images_out_4))]
  p_out_5    = path_images_out_5[which(grepl(pattern,path_images_out_5))]
  
  p_in_b1    = p_in[1]
  p_in_b2    = p_in[2]
  p_in_b3    = p_in[3]
  p_in_b4    = p_in[4]
  
  p_out_1_b1 = p_out_1[1]
  p_out_1_b2 = p_out_1[2]
  p_out_1_b3 = p_out_1[3]
  p_out_1_b4 = p_out_1[4]
  
  p_out_2_b1 = p_out_2[1]
  p_out_2_b2 = p_out_2[2]
  p_out_2_b3 = p_out_2[3]
  p_out_2_b4 = p_out_2[4]
  
  p_out_3_b1 = p_out_3[1]
  p_out_3_b2 = p_out_3[2]
  p_out_3_b3 = p_out_3[3]
  p_out_3_b4 = p_out_3[4]
  
  p_out_4_b1 = p_out_4[1]
  p_out_4_b2 = p_out_4[2]
  p_out_4_b3 = p_out_4[3]
  p_out_4_b4 = p_out_4[4]
  
  p_out_5_b1 = p_out_5[1]
  p_out_5_b2 = p_out_5[2]
  p_out_5_b3 = p_out_5[3]
  p_out_5_b4 = p_out_5[4]
  
  #Importing metadata-----------------------------------------------------------
  
  exif_b1 = exif_read(p_in_b1)
  exif_b2 = exif_read(p_in_b2)
  exif_b3 = exif_read(p_in_b3)
  exif_b4 = exif_read(p_in_b4)
  
  #Importing images-------------------------------------------------------------
  
  raw_rast_b1 = rast(p_in_b1)
  raw_rast_b2 = rast(p_in_b2)
  raw_rast_b3 = rast(p_in_b3)
  raw_rast_b4 = rast(p_in_b4)
  
  #Parameters-------------------------------------------------------------------
  
  height_b1 = exif_b1$ImageHeight
  width_b1  = exif_b1$ImageWidth
  height_b2 = exif_b2$ImageHeight
  width_b2  = exif_b2$ImageWidth
  height_b3 = exif_b3$ImageHeight
  width_b3  = exif_b3$ImageWidth
  height_b4 = exif_b4$ImageHeight
  width_b4  = exif_b4$ImageWidth
  
  Cx_b1 = exif_b1$VignettingCenter[[1]][1]
  Cy_b1 = exif_b1$VignettingCenter[[1]][2]
  Cx_b2 = exif_b2$VignettingCenter[[1]][1]
  Cy_b2 = exif_b2$VignettingCenter[[1]][2]
  Cx_b3 = exif_b3$VignettingCenter[[1]][1]
  Cy_b3 = exif_b3$VignettingCenter[[1]][2]
  Cx_b4 = exif_b4$VignettingCenter[[1]][1]
  Cy_b4 = exif_b4$VignettingCenter[[1]][2]
  
  c1_b1 = exif_b1$VignettingPolynomial[[1]][1]
  c2_b1 = exif_b1$VignettingPolynomial[[1]][2]
  c3_b1 = exif_b1$VignettingPolynomial[[1]][3]
  c4_b1 = exif_b1$VignettingPolynomial[[1]][4]
  c5_b1 = exif_b1$VignettingPolynomial[[1]][5]
  c6_b1 = exif_b1$VignettingPolynomial[[1]][6]
  
  c1_b2 = exif_b2$VignettingPolynomial[[1]][1]
  c2_b2 = exif_b2$VignettingPolynomial[[1]][2]
  c3_b2 = exif_b2$VignettingPolynomial[[1]][3]
  c4_b2 = exif_b2$VignettingPolynomial[[1]][4]
  c5_b2 = exif_b2$VignettingPolynomial[[1]][5]
  c6_b2 = exif_b2$VignettingPolynomial[[1]][6]
  
  c1_b3 = exif_b3$VignettingPolynomial[[1]][1]
  c2_b3 = exif_b3$VignettingPolynomial[[1]][2]
  c3_b3 = exif_b3$VignettingPolynomial[[1]][3]
  c4_b3 = exif_b3$VignettingPolynomial[[1]][4]
  c5_b3 = exif_b3$VignettingPolynomial[[1]][5]
  c6_b3 = exif_b3$VignettingPolynomial[[1]][6]
  
  c1_b4 = exif_b4$VignettingPolynomial[[1]][1]
  c2_b4 = exif_b4$VignettingPolynomial[[1]][2]
  c3_b4 = exif_b4$VignettingPolynomial[[1]][3]
  c4_b4 = exif_b4$VignettingPolynomial[[1]][4]
  c5_b4 = exif_b4$VignettingPolynomial[[1]][5]
  c6_b4 = exif_b4$VignettingPolynomial[[1]][6]
  
  distorsion_info_b1 = as.numeric(strsplit(exif_b1$DewarpData,"[;,]")[[1]])
  distorsion_info_b2 = as.numeric(strsplit(exif_b2$DewarpData,"[;,]")[[1]])
  distorsion_info_b3 = as.numeric(strsplit(exif_b3$DewarpData,"[;,]")[[1]])
  distorsion_info_b4 = as.numeric(strsplit(exif_b4$DewarpData,"[;,]")[[1]])
  
  fx_b1 = distorsion_info_b1[2]
  fy_b1 = distorsion_info_b1[3]
  cx_b1 = distorsion_info_b1[4]
  cy_b1 = distorsion_info_b1[5]
  k1_b1 = distorsion_info_b1[6]
  k2_b1 = distorsion_info_b1[7]
  p1_b1 = distorsion_info_b1[8]
  p2_b1 = distorsion_info_b1[9]
  k3_b1 = distorsion_info_b1[10]
  fx_b2 = distorsion_info_b2[2]
  fy_b2 = distorsion_info_b2[3]
  cx_b2 = distorsion_info_b2[4]
  cy_b2 = distorsion_info_b2[5]
  k1_b2 = distorsion_info_b2[6]
  k2_b2 = distorsion_info_b2[7]
  p1_b2 = distorsion_info_b2[8]
  p2_b2 = distorsion_info_b2[9]
  k3_b2 = distorsion_info_b2[10]
  fx_b3 = distorsion_info_b3[2]
  fy_b3 = distorsion_info_b3[3]
  cx_b3 = distorsion_info_b3[4]
  cy_b3 = distorsion_info_b3[5]
  k1_b3 = distorsion_info_b3[6]
  k2_b3 = distorsion_info_b3[7]
  p1_b3 = distorsion_info_b3[8]
  p2_b3 = distorsion_info_b3[9]
  k3_b3 = distorsion_info_b3[10]
  fx_b4 = distorsion_info_b4[2]
  fy_b4 = distorsion_info_b4[3]
  cx_b4 = distorsion_info_b4[4]
  cy_b4 = distorsion_info_b4[5]
  k1_b4 = distorsion_info_b4[6]
  k2_b4 = distorsion_info_b4[7]
  p1_b4 = distorsion_info_b4[8]
  p2_b4 = distorsion_info_b4[9]
  k3_b4 = distorsion_info_b4[10]
  
  alignment_info_b1 = as.numeric(strsplit(exif_b1$CalibratedHMatrix,"[;,]")[[1]])
  alignment_info_b2 = as.numeric(strsplit(exif_b2$CalibratedHMatrix,"[;,]")[[1]])
  alignment_info_b3 = as.numeric(strsplit(exif_b3$CalibratedHMatrix,"[;,]")[[1]])
  alignment_info_b4 = as.numeric(strsplit(exif_b4$CalibratedHMatrix,"[;,]")[[1]])
  
  M11_b1 = alignment_info_b1[1]
  M12_b1 = alignment_info_b1[2]
  M13_b1 = alignment_info_b1[3]
  M21_b1 = alignment_info_b1[4]
  M22_b1 = alignment_info_b1[5]
  M23_b1 = alignment_info_b1[6]
  M31_b1 = alignment_info_b1[7]
  M32_b1 = alignment_info_b1[8]
  M33_b1 = alignment_info_b1[9]
  M11_b2 = alignment_info_b2[1]
  M12_b2 = alignment_info_b2[2]
  M13_b2 = alignment_info_b2[3]
  M21_b2 = alignment_info_b2[4]
  M22_b2 = alignment_info_b2[5]
  M23_b2 = alignment_info_b2[6]
  M31_b2 = alignment_info_b2[7]
  M32_b2 = alignment_info_b2[8]
  M33_b2 = alignment_info_b2[9]
  M11_b3 = alignment_info_b3[1]
  M12_b3 = alignment_info_b3[2]
  M13_b3 = alignment_info_b3[3]
  M21_b3 = alignment_info_b3[4]
  M22_b3 = alignment_info_b3[5]
  M23_b3 = alignment_info_b3[6]
  M31_b3 = alignment_info_b3[7]
  M32_b3 = alignment_info_b3[8]
  M33_b3 = alignment_info_b3[9]
  M11_b4 = alignment_info_b4[1]
  M12_b4 = alignment_info_b4[2]
  M13_b4 = alignment_info_b4[3]
  M21_b4 = alignment_info_b4[4]
  M22_b4 = alignment_info_b4[5]
  M23_b4 = alignment_info_b4[6]
  M31_b4 = alignment_info_b4[7]
  M32_b4 = alignment_info_b4[8]
  M33_b4 = alignment_info_b4[9]
  
  BlackLevel_b1 = exif_b1$BlackLevel
  BlackLevel_b2 = exif_b2$BlackLevel
  BlackLevel_b3 = exif_b3$BlackLevel
  BlackLevel_b4 = exif_b4$BlackLevel
  
  BPS_b1 = exif_b1$BitsPerSample
  BPS_b2 = exif_b2$BitsPerSample
  BPS_b3 = exif_b3$BitsPerSample
  BPS_b4 = exif_b4$BitsPerSample
  
  exposure_time_b1 = exif_b1$ExposureTime
  exposure_time_b2 = exif_b2$ExposureTime
  exposure_time_b3 = exif_b3$ExposureTime
  exposure_time_b4 = exif_b4$ExposureTime
  
  gain_b1 = exif_b1$SensorGain
  gain_b2 = exif_b2$SensorGain
  gain_b3 = exif_b3$SensorGain
  gain_b4 = exif_b4$SensorGain
  
  radiance_down_b1 = exif_b1$Irradiance
  radiance_down_b2 = exif_b2$Irradiance
  radiance_down_b3 = exif_b3$Irradiance
  radiance_down_b4 = exif_b4$Irradiance
  
  sensor_Gain_adjustment_b1 = exif_b1$SensorGainAdjustment
  sensor_Gain_adjustment_b2 = exif_b2$SensorGainAdjustment
  sensor_Gain_adjustment_b3 = exif_b3$SensorGainAdjustment
  sensor_Gain_adjustment_b4 = exif_b4$SensorGainAdjustment
  
  #Vignetting-------------------------------------------------------------------
  
  x_mat_b1              = matrix(rep(1:width_b1,each=height_b1),ncol=width_b1)
  y_mat_b1              = matrix(rep(1:height_b1,times=width_b1),ncol=width_b1)
  x_mat_b2              = matrix(rep(1:width_b2,each=height_b2),ncol=width_b2)
  y_mat_b2              = matrix(rep(1:height_b2,times=width_b2),ncol=width_b2)
  x_mat_b3              = matrix(rep(1:width_b3,each=height_b3),ncol=width_b3)
  y_mat_b3              = matrix(rep(1:height_b3,times=width_b3),ncol=width_b3)
  x_mat_b4              = matrix(rep(1:width_b4,each=height_b4),ncol=width_b4)
  y_mat_b4              = matrix(rep(1:height_b4,times=width_b4),ncol=width_b4)
  matrice_vignetting_b1 = polynomial_correction(x_mat_b1,y_mat_b1,Cx_b1,Cy_b1,c1_b1,c2_b1,c3_b1,c4_b1,c5_b1,c6_b1)
  matrice_vignetting_b2 = polynomial_correction(x_mat_b2,y_mat_b2,Cx_b2,Cy_b2,c1_b2,c2_b2,c3_b2,c4_b2,c5_b2,c6_b2)
  matrice_vignetting_b3 = polynomial_correction(x_mat_b3,y_mat_b3,Cx_b3,Cy_b3,c1_b3,c2_b3,c3_b3,c4_b3,c5_b3,c6_b3)
  matrice_vignetting_b4 = polynomial_correction(x_mat_b4,y_mat_b4,Cx_b4,Cy_b4,c1_b4,c2_b4,c3_b4,c4_b4,c5_b4,c6_b4)
  
  image_corrected_vignetting_b1 = raw_rast_b1*rast(matrice_vignetting_b1)
  image_corrected_vignetting_b2 = raw_rast_b2*rast(matrice_vignetting_b2)
  image_corrected_vignetting_b3 = raw_rast_b3*rast(matrice_vignetting_b3)
  image_corrected_vignetting_b4 = raw_rast_b4*rast(matrice_vignetting_b4)
  
  max_raw_b1       = max(values(raw_rast_b1))
  min_raw_b1       = min(values(raw_rast_b1))
  max_corrected_b1 = max(values(image_corrected_vignetting_b1))
  min_corrected_b1 = min(values(image_corrected_vignetting_b1))
  a_b1             = (max_raw_b1-min_raw_b1)/(max_corrected_b1-min_corrected_b1)
  b_b1             = min_raw_b1 - a_b1*min_corrected_b1
  max_raw_b2       = max(values(raw_rast_b2))
  min_raw_b2       = min(values(raw_rast_b2))
  max_corrected_b2 = max(values(image_corrected_vignetting_b2))
  min_corrected_b2 = min(values(image_corrected_vignetting_b2))
  a_b2             = (max_raw_b2-min_raw_b2)/(max_corrected_b2-min_corrected_b2)
  b_b2             = min_raw_b2 - a_b2*min_corrected_b2
  max_raw_b3       = max(values(raw_rast_b3))
  min_raw_b3       = min(values(raw_rast_b3))
  max_corrected_b3 = max(values(image_corrected_vignetting_b3))
  min_corrected_b3 = min(values(image_corrected_vignetting_b3))
  a_b3             = (max_raw_b3-min_raw_b3)/(max_corrected_b3-min_corrected_b3)
  b_b3             = min_raw_b3 - a_b3*min_corrected_b3
  max_raw_b4       = max(values(raw_rast_b4))
  min_raw_b4       = min(values(raw_rast_b4))
  max_corrected_b4 = max(values(image_corrected_vignetting_b4))
  min_corrected_b4 = min(values(image_corrected_vignetting_b4))
  a_b4             = (max_raw_b4-min_raw_b4)/(max_corrected_b4-min_corrected_b4)
  b_b4             = min_raw_b4 - a_b4*min_corrected_b4
  
  image_corrected_vignetting_b1         = a_b1 * image_corrected_vignetting_b1 + b_b1
  values(image_corrected_vignetting_b1) = as.integer(values(image_corrected_vignetting_b1))
  image_corrected_vignetting_b2         = a_b2 * image_corrected_vignetting_b2 + b_b2
  values(image_corrected_vignetting_b2) = as.integer(values(image_corrected_vignetting_b2))
  image_corrected_vignetting_b3         = a_b3 * image_corrected_vignetting_b3 + b_b3
  values(image_corrected_vignetting_b3) = as.integer(values(image_corrected_vignetting_b3))
  image_corrected_vignetting_b4         = a_b4 * image_corrected_vignetting_b4 + b_b4
  values(image_corrected_vignetting_b4) = as.integer(values(image_corrected_vignetting_b4))
  
  writeRaster(image_corrected_vignetting_b1,p_out_1_b1,datatype = datatype(raw_rast_b1),overwrite=T)
  #system(command = paste("exiftool -tagsFromFile",p_in_b1,"-xmp",p_out_1_b1,sep = " "))
  
  writeRaster(image_corrected_vignetting_b2,p_out_1_b2,datatype = datatype(raw_rast_b2),overwrite=T)
  #system(command = paste("exiftool -tagsFromFile",p_in_b2,"-xmp",p_out_1_b2,sep = " "))
  
  writeRaster(image_corrected_vignetting_b3,p_out_1_b3,datatype = datatype(raw_rast_b3),overwrite=T)
  #system(command = paste("exiftool -tagsFromFile",p_in_b3,"-xmp",p_out_1_b3,sep = " "))
  
  writeRaster(image_corrected_vignetting_b4,p_out_1_b4,datatype = datatype(raw_rast_b4),overwrite=T)
  #system(command = paste("exiftool -tagsFromFile",p_in_b4,"-xmp",p_out_1_b4,sep = " "))
  
  
  #Distorsion-------------------------------------------------------------------
  
  image_corrected_distorsion_b1 = distorsion_correction_python(path_script_python_distorsion,p_out_1_b1,p_out_2_b1,fx_b1,fy_b1,Cx_b1+cx_b1,Cy_b1+cy_b1,k1_b1,k2_b1,p1_b1,p2_b1,k3_b1)
  image_corrected_distorsion_b2 = distorsion_correction_python(path_script_python_distorsion,p_out_1_b2,p_out_2_b2,fx_b2,fy_b2,Cx_b2+cx_b2,Cy_b2+cy_b2,k1_b2,k2_b2,p1_b2,p2_b2,k3_b2)
  image_corrected_distorsion_b3 = distorsion_correction_python(path_script_python_distorsion,p_out_1_b3,p_out_2_b3,fx_b3,fy_b3,Cx_b3+cx_b3,Cy_b3+cy_b3,k1_b3,k2_b3,p1_b3,p2_b3,k3_b3)
  image_corrected_distorsion_b4 = distorsion_correction_python(path_script_python_distorsion,p_out_1_b4,p_out_2_b4,fx_b4,fy_b4,Cx_b4+cx_b4,Cy_b4+cy_b4,k1_b4,k2_b4,p1_b4,p2_b4,k3_b4)
  
  #Alignment--------------------------------------------------------------------
  
  image_corrected_alignment_b1 = alignment_correction_python(path_script_python_alignment,p_out_2_b1,p_out_3_b1,M11_b1,M12_b1,M13_b1,M21_b1,M22_b1,M23_b1,M31_b1,M32_b1,M33_b1)
  image_corrected_alignment_b2 = alignment_correction_python(path_script_python_alignment,p_out_2_b2,p_out_3_b2,M11_b2,M12_b2,M13_b2,M21_b2,M22_b2,M23_b2,M31_b2,M32_b2,M33_b2)
  image_corrected_alignment_b3 = alignment_correction_python(path_script_python_alignment,p_out_2_b3,p_out_3_b3,M11_b3,M12_b3,M13_b3,M21_b3,M22_b3,M23_b3,M31_b3,M32_b3,M33_b3)
  image_corrected_alignment_b4 = alignment_correction_python(path_script_python_alignment,p_out_2_b4,p_out_3_b4,M11_b4,M12_b4,M13_b4,M21_b4,M22_b4,M23_b4,M31_b4,M32_b4,M33_b4)
  
  #ECC Alignment----------------------------------------------------------------
  
  file.copy(p_out_3_b1,p_out_4_b1,overwrite = T)
  image_corrected_ECC_alignment_b2 = ECC_correction_python(path_script_python_ECC_alignment,p_out_3_b1,p_out_3_b2,p_out_4_b2)
  image_corrected_ECC_alignment_b3 = ECC_correction_python(path_script_python_ECC_alignment,p_out_3_b1,p_out_3_b3,p_out_4_b3)
  image_corrected_ECC_alignment_b4 = ECC_correction_python(path_script_python_ECC_alignment,p_out_3_b1,p_out_3_b4,p_out_4_b4)
  
  #Homogenization---------------------------------------------------------------
  
  image_corrected_homogenized_b1 = rast(p_out_4_b1)
  image_corrected_homogenized_b2 = rast(p_out_4_b2)
  image_corrected_homogenized_b3 = rast(p_out_4_b3)
  image_corrected_homogenized_b4 = rast(p_out_4_b4)
  b1_test = image_corrected_homogenized_b1<min_raw_b1
  b2_test = image_corrected_homogenized_b2<min_raw_b2
  b3_test = image_corrected_homogenized_b3<min_raw_b3
  b4_test = image_corrected_homogenized_b4<min_raw_b4
  all_bands_test = b1_test | b2_test | b3_test | b4_test
  values(image_corrected_homogenized_b1)[which(values(all_bands_test))]=NA
  values(image_corrected_homogenized_b2)[which(values(all_bands_test))]=NA
  values(image_corrected_homogenized_b3)[which(values(all_bands_test))]=NA
  values(image_corrected_homogenized_b4)[which(values(all_bands_test))]=NA
  writeRaster(image_corrected_homogenized_b1,p_out_5_b1,datatype = datatype(raw_rast_b1),overwrite=T)
  writeRaster(image_corrected_homogenized_b2,p_out_5_b2,datatype = datatype(raw_rast_b2),overwrite=T)
  writeRaster(image_corrected_homogenized_b3,p_out_5_b3,datatype = datatype(raw_rast_b3),overwrite=T)
  writeRaster(image_corrected_homogenized_b4,p_out_5_b4,datatype = datatype(raw_rast_b4),overwrite=T)
  
  #Remove unwanted storage------------------------------------------------------
  
  if(!keep_step_1_vignetting){
    file.remove(p_out_1_b1)
    file.remove(p_out_1_b2)
    file.remove(p_out_1_b3)
    file.remove(p_out_1_b4)
  }
  if(!keep_step_2_distorsion){
    file.remove(p_out_2_b1)
    file.remove(p_out_2_b2)
    file.remove(p_out_2_b3)
    file.remove(p_out_2_b4)
  }
  if(!keep_step_3_alignment){
    file.remove(p_out_3_b1)
    file.remove(p_out_3_b2)
    file.remove(p_out_3_b3)
    file.remove(p_out_3_b4)
  }
  if(!keep_step_4_ECC_alignment){
    file.remove(p_out_4_b1)
    file.remove(p_out_4_b2)
    file.remove(p_out_4_b3)
    file.remove(p_out_4_b4)
  }
  if(!keep_step_5_homogenization){
    file.remove(p_out_5_b1)
    file.remove(p_out_5_b2)
    file.remove(p_out_5_b3)
    file.remove(p_out_5_b4)
  }
}

#path_original_exiftool = paste(path_out,list.files(path_out,pattern = "_original"),sep="/")
#file.remove(path_original_exiftool)
