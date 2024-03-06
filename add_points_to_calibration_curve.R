##Packages----------------------------------------------------------------------

require(exiftoolr)
require(terra)
require(shiny)
#install_exiftool()

##Clear options-----------------------------------------------------------------

clear_dir_after_adding_points    = TRUE

##Path--------------------------------------------------------------------------

path_in_raw_images               = "spectralon_images"
path_in_radiance                 = "step_6_radiance"

path_points_G                    = "calibration_reflectance/points_calibration_G.csv"
path_points_NIR                  = "calibration_reflectance/points_calibration_NIR.csv"
path_points_R                    = "calibration_reflectance/points_calibration_R.csv"
path_points_RE                   = "calibration_reflectance/points_calibration_RE.csv"

path_coeff_radiance_irradiance   = "calibration_reflectance/coeff_radiance_irradiance.RData"

R_spectralon_G                   = 0.875192433
R_spectralon_NIR                 = 0.868823461
R_spectralon_R                   = 0.873230624
R_spectralon_RE                  = 0.876424875

name_images_G                    = list.files(path_in_raw_images,pattern = "_G.TIF")
name_images_NIR                  = list.files(path_in_raw_images,pattern = "_NIR.TIF")
name_images_R                    = list.files(path_in_raw_images,pattern = "_R.TIF")
name_images_RE                   = list.files(path_in_raw_images,pattern = "_RE.TIF")
name_images_RGB                  = list.files(path_in_raw_images,pattern = "_D.JPG")

path_images_in_raw_G             = paste(path_in_raw_images,name_images_G,sep="/")
path_images_in_raw_NIR           = paste(path_in_raw_images,name_images_NIR,sep="/")
path_images_in_raw_R             = paste(path_in_raw_images,name_images_R,sep="/")
path_images_in_raw_RE            = paste(path_in_raw_images,name_images_RE,sep="/")
path_images_in_raw_RGB           = paste(path_in_raw_images,name_images_RGB,sep="/")

path_images_in_radiance_G        = paste(path_in_radiance,name_images_G,sep="/")
path_images_in_radiance_NIR      = paste(path_in_radiance,name_images_NIR,sep="/")
path_images_in_radiance_R        = paste(path_in_radiance,name_images_R,sep="/")
path_images_in_radiance_RE       = paste(path_in_radiance,name_images_RE,sep="/")

Irradiance_values_G              = exif_read(path_images_in_raw_G)$Irradiance
Irradiance_values_NIR            = exif_read(path_images_in_raw_NIR)$Irradiance
Irradiance_values_R              = exif_read(path_images_in_raw_R)$Irradiance
Irradiance_values_RE             = exif_read(path_images_in_raw_RE)$Irradiance

##Variables---------------------------------------------------------------------

coordonnees_click_x = c()
coordonnees_click_y = c()

dx                  = 10
dy                  = 10

##Functions---------------------------------------------------------------------

center_spectralon <- function(path_images_in_raw_RGB,path_images_in_radiance_G){
  
  i = 1
  i_max = length(path_images_in_radiance_G)
  
  ui <- fluidPage(
    plotOutput("radiance_plot", click = "plot_click"),
    plotOutput("rgb_plot")
  )
  
  server <- function(input, output, session) {
    
    output$radiance_plot <- renderPlot({
      plot(rast(path_images_in_radiance_G[i]))
      text(1, 1, "Radiance image", adj = c(0,0), cex = 1.5, col = "red", font = 2)
    })
    
    output$rgb_plot <- renderPlot({
      plot(rast(path_images_in_raw_RGB[i]))
      text(1, 1, "RGB image", adj = c(0,0), cex = 1.5, col = "red", font = 2)
    })
    
    observeEvent(input$plot_click,{
      
      click_x <- input$plot_click$x
      click_y <- input$plot_click$y
      
      coordonnees_click_x <<- append(coordonnees_click_x,click_x)
      coordonnees_click_y <<- append(coordonnees_click_y,click_y)
      
      output$radiance_plot <- renderPlot({
        plot(rast(path_images_in_radiance_G[i]))
        text(1, 1, "Radiance image", adj = c(0,0), cex = 1.5, col = "red", font = 2)
      })
      
      output$rgb_plot <- renderPlot({
        plot(rast(path_images_in_raw_RGB[i]))
        text(1, 1, "RGB image", adj = c(0,0), cex = 1.5, col = "red", font = 2)
      })
      
      if (i < i_max){
        i <<- i + 1
        updateNavlistPanel(session, "main")
      } 
      else{
        stopApp()
      }
    })
  }
  shinyApp(ui, server)
}

##Loop--------------------------------------------------------------------------

center_spectralon(path_images_in_raw_RGB,path_images_in_radiance_G)

ext_spectralon = matrix(c(coordonnees_click_x-dx,coordonnees_click_x+dx,coordonnees_click_y-dy,coordonnees_click_y+dy),ncol=4)

indice_first_image = as.numeric(strsplit(path_images_in_radiance_G[1],"_")[[1]][5])
indice_last_image  = as.numeric(strsplit(path_images_in_radiance_G[length(path_images_in_radiance_G)],"_")[[1]][5])

liste_irradiance_spectralon_G   = c()
liste_irradiance_spectralon_NIR = c()
liste_irradiance_spectralon_R   = c()
liste_irradiance_spectralon_RE  = c()

for(i in seq(indice_first_image,indice_last_image)){
  pattern                   = paste("_",formatC(i,3,flag = "0",format="d"),sep="")
  p_in_G                    = path_images_in_radiance_G[which(grepl(pattern,path_images_in_radiance_G))]
  p_in_NIR                  = path_images_in_radiance_G[which(grepl(pattern,path_images_in_radiance_G))]
  p_in_R                    = path_images_in_radiance_G[which(grepl(pattern,path_images_in_radiance_G))]
  p_in_RE                   = path_images_in_radiance_G[which(grepl(pattern,path_images_in_radiance_G))]
  
  ext                       = ext_spectralon[i-indice_first_image+1,]
  
  radiance_spectralon_G     = mean(values(crop(rast(p_in_G),ext)))
  radiance_spectralon_NIR   = mean(values(crop(rast(p_in_NIR),ext)))
  radiance_spectralon_R     = mean(values(crop(rast(p_in_R),ext)))
  radiance_spectralon_RE    = mean(values(crop(rast(p_in_RE),ext)))
  
  irradiance_spectralon_G   = radiance_spectralon_G/R_spectralon_G
  irradiance_spectralon_NIR = radiance_spectralon_NIR/R_spectralon_NIR
  irradiance_spectralon_R   = radiance_spectralon_R/R_spectralon_R
  irradiance_spectralon_RE  = radiance_spectralon_RE/R_spectralon_RE
  
  liste_irradiance_spectralon_G   = append(liste_irradiance_spectralon_G,irradiance_spectralon_G)
  liste_irradiance_spectralon_NIR = append(liste_irradiance_spectralon_NIR,irradiance_spectralon_NIR)
  liste_irradiance_spectralon_R   = append(liste_irradiance_spectralon_R,irradiance_spectralon_R)
  liste_irradiance_spectralon_RE  = append(liste_irradiance_spectralon_RE,irradiance_spectralon_RE)
}

##CSV writing-------------------------------------------------------------------

file.copy(path_points_G,paste(substr(path_points_G,1,nchar(path_points_G)-4),"_backup.csv",sep = ""),overwrite = T)
file.copy(path_points_NIR,paste(substr(path_points_NIR,1,nchar(path_points_NIR)-4),"_backup.csv",sep = ""),overwrite = T)
file.copy(path_points_R,paste(substr(path_points_R,1,nchar(path_points_R)-4),"_backup.csv",sep = ""),overwrite = T)
file.copy(path_points_RE,paste(substr(path_points_RE,1,nchar(path_points_RE)-4),"_backup.csv",sep = ""),overwrite = T)

csv_content_G   = read.table(path_points_G,header = T,sep = ";",dec = ",")
csv_content_NIR = read.table(path_points_NIR,header = T,sep = ";",dec = ",")
csv_content_R   = read.table(path_points_R,header = T,sep = ";",dec = ",")
csv_content_RE  = read.table(path_points_RE,header = T,sep = ";",dec = ",")

csv_names_G   = csv_content_G$name_image
csv_names_NIR = csv_content_NIR$name_image
csv_names_R   = csv_content_R$name_image
csv_names_RE  = csv_content_RE$name_image

csv_irradiance_spectralon_G   = csv_content_G$irradiance_spectralon
csv_irradiance_spectralon_NIR = csv_content_NIR$irradiance_spectralon
csv_irradiance_spectralon_R   = csv_content_R$irradiance_spectralon
csv_irradiance_spectralon_RE  = csv_content_RE$irradiance_spectralon

csv_irradiance_G   = csv_content_G$irradiance
csv_irradiance_NIR = csv_content_NIR$irradiance
csv_irradiance_R   = csv_content_R$irradiance
csv_irradiance_RE  = csv_content_RE$irradiance

indices_to_add_G   = which(!(name_images_G %in% csv_names_G))
indices_to_add_NIR = which(!(name_images_NIR %in% csv_names_NIR))
indices_to_add_R   = which(!(name_images_R %in% csv_names_R))
indices_to_add_RE  = which(!(name_images_RE %in% csv_names_RE))

csv_names_G_with_add   = c(csv_names_G,name_images_G[indices_to_add_G])
csv_names_NIR_with_add = c(csv_names_NIR,name_images_NIR[indices_to_add_NIR])
csv_names_R_with_add   = c(csv_names_R,name_images_R[indices_to_add_R])
csv_names_RE_with_add  = c(csv_names_RE,name_images_RE[indices_to_add_RE])

csv_irradiance_spectralon_G_with_add   = c(csv_irradiance_spectralon_G,liste_irradiance_spectralon_G[indices_to_add_G])
csv_irradiance_spectralon_NIR_with_add = c(csv_irradiance_spectralon_NIR,liste_irradiance_spectralon_NIR[indices_to_add_NIR])
csv_irradiance_spectralon_R_with_add   = c(csv_irradiance_spectralon_R,liste_irradiance_spectralon_R[indices_to_add_R])
csv_irradiance_spectralon_RE_with_add  = c(csv_irradiance_spectralon_RE,liste_irradiance_spectralon_RE[indices_to_add_RE])

csv_irradiance_G_with_add   = c(csv_irradiance_G,Irradiance_values_G[indices_to_add_G])
csv_irradiance_NIR_with_add = c(csv_irradiance_NIR,Irradiance_values_NIR[indices_to_add_NIR])
csv_irradiance_R_with_add   = c(csv_irradiance_R,Irradiance_values_R[indices_to_add_R])
csv_irradiance_RE_with_add  = c(csv_irradiance_RE,Irradiance_values_RE[indices_to_add_RE])

write.table(data.frame(name_image=csv_names_G_with_add,irradiance_spectralon=csv_irradiance_spectralon_G_with_add,irradiance=csv_irradiance_G_with_add),file = path_points_G,sep=";",row.names = F,dec = ",")
write.table(data.frame(name_image=csv_names_NIR_with_add,irradiance_spectralon=csv_irradiance_spectralon_NIR_with_add,irradiance=csv_irradiance_NIR_with_add),file = path_points_NIR,sep=";",row.names = F,dec = ",")
write.table(data.frame(name_image=csv_names_R_with_add,irradiance_spectralon=csv_irradiance_spectralon_R_with_add,irradiance=csv_irradiance_R_with_add),file = path_points_R,sep=";",row.names = F,dec = ",")
write.table(data.frame(name_image=csv_names_RE_with_add,irradiance_spectralon=csv_irradiance_spectralon_RE_with_add,irradiance=csv_irradiance_RE_with_add),file = path_points_RE,sep=";",row.names = F,dec = ",")

##Coefficients------------------------------------------------------------------

linear_model_G   = lm(csv_irradiance_spectralon_G_with_add~csv_irradiance_G_with_add)
linear_model_NIR = lm(csv_irradiance_spectralon_NIR_with_add~csv_irradiance_NIR_with_add)
linear_model_R   = lm(csv_irradiance_spectralon_R_with_add~csv_irradiance_R_with_add)
linear_model_RE  = lm(csv_irradiance_spectralon_RE_with_add~csv_irradiance_RE_with_add)

a_radiance_G   = linear_model_G$coefficients[[2]]
b_radiance_G   = linear_model_G$coefficients[[1]]
a_radiance_NIR = linear_model_NIR$coefficients[[2]]
b_radiance_NIR = linear_model_NIR$coefficients[[1]]
a_radiance_R   = linear_model_R$coefficients[[2]]
b_radiance_R   = linear_model_R$coefficients[[1]]
a_radiance_RE  = linear_model_RE$coefficients[[2]]
b_radiance_RE  = linear_model_RE$coefficients[[1]]

save(a_radiance_G,
     b_radiance_G,
     a_radiance_NIR,
     b_radiance_NIR,
     a_radiance_R,
     b_radiance_R,
     a_radiance_RE,
     b_radiance_RE, file = path_coeff_radiance_irradiance)

##Clearing directories----------------------------------------------------------

if(clear_dir_after_adding_points){
  file.remove(list.files("spectralon_images",full.names = T))
  file.remove(list.files("step_1_vignetting",full.names = T))
  file.remove(list.files("step_2_distorsion",full.names = T))
  file.remove(list.files("step_3_alignment",full.names = T))
  file.remove(list.files("step_4_ECC_alignment",full.names = T))
  file.remove(list.files("step_5_homogenization",full.names = T))
  file.remove(list.files("step_6_radiance",full.names = T))
  file.remove(list.files("step_7_reflectance",full.names = T))
  file.remove(list.files("step_8_NDVI",full.names = T))
}
