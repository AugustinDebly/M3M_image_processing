# DJI Mavic 3M image processing
Augustin Debly

## DJI Mavic 3M

## Packages
This code is designed in R but it has to use Python functions from opencv library. Another full Python version will be designed later. The packages needed in R are exiftoolr (for images metadata reading), terra and shiny. `install_exiftool()` is a function from exiftoolr that install [exiftool](https://exiftool.org) and has to be launched once. The packages needed in Python are numpy and opencv. Make sure you have [Python](https://www.python.org/) installed on your machine.

```{r}
##R library
require(exiftoolr)
require(terra)
require(shiny)
#install_exiftool()

##Python library
system("python -m pip install numpy")
system("python -m pip install opencv-python")
```

## Geometric processing
### Step 1 : Vignetting correction

### Step 2 : Distorsion correction

### Step 3 : Alignment correction

### Step 4 : ECC alignment correction

### Step 5 : Homogenization

## Spectral processing

### Step 6 : Digital Number (DN) to radiance

### Step 7 : Radiance to reflectance
