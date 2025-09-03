#### Leaf area photo processing script for lab SLA data ####
#### Created by Katherine T. Charton, edited for Brudvig lab by Riley Pizza
#### Last updated 23 May 2024
#### Used and edited by Markus Bauer 2025-09-03



library(here)
library(tidyverse)
library(magick) # pkg to assist with preprocessing and cropping of photo scans
library(LeafArea) # pkg to integrate and automate ImageJ processing through R


# list all full file pathways within the subdirectories of the working directory
# limited to .jpg files
image_list <- list.files(
  path = here("data", "raw", "photos_leaf_area"),
  full.names = TRUE, recursive = TRUE, pattern = ".jpg$"
)
  
# subset list if working with large numbers of subfolders and images
# (recommended that you don't process more than ~300 images at a time, so
# you'll need to run through step 2 multiple times before proceeding to step 3)
#  image_list <- image_list[1:300] 


# read all images in the file of file pathways generated above
image_files <- image_read(path = image_list)
  

# crop all images to specified pixel dimensions (2520 x 2775) at a specified
# starting pixel (16, 610)
# This will get rid of the plant ID tag and the entire red tape line on the
# scanner

# image_crops <- image_crop(image_files, "2520 x 2775 + 16 + 610") 


# overwrite original images with cropped images using the same initial full file
# pathways within the subdirectories of the working directory

# for (i in 1:length(image_crops)) {
#   image_write(image_crops[i], path = image_list[i])
# }


# NOTE: if running the below loop several times, do not re-run lines 31-36



# step 3: read in, binarize, and calculate area for all cropped images looping
# through subfolders
# create an empty data frame to store image areas into
image_areas <- data.frame(matrix(ncol = 3, nrow = 0))


# assign column names to the empty data frame consistent with run.ij output
colnames(image_areas) <- c("folder", "sample", "total.leaf.area") 


# list all the full folder pathways of subdirectories of the working directory
# that contain cropped images 
# (this allows us to build a loop since run.ij is not recursive)  
image_folders <- list.files(
  path = here("data", "raw"),
  full.names = TRUE, recursive = FALSE
  )


# subset list if working with large numbers of subfolders and images
# (recommended that you don't process more than ~500 images at a time,
# so you'll need to run through the loop below multiple times before proceeding
# to step 4
image_folders <- image_folders[25]


for (i in 1:length(image_folders)) {
  folder <- image_folders[i]
  
  # save the subfolder name housing each image to the data frame
  # create binary masks and calculate area for all cropped images using the
  # number of pixels for a known distance, calculated given the known dimensions
  # of the original image in pixels (2520 x 2775) and in mm
  area <- LeafArea::run.ij(
    path.imagej = "C:/Users/marku/Documents/ImageJ",
    set.directory = image_folders[i],
    set.memory = 20,
    distance.pixel = 118.15, 
    known.distance = 1, # cm (LeafArea works always with cm)
    trim.pixel = 0,
    low.size = 0.06,
    # upper.size = 5000,
    log = TRUE,
    save.image = FALSE
    )$summary
  
  # bind the subfolder name and run.ij output to the existing data frame
  image_areas <- rbind(image_areas, data.frame(folder, area)) 
}

# The resulting datarame should be the same length as image_list,
# include the file name and the sample ID as well as the total leaf area (mm2).
# Save this dataframe, merge it with the dataframe with seed mass (in mg!!!)
# and you can calculate SLA (leaf area/leaf mass)

# write_csv(
#   image_areas,
#   here("data", "processed", "data_processed_traits_leaf_area.csv")
# )
