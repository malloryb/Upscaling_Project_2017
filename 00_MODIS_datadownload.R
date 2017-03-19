setwd("C:/Users/rsstudent/Upscaling_Data/")
f <- paste0("modis_download.py -I -r -t h18v03,h18v04 -f 2008-01-01 -e 2008-01-31 Test_Download/")
# Call the python script
system(f)

