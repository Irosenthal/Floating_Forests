# Data Processing Pipeline for Floating Forests 2.0

## Classification Processing Pipeline to go from raw data to level 1
1. Separate classification, yes/no, and other workflow data into separate .Rds files
2. Turn classification data into processed sf object for R and postgis object (level 0)
3. Create consensus rasters for each subject individually and write out (level 0)
4. For each consensus raster, create and write out a consensus polygon (level 0)
5. Merge consensus polygons and rejected/null classifications into sf and postgis file (level 1)
