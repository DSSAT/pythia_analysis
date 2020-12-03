# pythia_analysis
Post-processing scripts for DSSAT-pythia outputs

# Run the script in command line
These series of R routines help us to analyze yearly and region level(admin level) results for a country.

To run these scripts, simply use the following command

    Rscript [path to script] [path to config JSON file] [path to Pythia coinfig JSON file]
 
If path to config JSON file is not mandatory. If not provided, it will try to find config.json in the root directory of script instead.
  
If path to Pythia config JSON file is not mandatory. If not provided, it will try to find pythia_config.json in the root directory of script instead.

# DSSAT_Pythia_spatialquery.R
This script is used to analyze data at area of interest. Routine will calculate averages between years and create csv files for each management just for area of interest which will be defined as polygon shapefile in script. Then, based on the averages between years, it will also calculate weighted average between managements to analyze at country level. Output files as same as country level analysis.  Additional y to country level routine, we need to define Country name (cntry) and polygon shape file (poly). Second part will read the polygon shape file and convert it to spatial polygon to use in clipping area of interest.

This script help us to create csv files for each factor combination. The factors are defined in the config JSON file.

The parameters of this script defined in the config JSON file are shown as below,

    work_dir : The path to the working directory;
    
    output_folder_name : folder name for output files;
    
    output_base_dir : base directory path for output files;
    
    shape_file_path : path to the shape file;
    
    first_year : first year of year range;
    
    last_year : last year of year range;
    
    skip_years : a list of years which you do not want to include in the calculation;
    
    earliest_planting_date : the day of year for determine if a date is belongs the season of current year or the last year.
    
# DSSAT-Pythia_adminlvl_Techtrend.R
This R script contains 3 sub-steps, 

Techtrend 1 will create csv outputs for each year for each region and country level.

Techtrend 2, will concatenate yearly results by group of regions which defined by admin level.

Techtrend 3, will read observed (reported) yearly values from the csv files that we predefined based on the admin level and crop. Then it will analyze first degree fitting analysis (1-degree polynomial). After all the calculations, it will create graphs for each region that show observed values, trendline of observed values, simulated values and corrected simulated values(based on the trendline and deviation from mean). 
    
You can run all of them or only part of them based on given config JSON file. The trigger is the existence of each output path.

For example,

1. If output_folder_name_1 is provided, then script 1 will be executed.
    
1. If both output_folder_name_1 and output_folder_name_2 are provided, then both script 2 and 3 will be executed.
    
1. If only output_folder_name_2 is provided, then it will on run script 2.
    
1. Only run script 1 and 3 is not supported, to run 3, you have to run script 2 or provide existing result from previous runs of 2. Similar condition for 2 as well.
 
The parameters of this script defined in the config JSON file are shown as below,
 
    work_dir : The path to the working directory;
    
    output_folder_name_1 : folder name for step 1 output files;
    
    output_folder_name_2 : folder name for step 2 output files;
    
    output_folder_name_3 : folder name for step 3 output files;
    
    skip_runs : the list of run number you want to skip；
    
    output_base_dir : base directory path for output files;
    
    observed_path : path to the observation data files;
    
    GADM.country_code : 3-digit code of country;
      
    GADM.admin_level : digit number of admin level;
      
    
    first_year : first year of year range;
    
    last_year : last year of year range;
    

    first_year_3 : first year of year range of step 3;
    
    last_year_3 : last year of year range of step 3;
    
    skip_years : a list of years which you do not want to include in the calculation.
    
# DSSAT-Pythia_SPAMraster_make.R
Read data from SPAM site to create raster plot. Currently by default it is using 2010V1r0 version of SPAM data.
    
The parameters of this script defined in the config JSON file are shown as below,
    
    SPAM_url : optional, the url for downloading SPAM data;
    
    SPAM_local_dir" : The path to local SPAM data folder, if not existed or empty, then will automatically download from SPAM site;
    
    output_base_dir" : base directory path for output files;
    
    country_name" : Country name, will be used for output folder name;
    
    crop" : crop name;
    
    longitude_min" : minimum longitude;
    
    longitude_max" : maximum longitude;
    
    latitude_min" : minimum lagitude;
    
    latitude_max" : maximum lagitude.
    
