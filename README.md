This is the public code respository for:

David A. Koweek Expected limits on the potential for carbon dioxide removal from artificial upwelling. Frontiers of Marine Science (in press).

You can use this repository to examine and/or reproduce the results of this study.

# Getting started

All analysis and visualization for this study was done in R. If you don't already have R up and working on your machine, start by installing R. The library of scripts in this repository also rely on a series of R packages described in `session_information.txt`. You can install these packages manually, or they will install when you first initialize the workspace (see below).

## Setting up the input data directory
The directory `data/input_data` contains a number of original files used in this project. This repository also relies on a series of input data files, some of which are not stored in this directory for both copyright (they are not mine!) and memory issues. **After cloning this repository, and before being able to execute any of the scripts in this directory**, one needs to add these external data sets to `data/input_data`. File paths for each of these downloaded data sets may need to be adjusted in `scripts/project_directories.R` and/or in the relevant script that opens and utilizes the data set.

1. [**GLODAP v2.2016 Mapped Climatologies**](https://www.glodap.info/index.php/mapped-data-product/)

2. **Aqua MODIS PAR Climatologies**: These monthly climatologies were downloaded at 9km resolution and regridded to 1$^o$ x 1$^o$ using `scripts/regrid_MODIS_PAR_data.R`. The raw (9 km) resoultion data can be accessed here (note: account needed):

          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021822019212.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20022132019243.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20022442019273.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20022742019304.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023052019334.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023352019365.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030012020031.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030322020060.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030602020091.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030912020121.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20031212020152.L3m_MC_PAR_par_9km.nc
          https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20031522020182.L3m_MC_PAR_par_9km.nc

3. [**Argo Mixed Layer Depth Climatology**](http://mixedlayer.ucsd.edu/)

4. [**OceanSODA-ETHZ Gridded Monthly pCO$_2$ Climatology**](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059)

5. [**Longhurst Biogeochemical Provinces**](https://www.marineregions.org/sources.php#longhurst)


## Setting up a working data directory

After cloning the repository, create a subdirectory to store intermediate and final data sets and figures as follows: `mkdir data/working_data`. 


