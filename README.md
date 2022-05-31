This is the public code respository for:

David A. Koweek Expected limits on the potential for carbon dioxide removal from artificial upwelling. Frontiers of Marine Science (in press).

You can use this repository to examine and/or reproduce the results of this study.

# Getting started

All analysis and visualization for this study was done in R. If you don't already have R up and working on your machine, start by installing R. The library of scripts in this repository also rely on a series of R packages described in `session_information.txt`. You can install these packages manually, or they will install when you first initialize the workspace (see below).

## Setting up the input data directory
The directory `data/input_data` contains a number of original files used in this project. This repository also relies on a series of input data files, some of which are not stored in this directory for both copyright (they are not mine!) and memory issues. **After cloning this repository, and before being able to execute any of the scripts in this directory**, one needs to add these external data sets to `data/input_data`. File paths for each of these downloaded data sets may need to be adjusted in `scripts/project_directories.R` and/or in the relevant script that opens and utilizes the data set.

1. [**GLODAP v2.2016 Mapped Climatologies**](https://www.glodap.info/index.php/mapped-data-product/)

2. **Aqua MODIS PAR Climatologies**: These monthly climatologies were downloaded at 9km resolution and regridded to 1$^o$ x 1$^o$ using `source("scripts/regrid_MODIS_PAR_data.R")`. The raw (9 km) resoultion data can be accessed here (note: account needed):

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

4. [**OceanSODA-ETHZ Gridded Monthly pCO<sub>2</sub> Climatology**](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059)

5. [**Longhurst Biogeochemical Provinces**](https://www.marineregions.org/sources.php#longhurst)


## Setting up a working data directory

After cloning the repository, create the subdirectory `working_data` as follows: `mkdir data/working_data`. Many scripts save intermediate and final data products and figures to this repository.

# Reproducing Results

Once all of this preparatory work is done, it should be much easier to execute the code found in the `scripts` folder. Note that errors may still emerge until all file paths are correct for your local machine set up.

## Initializing the Workspace

Start by initializing the workspace `source("scripts/initialize_workspace.R")`. This loads (and downloads as necessary) all packages needed and loads a series of file paths. 

## Initialize the Model Grid

Once the workspace has been initialized, you can initialize the model grid using `source("scripts/initialize_upwelling_grid.R")`. This will call a series of dependencies. If the script crashes due to a missing file or incorrect file path, trace the error back until you can correct the error. Iterate as needed. You will know the script successfully initializes the model when the file `data/working_data/upwelling_grid_initial.RDS` is saved.

## Calculate CO$_2$ Gradients

After generating the file `data/working_data/upwelling_grid_initial.RDS`, the next step to complete the model simulations is to calculate the expected CO$_2$ concentration gradients between background and upwelling conditions using `source("scripts/calculate_CO2_gradients.R")`. This will produce a series of files (one for each microalgae or macroalgae C:N:P ratio) in the `working_data` directory. All files will take the name format: `[model_name]_delta_CO2_grid.RDS`. **Note: Using the machine specifications listed in `session_information.txt`, it took me approximately 36 hours to complete these calculations**.

## Calculate Mass Flows

Mass flows need to be calcalculated from the initial upwelling grid (`data/working_data/upwelling_grid_initial.RDS`). These can be calculated using `source("scripts/calculate_mass_flow.R")`. Note that this script is independent of `source("scripts/calculate_CO2_gradients.R")` and can be run before or after calculating CO$_2$ gradients. This script will produce an intermediate output file (`data/working_data/mass_flow_grid.RDS`) for later use.

## Calculating Spatially and Temporally-Resolved Carbon Dioxide Removal Potential

Using the intermediate outputs from the two steps described above, calculating the CO$_2$ gradients and mass flows, one can now calculate spatially and monthly resolved carbon dioxide removal potential from every microalgae and macroalgae C:N:P ratio using `source("scripts/calculate_CDR.R")`. Running this script produces another intermediate output file (`data/working_data/CDR_grid.RDS`). 

After this step is complete, run `source("scripts/analyze_CDR.R")`. This script will load `data/working_data/CDR_grid.RDS` and produce: a) spatially resolved maximum annual carbon dioxide removal potential estimates for every microalgae and macroalgae C:N:P ratio used in this study, and b) areally-integrated estimates of maximum annual carbon dioxide removal potential estimates for every microalgae and macroalgae C:N:P ratio in this study. Note that no intermediate output is saved from this script.

## Summary

After all of the above steps have been completed to reproduce the model results, along with their respective intermediate data products, one can simply re-produce the final results of the model by: 1) initializing the workspace using `source("scripts/initialize_workspace.R")` and then analyzing the carbon dioxde removal potential using `source("scripts/analyze_CDR.R")`.

# Reproducing Study Figures and Tables

**Figure 1**: `source("scripts/plot_pipe_configs.R")`

**Figure 2 (and Figures S1 and S2)**: `source("scripts/plot_CDR_maps.R")`. Note that `source("scripts/analyze_CDR.R")` must be executed prior to running this script (see comments at the top of the script).

**Figure 3 (and Figure S3)**: `source("scripts/plot_CDR_distributions.R")`. Note that `source("scripts/analyze_CDR.R")` must be executed prior to running this script (see comments at the top of the script).

**Figure 4**: `source("scripts/plot_global_total_CDR.R")`. Note that `source("scripts/analyze_CDR.R")` must be executed prior to running this script (see comments at the top of the script).

**Figures 5 and 6**: `source("scripts/plot_CDR_controls.R")`. Note that `source("scripts/analyze_CDR.R")` and `source("scripts/load_GLODAP_data.R")` must be executed prior to running this script (see comments at the top of the script).

**Table 2**: Can be reproduced using the the outputs `CDR_tech_potential_grid` and `CDR_geophysical_potential_grid` from `source("scripts/analyze_CDR.R")`.

# Contact Info

If you have any questions about accessing the material in this repository and/or find an error, please do not hesitate to contact me.

Happy exploring.

[David Koweek](mailto:david.koweek@oceanvisions.org)