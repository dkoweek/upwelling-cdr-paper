#Adjust path(s) as needed
#Need cdo and nco installed

DATA_DIR="../data/input_data/Aqua_MODIS_PAR_climatology/original"
cd $DATA_DIR

GRIDFILE="../../gridfile.txt"

for i in *.nc
do
BASE="${i%.*}"
TMP="${BASE}_par_only.nc"
#Drop 'palette'
ncks -v par ${i} $TMP
#Nearest neighbor interpolation
OUTPUT="${BASE}_regrid.nc"
cdo -remapnn,$GRIDFILE $TMP ../regridded/$OUTPUT
rm *_par_only*
done