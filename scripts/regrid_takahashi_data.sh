#Adjust path as needed

DATA_DIR="../data/input_data"
INPUT="/takahashi_et_al/1.1/data/0-data/TALK_TCO2_pCO2_GLOB_Grid_Dat.nc"

INPUT_PATH="${DATA_DIR}${INPUT}"

OUTPUT="TALK_TCO2_pCO2_GLOB_Grid_Dat_regridded.nc"
GRIDFILE="${DATA_DIR}/gridfile.txt"

cd $DATA_DIR
#Nearest neighbor interpolation
cdo -remapnn,$GRIDFILE $INPUT_PATH $OUTPUT