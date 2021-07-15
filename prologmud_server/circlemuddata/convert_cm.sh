#!/bin/sh
# python doall.py $1

python convert_mob_files.py $1
python convert_wld_files.py $1
python convert_shp_files.py $1

#python convert_obj_files.py $1
#python convert_zon_files.py $1

