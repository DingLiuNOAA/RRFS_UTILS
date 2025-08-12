#!/bin/sh
#

dir_root=$(pwd)

################# Hera ####################
if [[ "`grep -i "hera" /etc/hosts | head -n1`" != "" ]] ; then
    source /etc/profile.d/modules.sh
    platform=hera
################# Jet ####################
elif [[ -d /jetmon ]] ; then
    source /etc/profile.d/modules.sh
    platform=jet

################# Cheyenne ####################
elif [[ -d /glade ]] ; then
    source /etc/profile.d/modules.sh
    platform=cheyenne

################# Orion ####################
elif [[ -d /work/noaa ]] ; then  ### orion
    platform=orion

################# Gaea C6 ####################
elif [[ -d /gpfs/f6 ]] ; then ### gaea c6
    module reset
    platform=gaeaC6

################# WCOSS2 ####################
elif [[ -d /lfs ]] ; then  ### orion
    platform=wcoss2

################# Generic ####################
else
    echo -e "\nunknown machine"
    exit 9
fi

if [ ! -f $modulefile ]; then
    echo "modulefiles $modulefile does not exist"
    exit 10
fi

#source $modulefile
set -x

module purge
module use ${dir_root}/modulefiles
module load build_${platform}_intel.lua
module list 

build_root=${dir_root}/build
mkdir -p ${build_root}
cd ${build_root}

cmake .. -DCMAKE_INSTALL_PREFIX=.

make VERBOSE=1 -j 1 
make install

exit
