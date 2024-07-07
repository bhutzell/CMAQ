#!/bin/csh -f

# Convert gas-phase mechanism definition file to kpp readable file
python mech-to-kpp.py

# Merge gas- and aqueous-phase mechanisms
python mech-merge.py

# Add wet scavenging for gas phase species
python add_wdep.py

# Run kpp
kpp mchem.kpp

# Perform file manipulation
rename .f90 .F90 *.f90
sed -i 's/  USE mchem_Util/\!  USE mchem_Util/g' mchem_Model.F90

# Cleanup directory from unnecessary files
# File created by this script
rm Makefile_*
rm mchem_Main.F90
rm mchem_Util.F90
rm mchem.map

# Optimize for constant rate constants
python optimize_RCONST.py mchem

# Run kpp
kpp gas.kpp

# Perform file manipulation
rename .f90 .F90 *.f90
sed -i 's/  USE gas_Util/\!  USE gas_Util/g' gas_Model.F90

# Cleanup directory from unnecessary files
# File created by this script
rm Makefile_*
rm gas_Main.F90
rm gas_Util.F90
rm gas.map

# Optimize for constant rate constants
python optimize_RCONST.py gas
