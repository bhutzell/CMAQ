#!/bin/csh -f

# Convert gas-phase mechanism definition file to kpp readable file
python mech-to-kpp.py

# Merge gas- and aqueous-phase mechanisms
python mech-merge.py

# Add wet scavenging for gas phase species
python add_wdep.py

# Run kpp
kpp mchem.kpp

# Cleanup directory from unnecessary files
# File created by this script
rm Makefile_*

# Optimize for constant rate constants
python optimize_RCONST.py mchem

# Run kpp
kpp gas.kpp

# Cleanup directory from unnecessary files
# File created by this script
rm Makefile_*

# Optimize for constant rate constants
python optimize_RCONST.py gas
