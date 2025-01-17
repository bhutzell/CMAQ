calc_tmetric
========

This Fortran program creates gridded IOAPI files with temporally blocked or stepped values of an OPERATION (see below) calculated from one or more gridded time-dependent IOAPI files.

## Run Time Environment variables used:

```
 OPERATION     operation to perform - SUM for summation, AVG for averaging, MAX for maximum, MIN for minimum,
               or RNG for RANGE
 SAMPLE_PERIOD number of time steps used to compute OPERATION, set to less then or equal to zero for total 
               number of steps. Note that output file will have time step equal to zero.
 SPECIES       list of species to output (e.g. setenv SPECIES "O3 HO HO2 NH3 HCL ANO3J ASO4J").  
               To extract all species use: setenv SPECIES "ALL"
 M3_FILE_#     List of input IOAPI file names with time-dependent values.
               The program will concatenate time steps from all input files to construct the
	       longest possible time record which can be processed. Duplicate time steps are
	       eliminated. The program will then sum or average variable values across these 
	       non-duplicate time steps.
	       The maximum number of IOAPI files is set to be one less than the global IOAPI parameter MXFILE3.
	       Since this parameter is currently set to 64 (https://www.cmascenter.org/ioapi/documentation/all_versions/html/TUTORIAL.html),
	       the maximum number of IOAPI input files is 63.
 OUTFILE       output IOAPI file name with gridded summed or averaged values
```

## Compile calc_tmetric source code:

Execute the build script to compile calc_tmetric:

```
cd $CMAQ_HOME/POST/calc_tmetric/scripts
./bldit_calc_tmetric.csh [compiler] [version] |& tee build_calc_tmetric.log
```

## Run calc_tmetric:
Edit the sample run script (run.calc_tmetric.csh), then run:
```
 ./run.calc_tmetric.csh |& tee calc_tmetric.log
```
Check the log file to ensure complete and correct execution without errors.

