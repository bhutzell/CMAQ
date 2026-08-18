
<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)

<!-- END COMMENT -->

# 7. Model Output Files

## 7.1 Introduction
In this section, details on the routine CCTM output files are provided. All CMAQ programs produce model output files that adhere to the netCDF format.  In addition to model data output, CMAQ can optionally produce ASCII log files that contain intermediate model execution information from the various CMAQ processes and captured with respect to processor number. If the log file option is not selected by the user and the simulation is run interactively, CMAQ will write all of the log information to the screen along with the standard error, which can be captured to a text file using basic UNIX syntax. Additional output files are created when using the Process Analysis (PA), Integrated Source Apportionment Method (ISAM) and Detailed Emissions Scaling, Isolation and Diagnostics Module (DESID) options.  The files associated with these options are discussed in [Chapter 9](CMAQ_UG_ch09_process_analysis.md), [Chapter 11](CMAQ_UG_ch11_ISAM.md), and [Appendix B](Appendix/CMAQ_UG_appendixB_emissions_control.md), respectively.

<a id=Output_Table></a>
<a id=Table7-1></a>

**Table 7-1. CMAQ Output files**

|**File Name<sup>1</sup>**|**File Type**|**Time-Dependence<sup>2</sup>**|**Spatial Dimensions<sup>3</sup>** |
|----------------------------|------|----|-----------------------------------|
|**Flexible Standard<sup>4</sup>**| | | |
|[CCTM_ELMO1_[fname1]](#ELMO)<a id=ELMO_t></a>|GRDDED3|Hourly Instantaneous|XYZ'
|[CCTM_ELMO2_[fname2]](#ELMO) <a id=ELMO_t></a>|GRDDED3|Hourly Averaged and Cumulative|XYZ'
|**Fixed Standard**| | | |
|[Output Log](#cmaq_output_log) <a id=cmaq_output_log_t></a>|ASCII|n/a|n/a
|[CCTM_CONC](#conc)<a id=conc_t></a>|GRDDED3|Hourly Instantaneous|XYZ'
|[CCTM_ACONC](#aconc) <a id=aconc_t></a>|GRDDED3|Hourly Averaged|XYZ'
|[CCTM_DRYDEP](#drydep) <a id=drydep_t></a>|GRDDED3|Hourly Cumulative|XY
|[CCTM_WETDEP1](#wetdep) <a id=wetdep_t></a>|GRDDED3|Hourly Cumulative|XY
|**Restart**| | | |
|[CCTM_CGRID](#cgrid) <a id=cgrid_t></a>|GRDDED3|Hourly Instantaneous|XYZ
|[CCTM_MEDIA_CONC](#media)<a id=media_conc_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_BSOILOUT](#soilout) <a id=soilout_t></a>|GRDDED3|n/a (see detailed file description below)|XY
|[CCTM_MSOILOUT](#soilout) <a id=soilout_t></a>|GRDDED3|n/a (see detailed file description below)|XY
|[CCTM_BDSNPOUT](#bdsnpout) <a id=bdsnpout_t></a>|GRDDED3|n/a (see detailed file description below)|XY
|**Diagnostic and Advanced**| | | |
|[FLOOR](#floor)<sup>5</sup> <a id=floor_t></a>|ASCII|Hourly|XYZ
|[CCTM_B3GTS_S](#b3gts) <a id=b3gts_t></a>|GRDDED3|Hourly Instantaneous| XY
|[CCTM_BUDGET](#budget) <a id=budget_t></a>|ASCII|Hourly Instantaneous| Domain-Wide
|[CCTM_DEPV](#depv) <a id=depv_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_DUSTEMIS](#dust) <a id=dust_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_DESIDX](#desid) <a id=desid_t></a>|GRDDED3|Hourly Instantaneous|XYZ
|[CCTM_DEPVMOS](#depv_mos) <a id=depv_mos_t></a>|GRDDED3|Hourly Instantaneous|XYW
|[CCTM_DDEP_MOS](#dry_dep_mos) <a id=dry_dep_mos_t></a>|GRDDED3|Hourly Cumulative|XYW
|[CCTM_LTNGHRLY](#ltngdiag1) <a id=ltngdiag1_t></a>|GRDDED3|Hourly Instantaneous|XYZ
|[CCTM_LTNGCOL](#ltngdiag2) <a id=ltngdiag2_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_PHOTDIAG1](#ctm_rj1) <a id=ctm_rj1_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_PHOTDIAG2](#ctm_rj2) <a id=ctm_rj2_t></a>|GRDDED3|Hourly Instantaneous|XYZ'
|[CCTM_PHOTDIAG3](#ctm_rj3) <a id=ctm_rj3_t></a>|GRDDED3|Hourly Instantaneous|XYZ'
|[CCTM_SSEMIS](#ssemis) <a id=ssemis_t></a>|GRDDED3|Hourly Instantaneous|XY
|[CCTM_WETDEP2](#wetdep2) <a id=wetdep2_t></a>|GRDDED3|Hourly Cumulative|XY
|[CCTM_VEXT](#vext) <a id=vext_t></a>|GRDDED3|Hourly Instantaneous|WZ

<sup>**1**</sup>By default, output files are named CCTM_XXX_${CTM_APPL}.nc where XXX is the file identifier and ${CTM_APPL} is a user defined string that identifies the model run.   
<sup>**2**</sup>While "Hourly" is indicated, users may define a different time step (e.g., 30 minutes) for model output by changing the TSTEP variable in the runscript. Hourly Instantaneous represents the model value at the exact model output time step.  Hourly Averaged values represent the average model values for the 60 minutes beginning with the model output time step.  Hourly Cumulative represents the cumulative (summed) model values time-stamped to the previous output time step.  
<sup>**3**</sup>X is the dimension along the x-axis, Y is the dimension along the y-axis, Z is the vertical dimension, Z' is the user pre-defined size of the vertical dimension controlled by the environment variables CONC_BLEV_ELEV, ACONC_BLEV_ELEV, AELMO_BLEV_ELEV, and NLAYS_PHOTDIAG (range from 1 to all layers) and W is a non-layer dimension, e.g. number of LU fractions, number of sites for vertical extraction.    
<sup>**4**</sup>The number and qualities of ELMO output files are entirely customizable by the user. Two examples are provided here to demonstrate that ELMO files currently must be either fully instantaneous or fully aggregated (i.e. average concentrations and cumulative deposition). The variable fname is user-specified for each file.  
<sup>**5**</sup>A special ASCII output file, FLOOR_xxx with xxx being the processor number, contains information when a simulation results in negative concentrations. 

## 7.2 CCTM Output Files

Each file is described in its own section and example output variables (with names, units, and descriptions) for each type of file are included in [Table 7-3](#variables_table). The users outputs will depend on what options are used for the simulation. [Table 7-3](#variables_table) uses cb6r5_ae7_aq with typical runtime output options.
  
<a id=ELMO></a>

**CCTM_ELMO: hourly ELMO output files**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ELMO_t)
<!-- END COMMENT -->

These optional 2-D or 3-D CCTM output files contains instantaneous or aggregated data for user-specified variables including 
concentrations that would appear on CONC and ACONC files, deposition fluxes that would appear on WETDEP or DRYDEP, and aggregated 
variables like total particulate mass (PM<sub>2.5</sub> and PM<sub>10</sub>). 
Diagnostic parameters that were found on the PMDIAG file in previous CMAQ versions are also available for output on ELMO files. 
Thease include particle geometric mean diameters, geometric standard deviations, bulk densities, 2nd moments and 3rd moments for 
the lognormal modes. 
One can also output the fraction of each mode that contributes to PM<sub>1</sub>, PM<sub>2.5</sub>, and PM<sub>10</sub> or the 
AMS (aerosol mass spectrometer) transmission factor for each mode. 
Many diagnostics relating to meteorology, heterogenous chemistry, and optical metrics (e.g. AOD, NO2_COLUMN) are provided. ELMOv2 
further includes support for ISAM and DDM variables. 

Units for all variables are specified in the output file. Verbose definitions of variables and their attributes are written to each CCTM_LOG ascii logfile.

The namelist input file CMAQ_Control.nml allows users to declare how many CCTM_ELMO files to create. 
See [Appendix F (ELMO Output):](Appendix/CMAQ_UG_appendixF_elmo_output.md) for more details.
 
Some output files created by the CCTM have been considered standard output in the past as these contain hourly concentration and deposition values and information to document the run. Options for these files are controlled by their corresponding environment variable in the CCTM RunScript (e.g. run_cctm.csh).
<a id=cmaq_output_log></a>

**CMAQ output log**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#cmaq_output_log_t)
<!-- END COMMENT -->
All of the CMAQ processors generate standard output and standard error during execution. When you run the CMAQ executable interactively, diagnostic output information can be captured to a log file using a UNIX redirect command:

```
run.cctm >& tee cctm.log
```


The LOGFILE environment variable allows users to specify the name of a log file for capturing the standard output from the program. If this variable is not set, the standard output is written to the terminal and can be captured using the UNIX redirect command (“>”), as shown in the example above.

<a id=conc></a>

**CCTM_CONC: CCTM hourly instantaneous concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#conc_t)
<!-- END COMMENT -->

The 2-D or 3-D CCTM hourly concentration file (CONC) contains instantaneous gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each model output time step. The number and type of species contained in the CONC files depends on the chemical mechanism and aerosol model configurations that are selected when the CCTM is compiled. The [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories list the modeled species, and contain a column that specifies which species are written to the CONC files (e.g. [AE_cb6r3_ae7_aq.nml][link_7_nml]). The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the CONC file by editing the CONC column in the NameList file(s) to reduce the number of species that are written to, and thus the size of the CONC file. Users can also specify the output species list (including temperature, pressure & relative humidity) by modifying the environment variable CONC_SPCS in the RunScript which overrides the setting of the CONC column in the NameList file(s). By default, concentrations for all model layers are output to the CONC file.  Users may specify the layers to output using the CONC_BLEV_ELEV environment variable in the RunScript where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number.

Example outputs from a cb6r5_ae7_aq simulation with all species output are shown in Table . Although all species were output, only select gases and aerosols are shown

<a id=aconc></a>

**CCTM_ACONC: hourly average concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#aconc_t)
<!-- END COMMENT -->

The 2-D or 3-D CCTM integral average concentration file contains average model species concentrations for each model hour, as opposed to instantaneous concentrations at the end of each output time step. The species written to the ACONC file are set by the user in the CCTM RunScript using the environment variable AVG_CONC_SPCS. The model layers for which hourly average concentrations are calculated are also set in the CCTM RunScript using the environment variable ACONC_BLEV_ELEV, where BLEV corresponds to the bottom layer number and ELEV corresponds to the top layer number. An example setting for the ACONC_BLEV_ELEV variable is “1 6”, which defines layers 1 through 6 as the vertical extent for which hourly average concentrations are calculated and written to the ACONC file.

<a id=drydep></a>

**CCTM_DRYDEP: hourly cumulative dry deposition file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#drydep_t)
<!-- END COMMENT -->

The 2-D CCTM dry deposition file contains cumulative hourly dry deposition fluxes (kg hectare<sup>-1</sup>) for selected model species.  CCTM calculates dry deposition for all of the species listed in the dry deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the dry deposition file by editing the DDEP column in the NameList file(s).

##### NH<sub>3</sub> flux components in CCTM_DRYDEP
CMAQ v5.3 and later contains two build-time options for calculating dry deposition/surface exchange: M3DRY and STAGE. (See [Section 6.8 ](CMAQ_UG_ch06_model_configuration_options.md#68-dry-depositionair-surface-exchange) for further information).  Both M3DRY and STAGE support modeling ammonia bidirectional surface flux.  The definition of the NH3 flux components in the CCTM_DRYDEP file will depend on whether or not bidirectional NH<sub>3</sub> flux option has been enabled (a run-time option controlled by setting CTM_ABFLUX to Y or N).  When the model is run without the bidirectional NH<sub>3</sub> flux option enabled (CTM_ABFLUX set to N), the variable NH3 in the CCTM_DRYDEP file represents the unidirectional ammonia dry deposition flux in both STAGE and M3DRY.

When the model is run with CTM_ABFLUX set to Y, the CCTM_DRYDEP file will contain additional NH3 flux components.  The variable names and definitions are defined in Table 7-2.  Note that these variables definitions may not agree with the definitions used in CMAQ versions prior to version 5.3.2.    

<a id=Table7-2></a>
**Table 7-2. NH3 Flux components in CCTM_DRYDEP Output files when ammonia bidirectional surface flux is enabled**

|**Variable Name**|**Variable Description**|
|:----:|:----------------------------:|
|NH3|Downward Deposition Flux (always positive)  |	
|NH3_Emis|Upward Emissions Flux (always positive)	   |  
|NH3_Flux|Net Flux (positive if downward and negative if upward)  | 


<a id=wetdep></a>

**CCTM_WETDEP1: hourly cumulative wet deposition file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#wetdep_t)
<!-- END COMMENT -->

The 2-D CCTM wet deposition file contains cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s).

## 7.3 Restart Files

There are several files created by the CCTM that are used to enable a restart of the run for any specific day.  The files contain values for parameters at the end of the day which are used to initialize the values for the start of calculations for the next day.

<a id=cgrid></a>

**CCTM_CGRID: gridded concentration restart file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#cgrid_t)
<!-- END COMMENT -->

The 3-D CCTM ending concentration file contains gas-phase species mixing ratios (ppmV) and aerosol species concentrations (µg m<sup>-3</sup>) at the end of each simulation period. The number and types of species contained in the output CGRID files depend on the chemical mechanism and aerosol model configurations that are selected when CCTM is compiled. This file can be used to initialize CCTM from a simulation period that the model completed. For example, if the CCTM is configuring to produce daily output files, a CGRID file will be written out at the end of each simulation day. These concentrations then become the initial conditions for the next simulation period.

<a id=media></a>

**CCTM_MEDIA_CONC: Bidirectional modeling media concentration file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#media_conc_t)
<!-- END COMMENT -->

This 2-D CCTM file contains the soil NH<sub>4</sub><sup>+</sup> and pH concentrations and/or the soil, vegetation and water Hg concentrations. This file is only created when the CTM_ABFLUX environment variable or the CTM_HGBIDI variable in the RunScript is set to Y (Default is N) for either the M3Dry or STAGE dry deposition option. For STAGE, it is used to initialize the next day of the model simulation for either the CTM_ABFLUX == Y or CTM_HGBIDI == Y case. For M3Dry, it is only used to initialize the next day of the model simulation for the CTM_HGBDIDI == Y case while the the soil NH<sub>4</sub><sup>+</sup> and pH concentrations written to this file for the CTM_ABFLUX == Y case are purely diagnostic. As described in [Chapter 6](CMAQ_UG_ch06_model_configuration_options.md#6.8.1_Dry_Depm3dry), M3Dry relies exclusively on input files generated by EPIC to derive the soil compensation concentration for the bidirectional NH<sub>3</sub><sup>+</sup> flux calculation.  

<a id=soilout></a>

**CCTM_BSOILOUT and CCTM_MSOILOUT**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#soilout_t)
<!-- END COMMENT -->

*BEIS*
 
The 2-D "soilout" file contains hourly total rainfall information for subsequent use by the CCTM in-line biogenics module. It is written out at the end of each simulation day and is only created if the CTM_BIOGEMIS_BE environment variable in the RunScript is set to Y (Default is N). The file name is defined in the runscript by setting the environmental variable BEIS_SOILOUT. With the exception of the first day of the simulation when the environment variable NEW_START is set to TRUE, the previous day's rainfall information contained in the file is used in the calculation of soil NO emissions by the CCTM in-line biogenics module. This is accomplished by setting the BEIS_SOILINP environment variable in the RunScript for a given day to the CCTM_BSOILOUT file created at the end of the previous day's simulation. Note that even though this file contains 24 hourly gridded rainfall fields, it has a time-independent file structure and stores these 24 values as 24 separate time-independent variables (RAINFALL01, ... RAINFALL24). However, while the structure of the file is time-independent, each day's CCTM_BSOILOUT file is unique due to the daily variations in meteorology. Therefore, care must be taken to ensure that the BEIS_SOILINP file specified for a given day is indeed the CCTM_BSOILOUT file for the previous day rather than that for a different day.  

 *MEGAN*
 
As with BEIS, the file set by the environmental variable MEGAN_SOILOUT contains rainfall information that is needed for the calculation of soil NO emissions when CTM_BIOGEMIS_MG is set to Y (Default is N). When enabling in-line MEGAN this file will also contains LAI, temperature, and radiation information that is used to calculate biogenic emissions. The input file from the previous day is identified by the environmental variable MEGAN_SOILINP in the run script.
 
**CCTM_BDSNPOUT**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#bdsnpout_t)
<!-- END COMMENT -->
 
This file is required when setting both CTM_BIOGEMIS_MG to Y and BDSNP_MEGAN to Y, since the BDSNP soil NO model requires information about the previous day's meteorology and nitrogen deposition reservoir. The output file is created at the end of the simulation day and its name is defined by setting the environmental variable BDSNPOUT. The input file for the previous day is defined by setting the environmental variable BDSNPINP. 

## 7.4 Diagnostic and Advanced CMAQ Output Files

Along with the standard output files detailed in the previous section, CCTM can be configured to output several auxiliary files for diagnostic model purposes. Each option is controlled by its corresponding environment variable in the CCTM RunScript (e.g. run_cctm.csh). For logical values, TRUE/T is equivalent to Y and FALSE/F is equivalent to N.

Note that I/O APIv3.2 supports up to MXFILE3=64 open files, each with up to MXVARS3=2048.  Turning on all of the diagnostic and advanced CMAQ output files can exceed this upper limit of open files, leading to a model crash. To avoid this issue, users may use I/O API version 3.2 "large" that increases MXFILE3 to 512 and MXVARS3 to 16384. This version is available as a zip file from the following address:

https://www.cmascenter.org/ioapi/download/ioapi-3.2-large.tar.gz

Installation instructions for I/O API v5.3-large are provided in README.txt in the .tar.gz file. 

<a id=floor></a>

**FLOOR: concentration-reset diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#floor_t)
<!-- END COMMENT -->

This optional ASCII file contains specific gridcells/timesteps in which species with negative concentrations are reset to zero. The location and name of the file is set by the FLOOR_FILE environment variable.

<a id=budget></a>

**CCTM_BUDGET: Budget Tool Output File**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#budget_t)
<!-- END COMMENT -->

This optional ascii file outputs domain-wide changes for user-specified species every output time step in units of kg for gases and aerosols, number for particle number, and m<sup>2</sup> for particle surface area. See [Chapter 9 (Process Analysis and Budget):](CMAQ_UG_ch09_process_analysis.md) for a description of the Budget Tool methods, interface, and potential applications.

The destination folder of this output file must be specified with the $OUTDIR environment variable in the RunScript. If this variable is not specified, the destination will be the root directory.    

<a id=b3gts></a>

**CCTM_B3GTS_S: biogenic emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#b3gts_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains total hourly biogenic emissions in mass units calculated in-line by the CCTM when the CTM_BIOGEMIS environment variable is set to Y. This file is only created if the B3GTS_DIAG environment variable in the RunScript is set to Y (Default is Y) and only if BEIS is the selected biogenic emisisons model. 

<a id=depv></a>

**CCTM_DEPV: inline deposition diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#depv_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains the deposition velocity (m/s) for each chemical species calculated for the final time step for the hour. CCTM calculates the deposition velocity for all of the species listed in the deposition velocity column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the deposition velocity file by editing the DDEP column in the NameList file(s). This file is only created if the CTM_DEPV_FILE environment variable in the RunScript is set to Y (Default is N). 


<a id=dust></a>

**CCTM_DUSTEMIS: dust emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#dust_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains dust emissions in mass units calculated in-line by the CCTM when the CTM_WB_DUST environment variable is set to Y. This file is only created if the CTM_DUSTEM_DIAG environment variable in the RunScript is set to Y (Default is N).
 
<a id=desid></a>

**CCTM_DESIDX: DESID diagnostic output file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#desid_t)
<!-- END COMMENT -->

This optional 2-D or 3-D CCTM hourly output file contains emission rates equal to those calculated by DESID after all user-specified rules have been implemented and input emissions data applied. 
Use the CMAQ_Control_DESID.nml file to specify the number and contents of these emissions diagnostic files. 
They may contain information about one stream or many, and the variable list for each is customizable. 
See [Appendix B (Emissions Control):](Appendix/CMAQ_UG_appendixB_emissions_control.md) for more information. 

<a id=depv_mos></a>

**CCTM_DEPVMOS: land use specific deposition velocity file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#depv_mos_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the deposition velocity (m s<sup>-1</sup>) for the final time step of the hour for each land use type within a grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers".  This file is only created if the DepMod environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_MOSAIC environment variable in the RunScript is set to Y (Default is N).


<a id=dry_dep_mos></a>

**CCTM_DDMOS: land use specific deposition flux file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#dry_dep_mos_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the total deposition (kg hectare<sup>-1</sup>) for the hour for each land use type within each grid cell. This output file is structured with the land use category being the 3rd dimension (i.e. equivalent to the layers in a concentration file). So, for model runs using the NLCD land use category system, the files will have 40 "layers". This file is only created if the ModDepv environment variable in the BuildScript is set to stage (rather than m3dry) and if the CTM_MOSAIC environment variable in the RunScript is set to Y (Default is N).

<a id=ltngdiag1></a>

**CCTM_LTNGHRLY: hourly lightning emissions file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ltngdiag1_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains hourly lightning NO emissions (mol/s) calculated in-line by the CCTM when setting the CTM_LTNG_NO environment variable to Y. This file is only created if the CTM_LTNGDIAG_1 environment variable in the RunScript is set to Y (Default is N).

<a id=ltngdiag2></a>

**CCTM_LTNGCOL: hourly column total lightning emissions**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ltngdiag2_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains hourly column-total lightning NO emissions (mol/s) calculated in-line by the CCTM when setting the CTM_LTNG_NO environment variable to Y. This file is only created if the CTM_LTNGDIAG_2 environment variable in the RunScript is set to Y (Default is N).

<a id=ctm_rj1></a>

**CCTM_PHOTDIAG1: In-line photolysis inputs and outputs - summary file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj1_t)
<!-- END COMMENT -->

This optional 2-D CCTM file contains general summary information for the photolysis calculation including the surface albedo, 
select photolysis rates and flux values.  This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=ctm_rj2></a>

**CCTM_PHOTDIAG2_2: In-line photolysis output – gridded photolysis rates**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj2_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains the photolysis rates calculated in-line by the CCTM.  The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to T (Default is N).

<a id=ctm_rj3></a>

**CCTM_PHOTDIAG3: In-line photolysis inputs and outputs – detailed**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ctm_rj3_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains detailed inputs and results from the photolysis rate calculation done in-line by the CCTM. The number of layers is set by the  NLAYS_PHOTDIAG environment variable (Default is all layers). The number of wavelengths included in the file is set by the NWAVE_PHOTDIAG environment variable (Default is all wavelengths). This file is only created if the CTM_PHOTDIAG environment variable in the RunScript is set to T (Default is N).

<a id=ssemis></a>

**CCTM_SSEMIS: Sea salt emissions diagnostic file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#ssemis_t)
<!-- END COMMENT -->

This optional 2-D CCTM hourly output file contains calculated sea salt emissions (g/s). This file is only created if the CTM_SSEMDIAG environment variable in the RunScript is set to Y (Default is N).

<a id=wetdep2></a>

**CCTM_WETDEP2: CCTM cloud diagnostics file**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#wetdep2_t)
<!-- END COMMENT -->

In CMAQ, wet deposition is calculated separately for resolved (grid-scale) clouds and for convective (subgrid) clouds. The WETDEP1 file contains the total wet deposition, i.e., the sum of both resolved-scale and subgrid-scale deposition. The WETDEP2 file contains only subgrid-scale deposition, plus some cloud diagnostic variables. The 2-D CCTM wet deposition file (WETDEP2) includes cumulative hourly wet deposition fluxes (kg hectare<sup>-1</sup>) for selected model species. CCTM calculates wet deposition for all of the species listed in the wet deposition column of the [Species NameLists files](CMAQ_UG_ch04_model_inputs.md#matrix_nml) files within the mechanism directories. The GC_*mechname*.nml file lists the gas-phase species, the AE_*mechname*.nml file lists the aerosol species, and the NR_*mechname*.nml lists the nonreactive (inert) species. Species can be removed from the wet deposition file by editing the WDEP column in the NameList file(s). This file is only created if the CLD_DIAG environment variable in the RunScript is set to Y (Default is N).

<a id=vext></a>

**CCTM_VEXT: file of vertical profiles of concentration at selected sites**
<!-- BEGIN COMMENT -->
[Return to Table 7-1](#vext_t)
<!-- END COMMENT -->

This optional 3-D CCTM file contains vertical profiles of the concentration of multiple chemical species for latitude / longitude coordinates specified in the VERTEXT_COORD_PATH file. The species written to this output file are identical to those written to the 3D CONC file which in turn are controlled either by the setting of CONC_SPCS in the RunScript or the last column in the GC, AE, NR, and TR namelist files. There is one row for each location specified. The coordinates for each location are echoed in the file metadata in the "history" field. This file is only created if the VERTEXT environment variable in the RunScript is set to Y (Default is N).



<a id=variables_table></a>
<a id=Table7-3></a>

**Table 7-3. Example CMAQ Output Variables from a cb6r5_ae7_aq simulation.**

|**Variable Name**|**Description**|**Units**|**Dimensions**|**File**|**Default**|
|--------|---------------|--------------|----------|------------|----------|
| PM_NUM  | Total Particle Number | N m-3 | XYT | CCTM_ELMO1 | True |
| PMF_NUM  | Fine-Mode Particle Number | N m-3 | XYT | CCTM_ELMO1 | True |
| PMC_NUM  | Coarse-Mode Particle Number | N m-3 | XYT | CCTM_ELMO1 | True |
| PMF_MASS  | Fine Particle Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_FRM  | Federal Reference Method PMIJ | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_MASS  | Coarse Particle Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_OC  | Fine-Mode Organic Carbon | ugC m-3 | XYT | CCTM_ELMO1 | True |
| PMF_EC  | Fine-Mode Black Carbon | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_NA  | Fine-Mode Sodium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_CL  | Fine-Mode Chloride | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_SO4  | Fine-Mode Sulfate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_NO3  | Fine-Mode Nitrate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_NH4  | Fine-Mode Ammonium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25  | Bulk PM2.5 Concentration | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_FRM  | Federal Reference Method PM2.5 | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_SO4  | PM2.5 Sulfate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_NO3  | PM2.5 Nitrate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_NH4  | PM2.5 Ammonium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_OC  | PM2.5 Organic Carbon | ugC m-3 | XYT | CCTM_ELMO1 | True |
| PM25_EC  | PM2.5 Black Carbon | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_FE  | Fine-Mode Iron | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_AL  | Fine-Mode Aluminum | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_SI  | Fine-Mode Silicon | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_TI  | Fine-Mode Titanium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_CA  | Fine-Mode Calcium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_MG  | Fine-Mode Magnesium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_K  | Fine-Mode Potassium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_MN  | Fine-Mode Manganese | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_SOILIMPV  | Recon of Soil PM from correlations at IMPROVE sites | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_UN_IMPV1  | Unspeciated PM including non-carbon organic mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_NCOM  | Fine-Mode Non-Carbon Organic Matter in POA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_UN_IMPV2  | Unspeciated PM excluding non-carbon organic mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| TNO3  | Total Nitrate only Including Inorganics | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM_MASS  | Total Particle Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| TSURF  | 2-meter Surface Temperature | K | XYT | CCTM_ELMO1 | True |
| AOD_550  | Aerosol Optical Depth at 550 nm -Angstrom interp | 1 | XYT | CCTM_ELMO1 | True |
| FPM01AIT  | PM01 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM01ACC  | PM01 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM01COR  | PM01 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM1AIT  | PM1 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM1ACC  | PM1 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM1COR  | PM1 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25AIT  | PM2.5 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25ACC  | PM2.5 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25COR  | PM2.5 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM10AIT  | PM10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM10ACC  | PM10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM10COR  | PM10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25TO10AIT  | PM2.5-10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25TO10ACC  | PM2.5-10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FPM25TO10COR  | PM2.5-10 fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FAMSAIT  | AMS Transmission Fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FAMSACC  | AMS Transmission Fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| FAMSCOR  | AMS Transmission Fraction of mode | 1 | XYT | CCTM_ELMO1 | True |
| PM01  | Bulk PM0.1 Concentration | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM1  | Bulk PM1.0 Concentration | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM10  | Bulk PM10.0 Concentration | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10  | Coarse-Mode Total | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMU_MASS  | Ultrafine Particle Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS  | Bulk PM Concentration in AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAIT_MASS  | Aitken Mode Total PM | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMACC_MASS  | Accumulation Mode Total PM | ug m-3 | XYT | CCTM_ELMO1 | True |
| TA  | Temperature | K | XYT | CCTM_ELMO1 | True |
| PRES  | Pressure | Pa | XYT | CCTM_ELMO1 | True |
| RH  | Relative humidity | 1 | XYT | CCTM_ELMO1 | True |
| GAMMA_N2O5  | Fine Mode N2O5 Heterogeneous rxn probability | 1 | XYT | CCTM_ELMO1 | True |
| GAMMA_N2O5K  | Coarse Mode N2O5 Heterogeneous rxn probability | 1 | XYT | CCTM_ELMO1 | True |
| YIELD_CLNO2  | Fine Mode CLNO2 Heterogeneous reaction yield | 1 | XYT | CCTM_ELMO1 | True |
| YIELD_CLNO2K  | Coarse Mode CLNO2 Heterogeneous reaction yield | 1 | XYT | CCTM_ELMO1 | True |
| GAMMA_IEPOX  | IEPOX heterogeneous uptake coefficient | 1 | XYT | CCTM_ELMO1 | True |
| K_IEPOX  | IEPOX 1st order particle phase reaction rate const | s-1 | XYT | CCTM_ELMO1 | True |
| GAMMA_IMAE  | IMAE+HMML heterogeneous uptake coefficient | 1 | XYT | CCTM_ELMO1 | True |
| PMF_OA  | Fine-Mode Organic Aerosol | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_H2O  | Fine-Mode Particle Water | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_HP  | Fine-Mode H+ Ion | umol m-3 | XYT | CCTM_ELMO1 | True |
| PMF_HPMOLAL  | Fine-Mode Concentration of H+ in Particle Water | mol L-1 | XYT | CCTM_ELMO1 | True |
| PMF_PH  | Fine-Mode pH | 1 | XYT | CCTM_ELMO1 | True |
| PMC_SO4  | Coarse-Mode Sulfate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_NO3  | Coarse-Mode Nitrate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_NH4  | Coarse-Mode Ammonium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_NA  | Recon Crs Sodium (.8373*ASEACAT+.0626*ASOIL+.0023*ACORS) | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_MG  | Recon Crs Magnesium (.0997*ASEACAT+.0170*ASOIL+.0032*ACORS) | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_K  | Recon Crs Potasium (.0310*ASEACAT+.0242*ASOIL+.0176*ACORS) | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMC_CA  | Recon Crs Calcium (.0320*ASEACAT+.0838*ASOIL+.0562*ACORS) | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_CL  | PM2.5 Chloride | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_NA  | PM2.5 Sodium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_OA  | PM2.5 Organic Aerosol | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_MG  | PM2.5 Magnesium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_K  | PM2.5 Potassium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_CA  | PM2.5 Calcium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_OTHER  | PM2.5 Other Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_FE  | PM2.5 Iron | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_SI  | PM2.5 Silicon | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_TI  | PM2.5 Titanium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_MN  | PM2.5 Manganese | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_AL  | PM2.5 Aluminum | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_SOIL  | PM2.5 Recon Soil (Fine) and Explicit (Coarse) | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_UNSP1  | PM2.5 Unsp. Coarse Mass for the IMPROVE method | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_UNSPCRS  | PM2.5 Unspeciated Coarse Mass | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25_HP  | PM2.5 H+ Ion | ug m-3 | XYT | CCTM_ELMO1 | True |
| DRY_DGAIT  | Mode mean diameter (dry) | um | XYT | CCTM_ELMO1 | True |
| DRY_DGACC  | Mode mean diameter (dry) | um | XYT | CCTM_ELMO1 | True |
| DRY_DGCOR  | Mode mean diameter (dry) | um | XYT | CCTM_ELMO1 | True |
| WET_DGAIT  | Mode mean diameter (wet) | um | XYT | CCTM_ELMO1 | True |
| WET_DGACC  | Mode mean diameter (wet) | um | XYT | CCTM_ELMO1 | True |
| WET_DGCOR  | Mode mean diameter (wet) | um | XYT | CCTM_ELMO1 | True |
| STDEVAIT  | Mode standard deviation (dry and wet) |  | XYT | CCTM_ELMO1 | True |
| STDEVACC  | Mode standard deviation (dry and wet) |  | XYT | CCTM_ELMO1 | True |
| STDEVCOR  | Mode standard deviation (dry and wet) |  | XYT | CCTM_ELMO1 | True |
| DRY_M3AIT  | Mode 3rd moment (dry) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| DRY_M3ACC  | Mode 3rd moment (dry) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| DRY_M3COR  | Mode 3rd moment (dry) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M3AIT  | Mode 3rd moment (wet) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M3ACC  | Mode 3rd moment (wet) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M3COR  | Mode 3rd moment (wet) | m3 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M2AIT  | Mode 2nd moment (wet) | m2 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M2ACC  | Mode 2nd moment (wet) | m2 m-3 | XYT | CCTM_ELMO1 | True |
| WET_M2COR  | Mode 2nd moment (wet) | m2 m-3 | XYT | CCTM_ELMO1 | True |
| DRY_DENSAIT  | Bulk Density of Particles excluding particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| DRY_DENSACC  | Bulk Density of Particles excluding particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| DRY_DENSCOR  | Bulk Density of Particles excluding particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| WET_DENSAIT  | Bulk Density of Particles including particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| WET_DENSACC  | Bulk Density of Particles including particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| WET_DENSCOR  | Bulk Density of Particles including particle water | kg m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS_CL  | Chloride Collected by the AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS_NH4  | Ammonium Collected by the AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS_NO3  | Nitrate Collected by the AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS_OA  | Organic Aerosol Collected by the AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMAMS_SO4  | Sulfate Collected by the AMS | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10_SO4  | Coarse-Mode Sulfate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10_NO3  | Coarse-Mode Nitrate | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10_NH4  | Coarse-Mode Ammonium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10_CL  | Coarse-Mode Chloride | ug m-3 | XYT | CCTM_ELMO1 | True |
| PM25TO10_NA  | Coarse-Mode Sodium | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_POC  | Fine-Mode Primary Organic Carbon | ugC m-3 | XYT | CCTM_ELMO1 | True |
| PMF_SOC  | Fine-Mode Secondary Organic Carbon | ugC m-3 | XYT | CCTM_ELMO1 | True |
| PMF_POA  | Fine-Mode Primary Organic Aerosol | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_SOA  | Fine-Mode Secondary Organic Aerosol | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_OMOC  | Fine-Mode OM/OC | 1 | XYT | CCTM_ELMO1 | True |
| PMF_OTOC  | Fine-Mode O:C | 1 | XYT | CCTM_ELMO1 | True |
| PMF_ASOA  | Fine-Mode Anthropogenic-VOC Derived OA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_BSOA  | Fine-Mode Biogenic-VOC Derived OA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_CLDGLY  | Fine-Mode SOA from glyoxal and methylglyoxal | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_ISOPSOA  | Fine-Mode Isoprene SOA excl IEPOX SOA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_IEPOXSOA  | Fine-Mode IEPOX SOA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_MTNSOA  | Fine-Mode Monoterpene Nitrate SOA | ug m-3 | XYT | CCTM_ELMO1 | True |
| PMF_MTSOA  | Fine-Mode Monoterpene SOA excl Nitrates | ug m-3 | XYT | CCTM_ELMO1 | True |
| N10  | Total Particle Number greater than 10 nm | N m-3 | XYT | CCTM_ELMO1 | True |
| N20  | Total Particle Number greater than 20 nm | N m-3 | XYT | CCTM_ELMO1 | True |
| N40  | Total Particle Number greater than 40 nm | N m-3 | XYT | CCTM_ELMO1 | True |
| N100  | Total Particle Number greater than 100 nm | N m-3 | XYT | CCTM_ELMO1 | True |
| NO2  | Instantaneous Molar Mixing Ratio NO2 | ppmV | XYZT | CCTM_CONC | True |
| NO  | Instantaneous Molar Mixing Ratio NO | ppmV | XYZT | CCTM_CONC | True |
| O  | Instantaneous Molar Mixing Ratio O | ppmV | XYZT | CCTM_CONC | True |
| O3  | Instantaneous Molar Mixing Ratio O3 | ppmV | XYZT | CCTM_CONC | True |
| NO3  | Instantaneous Molar Mixing Ratio NO3 | ppmV | XYZT | CCTM_CONC | True |
| O1D  | Instantaneous Molar Mixing Ratio O1D | ppmV | XYZT | CCTM_CONC | True |
| OH  | Instantaneous Molar Mixing Ratio OH | ppmV | XYZT | CCTM_CONC | True |
| HO2  | Instantaneous Molar Mixing Ratio HO2 | ppmV | XYZT | CCTM_CONC | True |
| H2O2  | Instantaneous Molar Mixing Ratio H2O2 | ppmV | XYZT | CCTM_CONC | True |
| N2O5  | Instantaneous Molar Mixing Ratio N2O5 | ppmV | XYZT | CCTM_CONC | True |
| HNO3  | Instantaneous Molar Mixing Ratio HNO3 | ppmV | XYZT | CCTM_CONC | True |
| HONO  | Instantaneous Molar Mixing Ratio HONO | ppmV | XYZT | CCTM_CONC | True |
| PNA  | Instantaneous Molar Mixing Ratio PNA | ppmV | XYZT | CCTM_CONC | True |
| SO2  | Instantaneous Molar Mixing Ratio SO2 | ppmV | XYZT | CCTM_CONC | True |
| SULF  | Instantaneous Molar Mixing Ratio SULF | ppmV | XYZT | CCTM_CONC | True |
| C2O3  | Instantaneous Molar Mixing Ratio C2O3 | ppmV | XYZT | CCTM_CONC | True |
| MEO2  | Instantaneous Molar Mixing Ratio MEO2 | ppmV | XYZT | CCTM_CONC | True |
| RO2  | Instantaneous Molar Mixing Ratio RO2 | ppmV | XYZT | CCTM_CONC | True |
| PAN  | Instantaneous Molar Mixing Ratio PAN | ppmV | XYZT | CCTM_CONC | True |
| PACD  | Instantaneous Molar Mixing Ratio PACD | ppmV | XYZT | CCTM_CONC | True |
| AACD  | Instantaneous Molar Mixing Ratio AACD | ppmV | XYZT | CCTM_CONC | True |
| CXO3  | Instantaneous Molar Mixing Ratio CXO3 | ppmV | XYZT | CCTM_CONC | True |
| ALD2  | Instantaneous Molar Mixing Ratio ALD2 | ppmV | XYZT | CCTM_CONC | True |
| XO2H  | Instantaneous Molar Mixing Ratio XO2H | ppmV | XYZT | CCTM_CONC | True |
| PANX  | Instantaneous Molar Mixing Ratio PANX | ppmV | XYZT | CCTM_CONC | True |
| FORM  | Instantaneous Molar Mixing Ratio FORM | ppmV | XYZT | CCTM_CONC | True |
| MEPX  | Instantaneous Molar Mixing Ratio MEPX | ppmV | XYZT | CCTM_CONC | True |
| MEOH  | Instantaneous Molar Mixing Ratio MEOH | ppmV | XYZT | CCTM_CONC | True |
| ROOH  | Instantaneous Molar Mixing Ratio ROOH | ppmV | XYZT | CCTM_CONC | True |
| XO2  | Instantaneous Molar Mixing Ratio XO2 | ppmV | XYZT | CCTM_CONC | True |
| XO2N  | Instantaneous Molar Mixing Ratio XO2N | ppmV | XYZT | CCTM_CONC | True |
| XPAR  | Instantaneous Molar Mixing Ratio XPAR | ppmV | XYZT | CCTM_CONC | True |
| XPRP  | Instantaneous Molar Mixing Ratio XPRP | ppmV | XYZT | CCTM_CONC | True |
| NTR1  | Instantaneous Molar Mixing Ratio NTR1 | ppmV | XYZT | CCTM_CONC | True |
| NTR2  | Instantaneous Molar Mixing Ratio NTR2 | ppmV | XYZT | CCTM_CONC | True |
| FACD  | Instantaneous Molar Mixing Ratio FACD | ppmV | XYZT | CCTM_CONC | True |
| CO  | Instantaneous Molar Mixing Ratio CO | ppmV | XYZT | CCTM_CONC | True |
| HCO3  | Instantaneous Molar Mixing Ratio HCO3 | ppmV | XYZT | CCTM_CONC | True |
| ALDX  | Instantaneous Molar Mixing Ratio ALDX | ppmV | XYZT | CCTM_CONC | True |
| GLYD  | Instantaneous Molar Mixing Ratio GLYD | ppmV | XYZT | CCTM_CONC | True |
| GLY  | Instantaneous Molar Mixing Ratio GLY | ppmV | XYZT | CCTM_CONC | True |
| MGLY  | Instantaneous Molar Mixing Ratio MGLY | ppmV | XYZT | CCTM_CONC | True |
| ETHA  | Instantaneous Molar Mixing Ratio ETHA | ppmV | XYZT | CCTM_CONC | True |
| ETOH  | Instantaneous Molar Mixing Ratio ETOH | ppmV | XYZT | CCTM_CONC | True |
| KET  | Instantaneous Molar Mixing Ratio KET | ppmV | XYZT | CCTM_CONC | True |
| PAR  | Instantaneous Molar Mixing Ratio PAR | ppmV | XYZT | CCTM_CONC | True |
| ACET  | Instantaneous Molar Mixing Ratio ACET | ppmV | XYZT | CCTM_CONC | True |
| PRPA  | Instantaneous Molar Mixing Ratio PRPA | ppmV | XYZT | CCTM_CONC | True |
| ROR  | Instantaneous Molar Mixing Ratio ROR | ppmV | XYZT | CCTM_CONC | True |
| ETHY  | Instantaneous Molar Mixing Ratio ETHY | ppmV | XYZT | CCTM_CONC | True |
| ETH  | Instantaneous Molar Mixing Ratio ETH | ppmV | XYZT | CCTM_CONC | True |
| OLE  | Instantaneous Molar Mixing Ratio OLE | ppmV | XYZT | CCTM_CONC | True |
| IOLE  | Instantaneous Molar Mixing Ratio IOLE | ppmV | XYZT | CCTM_CONC | True |
| ISOP  | Instantaneous Molar Mixing Ratio ISOP | ppmV | XYZT | CCTM_CONC | True |
| ISO2  | Instantaneous Molar Mixing Ratio ISO2 | ppmV | XYZT | CCTM_CONC | True |
| ISPD  | Instantaneous Molar Mixing Ratio ISPD | ppmV | XYZT | CCTM_CONC | True |
| INTR  | Instantaneous Molar Mixing Ratio INTR | ppmV | XYZT | CCTM_CONC | True |
| ISPX  | Instantaneous Molar Mixing Ratio ISPX | ppmV | XYZT | CCTM_CONC | True |
| HPLD  | Instantaneous Molar Mixing Ratio HPLD | ppmV | XYZT | CCTM_CONC | True |
| OPO3  | Instantaneous Molar Mixing Ratio OPO3 | ppmV | XYZT | CCTM_CONC | True |
| EPOX  | Instantaneous Molar Mixing Ratio EPOX | ppmV | XYZT | CCTM_CONC | True |
| IEPOXP  | Instantaneous Molar Mixing Ratio IEPOXP | ppmV | XYZT | CCTM_CONC | True |
| EPX2  | Instantaneous Molar Mixing Ratio EPX2 | ppmV | XYZT | CCTM_CONC | True |
| TERP  | Instantaneous Molar Mixing Ratio TERP | ppmV | XYZT | CCTM_CONC | True |
| APIN  | Instantaneous Molar Mixing Ratio APIN | ppmV | XYZT | CCTM_CONC | True |
| TERPNRO2  | Instantaneous Molar Mixing Ratio TERPNRO2 | ppmV | XYZT | CCTM_CONC | True |
| MTNO3  | Instantaneous Molar Mixing Ratio MTNO3 | ppmV | XYZT | CCTM_CONC | True |
| BENZENE  | Instantaneous Molar Mixing Ratio BENZENE | ppmV | XYZT | CCTM_CONC | True |
| CRES  | Instantaneous Molar Mixing Ratio CRES | ppmV | XYZT | CCTM_CONC | True |
| BZO2  | Instantaneous Molar Mixing Ratio BZO2 | ppmV | XYZT | CCTM_CONC | True |
| OPEN  | Instantaneous Molar Mixing Ratio OPEN | ppmV | XYZT | CCTM_CONC | True |
| BENZRO2  | Instantaneous Molar Mixing Ratio BENZRO2 | ppmV | XYZT | CCTM_CONC | True |
| TOL  | Instantaneous Molar Mixing Ratio TOL | ppmV | XYZT | CCTM_CONC | True |
| TO2  | Instantaneous Molar Mixing Ratio TO2 | ppmV | XYZT | CCTM_CONC | True |
| TOLRO2  | Instantaneous Molar Mixing Ratio TOLRO2 | ppmV | XYZT | CCTM_CONC | True |
| XOPN  | Instantaneous Molar Mixing Ratio XOPN | ppmV | XYZT | CCTM_CONC | True |
| XYLMN  | Instantaneous Molar Mixing Ratio XYLMN | ppmV | XYZT | CCTM_CONC | True |
| XLO2  | Instantaneous Molar Mixing Ratio XLO2 | ppmV | XYZT | CCTM_CONC | True |
| XYLRO2  | Instantaneous Molar Mixing Ratio XYLRO2 | ppmV | XYZT | CCTM_CONC | True |
| NAPH  | Instantaneous Molar Mixing Ratio NAPH | ppmV | XYZT | CCTM_CONC | True |
| PAHRO2  | Instantaneous Molar Mixing Ratio PAHRO2 | ppmV | XYZT | CCTM_CONC | True |
| CRO  | Instantaneous Molar Mixing Ratio CRO | ppmV | XYZT | CCTM_CONC | True |
| CAT1  | Instantaneous Molar Mixing Ratio CAT1 | ppmV | XYZT | CCTM_CONC | True |
| CRON  | Instantaneous Molar Mixing Ratio CRON | ppmV | XYZT | CCTM_CONC | True |
| OPAN  | Instantaneous Molar Mixing Ratio OPAN | ppmV | XYZT | CCTM_CONC | True |
| ECH4  | Instantaneous Molar Mixing Ratio ECH4 | ppmV | XYZT | CCTM_CONC | True |
| CL2  | Instantaneous Molar Mixing Ratio CL2 | ppmV | XYZT | CCTM_CONC | True |
| CL  | Instantaneous Molar Mixing Ratio CL | ppmV | XYZT | CCTM_CONC | True |
| HOCL  | Instantaneous Molar Mixing Ratio HOCL | ppmV | XYZT | CCTM_CONC | True |
| CLO  | Instantaneous Molar Mixing Ratio CLO | ppmV | XYZT | CCTM_CONC | True |
| FMCL  | Instantaneous Molar Mixing Ratio FMCL | ppmV | XYZT | CCTM_CONC | True |
| HCL  | Instantaneous Molar Mixing Ratio HCL | ppmV | XYZT | CCTM_CONC | True |
| CLNO2  | Instantaneous Molar Mixing Ratio CLNO2 | ppmV | XYZT | CCTM_CONC | True |
| CLNO3  | Instantaneous Molar Mixing Ratio CLNO3 | ppmV | XYZT | CCTM_CONC | True |
| SESQ  | Instantaneous Molar Mixing Ratio SESQ | ppmV | XYZT | CCTM_CONC | True |
| SOAALK  | Instantaneous Molar Mixing Ratio SOAALK | ppmV | XYZT | CCTM_CONC | True |
| H2NO3PIJ  | Instantaneous Molar Mixing Ratio H2NO3PIJ | ppmV | XYZT | CCTM_CONC | True |
| H2NO3PK  | Instantaneous Molar Mixing Ratio H2NO3PK | ppmV | XYZT | CCTM_CONC | True |
| VLVPO1  | Instantaneous Molar Mixing Ratio VLVPO1 | ppmV | XYZT | CCTM_CONC | True |
| VSVPO1  | Instantaneous Molar Mixing Ratio VSVPO1 | ppmV | XYZT | CCTM_CONC | True |
| VSVPO2  | Instantaneous Molar Mixing Ratio VSVPO2 | ppmV | XYZT | CCTM_CONC | True |
| VSVPO3  | Instantaneous Molar Mixing Ratio VSVPO3 | ppmV | XYZT | CCTM_CONC | True |
| VIVPO1  | Instantaneous Molar Mixing Ratio VIVPO1 | ppmV | XYZT | CCTM_CONC | True |
| VLVOO1  | Instantaneous Molar Mixing Ratio VLVOO1 | ppmV | XYZT | CCTM_CONC | True |
| VLVOO2  | Instantaneous Molar Mixing Ratio VLVOO2 | ppmV | XYZT | CCTM_CONC | True |
| VSVOO1  | Instantaneous Molar Mixing Ratio VSVOO1 | ppmV | XYZT | CCTM_CONC | True |
| VSVOO2  | Instantaneous Molar Mixing Ratio VSVOO2 | ppmV | XYZT | CCTM_CONC | True |
| VSVOO3  | Instantaneous Molar Mixing Ratio VSVOO3 | ppmV | XYZT | CCTM_CONC | True |
| PCVOC  | Instantaneous Molar Mixing Ratio PCVOC | ppmV | XYZT | CCTM_CONC | True |
| FORM_PRIMARY  | Instantaneous Molar Mixing Ratio FORM_PRIMARY | ppmV | XYZT | CCTM_CONC | True |
| ALD2_PRIMARY  | Instantaneous Molar Mixing Ratio ALD2_PRIMARY | ppmV | XYZT | CCTM_CONC | True |
| BUTADIENE13  | Instantaneous Molar Mixing Ratio BUTADIENE13 | ppmV | XYZT | CCTM_CONC | True |
| ACROLEIN  | Instantaneous Molar Mixing Ratio ACROLEIN | ppmV | XYZT | CCTM_CONC | True |
| ACRO_PRIMARY  | Instantaneous Molar Mixing Ratio ACRO_PRIMARY | ppmV | XYZT | CCTM_CONC | True |
| TOLU  | Instantaneous Molar Mixing Ratio TOLU | ppmV | XYZT | CCTM_CONC | True |
| HG  | Instantaneous Molar Mixing Ratio HG | ppmV | XYZT | CCTM_CONC | True |
| HGIIGAS  | Instantaneous Molar Mixing Ratio HGIIGAS | ppmV | XYZT | CCTM_CONC | True |
| SVAVB1  | Instantaneous Molar Mixing Ratio SVAVB1 | ppmV | XYZT | CCTM_CONC | True |
| SVAVB2  | Instantaneous Molar Mixing Ratio SVAVB2 | ppmV | XYZT | CCTM_CONC | True |
| SVAVB3  | Instantaneous Molar Mixing Ratio SVAVB3 | ppmV | XYZT | CCTM_CONC | True |
| SVAVB4  | Instantaneous Molar Mixing Ratio SVAVB4 | ppmV | XYZT | CCTM_CONC | True |
| DMS  | Instantaneous Molar Mixing Ratio DMS | ppmV | XYZT | CCTM_CONC | True |
| MSA  | Instantaneous Molar Mixing Ratio MSA | ppmV | XYZT | CCTM_CONC | True |
| ASO4I  | Instantaneous Aerosol Concentration of ASO4I | ug m-3 | XYZT | CCTM_CONC | True |
| ASO4J  | Instantaneous Aerosol Concentration of ASO4J | ug m-3 | XYZT | CCTM_CONC | True |
| ASO4K  | Instantaneous Aerosol Concentration of ASO4K | ug m-3 | XYZT | CCTM_CONC | True |
| ANH4I  | Instantaneous Aerosol Concentration of ANH4I | ug m-3 | XYZT | CCTM_CONC | True |
| ANH4J  | Instantaneous Aerosol Concentration of ANH4J | ug m-3 | XYZT | CCTM_CONC | True |
| ANH4K  | Instantaneous Aerosol Concentration of ANH4K | ug m-3 | XYZT | CCTM_CONC | True |
| ANO3I  | Instantaneous Aerosol Concentration of ANO3I | ug m-3 | XYZT | CCTM_CONC | True |
| ANO3J  | Instantaneous Aerosol Concentration of ANO3J | ug m-3 | XYZT | CCTM_CONC | True |
| ANO3K  | Instantaneous Aerosol Concentration of ANO3K | ug m-3 | XYZT | CCTM_CONC | True |
| ANAI  | Instantaneous Aerosol Concentration of ANAI | ug m-3 | XYZT | CCTM_CONC | True |
| ANAJ  | Instantaneous Aerosol Concentration of ANAJ | ug m-3 | XYZT | CCTM_CONC | True |
| ACLI  | Instantaneous Aerosol Concentration of ACLI | ug m-3 | XYZT | CCTM_CONC | True |
| ACLJ  | Instantaneous Aerosol Concentration of ACLJ | ug m-3 | XYZT | CCTM_CONC | True |
| ACLK  | Instantaneous Aerosol Concentration of ACLK | ug m-3 | XYZT | CCTM_CONC | True |
| AISO1J  | Instantaneous Aerosol Concentration of AISO1J | ug m-3 | XYZT | CCTM_CONC | True |
| AISO2J  | Instantaneous Aerosol Concentration of AISO2J | ug m-3 | XYZT | CCTM_CONC | True |
| ASQTJ  | Instantaneous Aerosol Concentration of ASQTJ | ug m-3 | XYZT | CCTM_CONC | True |
| AORGCJ  | Instantaneous Aerosol Concentration of AORGCJ | ug m-3 | XYZT | CCTM_CONC | True |
| AECI  | Instantaneous Aerosol Concentration of AECI | ug m-3 | XYZT | CCTM_CONC | True |
| AECJ  | Instantaneous Aerosol Concentration of AECJ | ug m-3 | XYZT | CCTM_CONC | True |
| AOTHRI  | Instantaneous Aerosol Concentration of AOTHRI | ug m-3 | XYZT | CCTM_CONC | True |
| AOTHRJ  | Instantaneous Aerosol Concentration of AOTHRJ | ug m-3 | XYZT | CCTM_CONC | True |
| AFEJ  | Instantaneous Aerosol Concentration of AFEJ | ug m-3 | XYZT | CCTM_CONC | True |
| AALJ  | Instantaneous Aerosol Concentration of AALJ | ug m-3 | XYZT | CCTM_CONC | True |
| ASIJ  | Instantaneous Aerosol Concentration of ASIJ | ug m-3 | XYZT | CCTM_CONC | True |
| ATIJ  | Instantaneous Aerosol Concentration of ATIJ | ug m-3 | XYZT | CCTM_CONC | True |
| ACAJ  | Instantaneous Aerosol Concentration of ACAJ | ug m-3 | XYZT | CCTM_CONC | True |
| AMGJ  | Instantaneous Aerosol Concentration of AMGJ | ug m-3 | XYZT | CCTM_CONC | True |
| AKJ  | Instantaneous Aerosol Concentration of AKJ | ug m-3 | XYZT | CCTM_CONC | True |
| AMNJ  | Instantaneous Aerosol Concentration of AMNJ | ug m-3 | XYZT | CCTM_CONC | True |
| ACORS  | Instantaneous Aerosol Concentration of ACORS | ug m-3 | XYZT | CCTM_CONC | True |
| ASOIL  | Instantaneous Aerosol Concentration of ASOIL | ug m-3 | XYZT | CCTM_CONC | True |
| NUMATKN  | Instantaneous Aerosol Concentration of NUMATKN | m-3 | XYZT | CCTM_CONC | True |
| NUMACC  | Instantaneous Aerosol Concentration of NUMACC | m-3 | XYZT | CCTM_CONC | True |
| NUMCOR  | Instantaneous Aerosol Concentration of NUMCOR | m-3 | XYZT | CCTM_CONC | True |
| SRFATKN  | Instantaneous Aerosol Concentration of SRFATKN | m2 m-3 | XYZT | CCTM_CONC | True |
| SRFACC  | Instantaneous Aerosol Concentration of SRFACC | m2 m-3 | XYZT | CCTM_CONC | True |
| SRFCOR  | Instantaneous Aerosol Concentration of SRFCOR | m2 m-3 | XYZT | CCTM_CONC | True |
| AORGH2OJ  | Instantaneous Aerosol Concentration of AORGH2OJ | ug m-3 | XYZT | CCTM_CONC | True |
| AH2OI  | Instantaneous Aerosol Concentration of AH2OI | ug m-3 | XYZT | CCTM_CONC | True |
| AH2OJ  | Instantaneous Aerosol Concentration of AH2OJ | ug m-3 | XYZT | CCTM_CONC | True |
| AH2OK  | Instantaneous Aerosol Concentration of AH2OK | ug m-3 | XYZT | CCTM_CONC | True |
| AH3OPI  | Instantaneous Aerosol Concentration of AH3OPI | ug m-3 | XYZT | CCTM_CONC | True |
| AH3OPJ  | Instantaneous Aerosol Concentration of AH3OPJ | ug m-3 | XYZT | CCTM_CONC | True |
| AH3OPK  | Instantaneous Aerosol Concentration of AH3OPK | ug m-3 | XYZT | CCTM_CONC | True |
| ASEACAT  | Instantaneous Aerosol Concentration of ASEACAT | ug m-3 | XYZT | CCTM_CONC | True |
| AISO3J  | Instantaneous Aerosol Concentration of AISO3J | ug m-3 | XYZT | CCTM_CONC | True |
| AOLGAJ  | Instantaneous Aerosol Concentration of AOLGAJ | ug m-3 | XYZT | CCTM_CONC | True |
| AOLGBJ  | Instantaneous Aerosol Concentration of AOLGBJ | ug m-3 | XYZT | CCTM_CONC | True |
| AGLYJ  | Instantaneous Aerosol Concentration of AGLYJ | ug m-3 | XYZT | CCTM_CONC | True |
| AMTNO3J  | Instantaneous Aerosol Concentration of AMTNO3J | ug m-3 | XYZT | CCTM_CONC | True |
| AMTHYDJ  | Instantaneous Aerosol Concentration of AMTHYDJ | ug m-3 | XYZT | CCTM_CONC | True |
| APOCI  | Instantaneous Aerosol Concentration of APOCI | ug m-3 | XYZT | CCTM_CONC | True |
| APOCJ  | Instantaneous Aerosol Concentration of APOCJ | ug m-3 | XYZT | CCTM_CONC | True |
| APNCOMI  | Instantaneous Aerosol Concentration of APNCOMI | ug m-3 | XYZT | CCTM_CONC | True |
| APNCOMJ  | Instantaneous Aerosol Concentration of APNCOMJ | ug m-3 | XYZT | CCTM_CONC | True |
| APCSOJ  | Instantaneous Aerosol Concentration of APCSOJ | ug m-3 | XYZT | CCTM_CONC | True |
| ALVPO1I  | Instantaneous Aerosol Concentration of ALVPO1I | ug m-3 | XYZT | CCTM_CONC | True |
| ALVPO1J  | Instantaneous Aerosol Concentration of ALVPO1J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVPO1I  | Instantaneous Aerosol Concentration of ASVPO1I | ug m-3 | XYZT | CCTM_CONC | True |
| ASVPO1J  | Instantaneous Aerosol Concentration of ASVPO1J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVPO2I  | Instantaneous Aerosol Concentration of ASVPO2I | ug m-3 | XYZT | CCTM_CONC | True |
| ASVPO2J  | Instantaneous Aerosol Concentration of ASVPO2J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVPO3J  | Instantaneous Aerosol Concentration of ASVPO3J | ug m-3 | XYZT | CCTM_CONC | True |
| AIVPO1J  | Instantaneous Aerosol Concentration of AIVPO1J | ug m-3 | XYZT | CCTM_CONC | True |
| ALVOO1I  | Instantaneous Aerosol Concentration of ALVOO1I | ug m-3 | XYZT | CCTM_CONC | True |
| ALVOO1J  | Instantaneous Aerosol Concentration of ALVOO1J | ug m-3 | XYZT | CCTM_CONC | True |
| ALVOO2I  | Instantaneous Aerosol Concentration of ALVOO2I | ug m-3 | XYZT | CCTM_CONC | True |
| ALVOO2J  | Instantaneous Aerosol Concentration of ALVOO2J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVOO1I  | Instantaneous Aerosol Concentration of ASVOO1I | ug m-3 | XYZT | CCTM_CONC | True |
| ASVOO1J  | Instantaneous Aerosol Concentration of ASVOO1J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVOO2I  | Instantaneous Aerosol Concentration of ASVOO2I | ug m-3 | XYZT | CCTM_CONC | True |
| ASVOO2J  | Instantaneous Aerosol Concentration of ASVOO2J | ug m-3 | XYZT | CCTM_CONC | True |
| ASVOO3J  | Instantaneous Aerosol Concentration of ASVOO3J | ug m-3 | XYZT | CCTM_CONC | True |
| AAVB1J  | Instantaneous Aerosol Concentration of AAVB1J | ug m-3 | XYZT | CCTM_CONC | True |
| AAVB2J  | Instantaneous Aerosol Concentration of AAVB2J | ug m-3 | XYZT | CCTM_CONC | True |
| AAVB3J  | Instantaneous Aerosol Concentration of AAVB3J | ug m-3 | XYZT | CCTM_CONC | True |
| AAVB4J  | Instantaneous Aerosol Concentration of AAVB4J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT1J  | Instantaneous Aerosol Concentration of AMT1J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT2J  | Instantaneous Aerosol Concentration of AMT2J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT3J  | Instantaneous Aerosol Concentration of AMT3J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT4J  | Instantaneous Aerosol Concentration of AMT4J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT5J  | Instantaneous Aerosol Concentration of AMT5J | ug m-3 | XYZT | CCTM_CONC | True |
| AMT6J  | Instantaneous Aerosol Concentration of AMT6J | ug m-3 | XYZT | CCTM_CONC | True |
| NH3  | Instantaneous molar mixing ratio of NH3 | ppmV | XYZT | CCTM_CONC | True |
| SVISO1  | Instantaneous molar mixing ratio of SVISO1 | ppmV | XYZT | CCTM_CONC | True |
| SVISO2  | Instantaneous molar mixing ratio of SVISO2 | ppmV | XYZT | CCTM_CONC | True |
| SVSQT  | Instantaneous molar mixing ratio of SVSQT | ppmV | XYZT | CCTM_CONC | True |
| LVPCSOG  | Instantaneous molar mixing ratio of LVPCSOG | ppmV | XYZT | CCTM_CONC | True |
| SVMT1  | Instantaneous molar mixing ratio of SVMT1 | ppmV | XYZT | CCTM_CONC | True |
| SVMT2  | Instantaneous molar mixing ratio of SVMT2 | ppmV | XYZT | CCTM_CONC | True |
| SVMT3  | Instantaneous molar mixing ratio of SVMT3 | ppmV | XYZT | CCTM_CONC | True |
| SVMT4  | Instantaneous molar mixing ratio of SVMT4 | ppmV | XYZT | CCTM_CONC | True |
| SVMT5  | Instantaneous molar mixing ratio of SVMT5 | ppmV | XYZT | CCTM_CONC | True |
| SVMT6  | Instantaneous molar mixing ratio of SVMT6 | ppmV | XYZT | CCTM_CONC | True |
| O3_BCT  | Instantaneous molar mixing ratio of O3_BCT | ppmV | XYZT | CCTM_CONC | True |
| W_VEL  | Derived vertical velocity component | m s-1 | XYZT | CCTM_CONC | True |
| RH  | Fractional Relative Humidity | 1 | XYZT | CCTM_CONC | True |
| TA  | Air Temperature | K | XYZT | CCTM_CONC | True |
| PRES  | Air Pressure | Pa | XYZT | CCTM_CONC | True |
| NO2  | Average Molar Mixing Ratio of NO2 | ppmV | XYT | CCTM_ACONC | True |
| NO  | Average Molar Mixing Ratio of NO | ppmV | XYT | CCTM_ACONC | True |
| O  | Average Molar Mixing Ratio of O | ppmV | XYT | CCTM_ACONC | True |
| O3  | Average Molar Mixing Ratio of O3 | ppmV | XYT | CCTM_ACONC | True |
| NO3  | Average Molar Mixing Ratio of NO3 | ppmV | XYT | CCTM_ACONC | True |
| O1D  | Average Molar Mixing Ratio of O1D | ppmV | XYT | CCTM_ACONC | True |
| OH  | Average Molar Mixing Ratio of OH | ppmV | XYT | CCTM_ACONC | True |
| HO2  | Average Molar Mixing Ratio of HO2 | ppmV | XYT | CCTM_ACONC | True |
| H2O2  | Average Molar Mixing Ratio of H2O2 | ppmV | XYT | CCTM_ACONC | True |
| N2O5  | Average Molar Mixing Ratio of N2O5 | ppmV | XYT | CCTM_ACONC | True |
| HNO3  | Average Molar Mixing Ratio of HNO3 | ppmV | XYT | CCTM_ACONC | True |
| HONO  | Average Molar Mixing Ratio of HONO | ppmV | XYT | CCTM_ACONC | True |
| PNA  | Average Molar Mixing Ratio of PNA | ppmV | XYT | CCTM_ACONC | True |
| SO2  | Average Molar Mixing Ratio of SO2 | ppmV | XYT | CCTM_ACONC | True |
| SULF  | Average Molar Mixing Ratio of SULF | ppmV | XYT | CCTM_ACONC | True |
| C2O3  | Average Molar Mixing Ratio of C2O3 | ppmV | XYT | CCTM_ACONC | True |
| MEO2  | Average Molar Mixing Ratio of MEO2 | ppmV | XYT | CCTM_ACONC | True |
| RO2  | Average Molar Mixing Ratio of RO2 | ppmV | XYT | CCTM_ACONC | True |
| PAN  | Average Molar Mixing Ratio of PAN | ppmV | XYT | CCTM_ACONC | True |
| PACD  | Average Molar Mixing Ratio of PACD | ppmV | XYT | CCTM_ACONC | True |
| AACD  | Average Molar Mixing Ratio of AACD | ppmV | XYT | CCTM_ACONC | True |
| CXO3  | Average Molar Mixing Ratio of CXO3 | ppmV | XYT | CCTM_ACONC | True |
| ALD2  | Average Molar Mixing Ratio of ALD2 | ppmV | XYT | CCTM_ACONC | True |
| XO2H  | Average Molar Mixing Ratio of XO2H | ppmV | XYT | CCTM_ACONC | True |
| PANX  | Average Molar Mixing Ratio of PANX | ppmV | XYT | CCTM_ACONC | True |
| FORM  | Average Molar Mixing Ratio of FORM | ppmV | XYT | CCTM_ACONC | True |
| MEPX  | Average Molar Mixing Ratio of MEPX | ppmV | XYT | CCTM_ACONC | True |
| MEOH  | Average Molar Mixing Ratio of MEOH | ppmV | XYT | CCTM_ACONC | True |
| ROOH  | Average Molar Mixing Ratio of ROOH | ppmV | XYT | CCTM_ACONC | True |
| XO2  | Average Molar Mixing Ratio of XO2 | ppmV | XYT | CCTM_ACONC | True |
| XO2N  | Average Molar Mixing Ratio of XO2N | ppmV | XYT | CCTM_ACONC | True |
| XPAR  | Average Molar Mixing Ratio of XPAR | ppmV | XYT | CCTM_ACONC | True |
| XPRP  | Average Molar Mixing Ratio of XPRP | ppmV | XYT | CCTM_ACONC | True |
| NTR1  | Average Molar Mixing Ratio of NTR1 | ppmV | XYT | CCTM_ACONC | True |
| NTR2  | Average Molar Mixing Ratio of NTR2 | ppmV | XYT | CCTM_ACONC | True |
| FACD  | Average Molar Mixing Ratio of FACD | ppmV | XYT | CCTM_ACONC | True |
| CO  | Average Molar Mixing Ratio of CO | ppmV | XYT | CCTM_ACONC | True |
| HCO3  | Average Molar Mixing Ratio of HCO3 | ppmV | XYT | CCTM_ACONC | True |
| ALDX  | Average Molar Mixing Ratio of ALDX | ppmV | XYT | CCTM_ACONC | True |
| GLYD  | Average Molar Mixing Ratio of GLYD | ppmV | XYT | CCTM_ACONC | True |
| GLY  | Average Molar Mixing Ratio of GLY | ppmV | XYT | CCTM_ACONC | True |
| MGLY  | Average Molar Mixing Ratio of MGLY | ppmV | XYT | CCTM_ACONC | True |
| ETHA  | Average Molar Mixing Ratio of ETHA | ppmV | XYT | CCTM_ACONC | True |
| ETOH  | Average Molar Mixing Ratio of ETOH | ppmV | XYT | CCTM_ACONC | True |
| KET  | Average Molar Mixing Ratio of KET | ppmV | XYT | CCTM_ACONC | True |
| PAR  | Average Molar Mixing Ratio of PAR | ppmV | XYT | CCTM_ACONC | True |
| ACET  | Average Molar Mixing Ratio of ACET | ppmV | XYT | CCTM_ACONC | True |
| PRPA  | Average Molar Mixing Ratio of PRPA | ppmV | XYT | CCTM_ACONC | True |
| ROR  | Average Molar Mixing Ratio of ROR | ppmV | XYT | CCTM_ACONC | True |
| ETHY  | Average Molar Mixing Ratio of ETHY | ppmV | XYT | CCTM_ACONC | True |
| ETH  | Average Molar Mixing Ratio of ETH | ppmV | XYT | CCTM_ACONC | True |
| OLE  | Average Molar Mixing Ratio of OLE | ppmV | XYT | CCTM_ACONC | True |
| IOLE  | Average Molar Mixing Ratio of IOLE | ppmV | XYT | CCTM_ACONC | True |
| ISOP  | Average Molar Mixing Ratio of ISOP | ppmV | XYT | CCTM_ACONC | True |
| ISO2  | Average Molar Mixing Ratio of ISO2 | ppmV | XYT | CCTM_ACONC | True |
| ISPD  | Average Molar Mixing Ratio of ISPD | ppmV | XYT | CCTM_ACONC | True |
| INTR  | Average Molar Mixing Ratio of INTR | ppmV | XYT | CCTM_ACONC | True |
| ISPX  | Average Molar Mixing Ratio of ISPX | ppmV | XYT | CCTM_ACONC | True |
| HPLD  | Average Molar Mixing Ratio of HPLD | ppmV | XYT | CCTM_ACONC | True |
| OPO3  | Average Molar Mixing Ratio of OPO3 | ppmV | XYT | CCTM_ACONC | True |
| EPOX  | Average Molar Mixing Ratio of EPOX | ppmV | XYT | CCTM_ACONC | True |
| IEPOXP  | Average Molar Mixing Ratio of IEPOXP | ppmV | XYT | CCTM_ACONC | True |
| EPX2  | Average Molar Mixing Ratio of EPX2 | ppmV | XYT | CCTM_ACONC | True |
| TERP  | Average Molar Mixing Ratio of TERP | ppmV | XYT | CCTM_ACONC | True |
| APIN  | Average Molar Mixing Ratio of APIN | ppmV | XYT | CCTM_ACONC | True |
| TERPNRO2  | Average Molar Mixing Ratio of TERPNRO2 | ppmV | XYT | CCTM_ACONC | True |
| MTNO3  | Average Molar Mixing Ratio of MTNO3 | ppmV | XYT | CCTM_ACONC | True |
| BENZENE  | Average Molar Mixing Ratio of BENZENE | ppmV | XYT | CCTM_ACONC | True |
| CRES  | Average Molar Mixing Ratio of CRES | ppmV | XYT | CCTM_ACONC | True |
| BZO2  | Average Molar Mixing Ratio of BZO2 | ppmV | XYT | CCTM_ACONC | True |
| OPEN  | Average Molar Mixing Ratio of OPEN | ppmV | XYT | CCTM_ACONC | True |
| BENZRO2  | Average Molar Mixing Ratio of BENZRO2 | ppmV | XYT | CCTM_ACONC | True |
| TOL  | Average Molar Mixing Ratio of TOL | ppmV | XYT | CCTM_ACONC | True |
| TO2  | Average Molar Mixing Ratio of TO2 | ppmV | XYT | CCTM_ACONC | True |
| TOLRO2  | Average Molar Mixing Ratio of TOLRO2 | ppmV | XYT | CCTM_ACONC | True |
| XOPN  | Average Molar Mixing Ratio of XOPN | ppmV | XYT | CCTM_ACONC | True |
| XYLMN  | Average Molar Mixing Ratio of XYLMN | ppmV | XYT | CCTM_ACONC | True |
| XLO2  | Average Molar Mixing Ratio of XLO2 | ppmV | XYT | CCTM_ACONC | True |
| XYLRO2  | Average Molar Mixing Ratio of XYLRO2 | ppmV | XYT | CCTM_ACONC | True |
| NAPH  | Average Molar Mixing Ratio of NAPH | ppmV | XYT | CCTM_ACONC | True |
| PAHRO2  | Average Molar Mixing Ratio of PAHRO2 | ppmV | XYT | CCTM_ACONC | True |
| CRO  | Average Molar Mixing Ratio of CRO | ppmV | XYT | CCTM_ACONC | True |
| CAT1  | Average Molar Mixing Ratio of CAT1 | ppmV | XYT | CCTM_ACONC | True |
| CRON  | Average Molar Mixing Ratio of CRON | ppmV | XYT | CCTM_ACONC | True |
| OPAN  | Average Molar Mixing Ratio of OPAN | ppmV | XYT | CCTM_ACONC | True |
| ECH4  | Average Molar Mixing Ratio of ECH4 | ppmV | XYT | CCTM_ACONC | True |
| CL2  | Average Molar Mixing Ratio of CL2 | ppmV | XYT | CCTM_ACONC | True |
| CL  | Average Molar Mixing Ratio of CL | ppmV | XYT | CCTM_ACONC | True |
| HOCL  | Average Molar Mixing Ratio of HOCL | ppmV | XYT | CCTM_ACONC | True |
| CLO  | Average Molar Mixing Ratio of CLO | ppmV | XYT | CCTM_ACONC | True |
| FMCL  | Average Molar Mixing Ratio of FMCL | ppmV | XYT | CCTM_ACONC | True |
| HCL  | Average Molar Mixing Ratio of HCL | ppmV | XYT | CCTM_ACONC | True |
| CLNO2  | Average Molar Mixing Ratio of CLNO2 | ppmV | XYT | CCTM_ACONC | True |
| CLNO3  | Average Molar Mixing Ratio of CLNO3 | ppmV | XYT | CCTM_ACONC | True |
| SESQ  | Average Molar Mixing Ratio of SESQ | ppmV | XYT | CCTM_ACONC | True |
| SOAALK  | Average Molar Mixing Ratio of SOAALK | ppmV | XYT | CCTM_ACONC | True |
| H2NO3PIJ  | Average Molar Mixing Ratio of H2NO3PIJ | ppmV | XYT | CCTM_ACONC | True |
| H2NO3PK  | Average Molar Mixing Ratio of H2NO3PK | ppmV | XYT | CCTM_ACONC | True |
| VLVPO1  | Average Molar Mixing Ratio of VLVPO1 | ppmV | XYT | CCTM_ACONC | True |
| VSVPO1  | Average Molar Mixing Ratio of VSVPO1 | ppmV | XYT | CCTM_ACONC | True |
| VSVPO2  | Average Molar Mixing Ratio of VSVPO2 | ppmV | XYT | CCTM_ACONC | True |
| VSVPO3  | Average Molar Mixing Ratio of VSVPO3 | ppmV | XYT | CCTM_ACONC | True |
| VIVPO1  | Average Molar Mixing Ratio of VIVPO1 | ppmV | XYT | CCTM_ACONC | True |
| VLVOO1  | Average Molar Mixing Ratio of VLVOO1 | ppmV | XYT | CCTM_ACONC | True |
| VLVOO2  | Average Molar Mixing Ratio of VLVOO2 | ppmV | XYT | CCTM_ACONC | True |
| VSVOO1  | Average Molar Mixing Ratio of VSVOO1 | ppmV | XYT | CCTM_ACONC | True |
| VSVOO2  | Average Molar Mixing Ratio of VSVOO2 | ppmV | XYT | CCTM_ACONC | True |
| VSVOO3  | Average Molar Mixing Ratio of VSVOO3 | ppmV | XYT | CCTM_ACONC | True |
| PCVOC  | Average Molar Mixing Ratio of PCVOC | ppmV | XYT | CCTM_ACONC | True |
| FORM_PRIMARY  | Average Molar Mixing Ratio of FORM_PRIMARY | ppmV | XYT | CCTM_ACONC | True |
| ALD2_PRIMARY  | Average Molar Mixing Ratio of ALD2_PRIMARY | ppmV | XYT | CCTM_ACONC | True |
| BUTADIENE13  | Average Molar Mixing Ratio of BUTADIENE13 | ppmV | XYT | CCTM_ACONC | True |
| ACROLEIN  | Average Molar Mixing Ratio of ACROLEIN | ppmV | XYT | CCTM_ACONC | True |
| ACRO_PRIMARY  | Average Molar Mixing Ratio of ACRO_PRIMARY | ppmV | XYT | CCTM_ACONC | True |
| TOLU  | Average Molar Mixing Ratio of TOLU | ppmV | XYT | CCTM_ACONC | True |
| HG  | Average Molar Mixing Ratio of HG | ppmV | XYT | CCTM_ACONC | True |
| HGIIGAS  | Average Molar Mixing Ratio of HGIIGAS | ppmV | XYT | CCTM_ACONC | True |
| SVAVB1  | Average Molar Mixing Ratio of SVAVB1 | ppmV | XYT | CCTM_ACONC | True |
| SVAVB2  | Average Molar Mixing Ratio of SVAVB2 | ppmV | XYT | CCTM_ACONC | True |
| SVAVB3  | Average Molar Mixing Ratio of SVAVB3 | ppmV | XYT | CCTM_ACONC | True |
| SVAVB4  | Average Molar Mixing Ratio of SVAVB4 | ppmV | XYT | CCTM_ACONC | True |
| DMS  | Average Molar Mixing Ratio of DMS | ppmV | XYT | CCTM_ACONC | True |
| MSA  | Average Molar Mixing Ratio of MSA | ppmV | XYT | CCTM_ACONC | True |
| ASO4I  | Average Concentrations of ASO4I | ug m-3 | XYT | CCTM_ACONC | True |
| ASO4J  | Average Concentrations of ASO4J | ug m-3 | XYT | CCTM_ACONC | True |
| ASO4K  | Average Concentrations of ASO4K | ug m-3 | XYT | CCTM_ACONC | True |
| ANH4I  | Average Concentrations of ANH4I | ug m-3 | XYT | CCTM_ACONC | True |
| ANH4J  | Average Concentrations of ANH4J | ug m-3 | XYT | CCTM_ACONC | True |
| ANH4K  | Average Concentrations of ANH4K | ug m-3 | XYT | CCTM_ACONC | True |
| ANO3I  | Average Concentrations of ANO3I | ug m-3 | XYT | CCTM_ACONC | True |
| ANO3J  | Average Concentrations of ANO3J | ug m-3 | XYT | CCTM_ACONC | True |
| ANO3K  | Average Concentrations of ANO3K | ug m-3 | XYT | CCTM_ACONC | True |
| ANAI  | Average Concentrations of ANAI | ug m-3 | XYT | CCTM_ACONC | True |
| ANAJ  | Average Concentrations of ANAJ | ug m-3 | XYT | CCTM_ACONC | True |
| ACLI  | Average Concentrations of ACLI | ug m-3 | XYT | CCTM_ACONC | True |
| ACLJ  | Average Concentrations of ACLJ | ug m-3 | XYT | CCTM_ACONC | True |
| ACLK  | Average Concentrations of ACLK | ug m-3 | XYT | CCTM_ACONC | True |
| AISO1J  | Average Concentrations of AISO1J | ug m-3 | XYT | CCTM_ACONC | True |
| AISO2J  | Average Concentrations of AISO2J | ug m-3 | XYT | CCTM_ACONC | True |
| ASQTJ  | Average Concentrations of ASQTJ | ug m-3 | XYT | CCTM_ACONC | True |
| AORGCJ  | Average Concentrations of AORGCJ | ug m-3 | XYT | CCTM_ACONC | True |
| AECI  | Average Concentrations of AECI | ug m-3 | XYT | CCTM_ACONC | True |
| AECJ  | Average Concentrations of AECJ | ug m-3 | XYT | CCTM_ACONC | True |
| AOTHRI  | Average Concentrations of AOTHRI | ug m-3 | XYT | CCTM_ACONC | True |
| AOTHRJ  | Average Concentrations of AOTHRJ | ug m-3 | XYT | CCTM_ACONC | True |
| AFEJ  | Average Concentrations of AFEJ | ug m-3 | XYT | CCTM_ACONC | True |
| AALJ  | Average Concentrations of AALJ | ug m-3 | XYT | CCTM_ACONC | True |
| ASIJ  | Average Concentrations of ASIJ | ug m-3 | XYT | CCTM_ACONC | True |
| ATIJ  | Average Concentrations of ATIJ | ug m-3 | XYT | CCTM_ACONC | True |
| ACAJ  | Average Concentrations of ACAJ | ug m-3 | XYT | CCTM_ACONC | True |
| AMGJ  | Average Concentrations of AMGJ | ug m-3 | XYT | CCTM_ACONC | True |
| AKJ  | Average Concentrations of AKJ | ug m-3 | XYT | CCTM_ACONC | True |
| AMNJ  | Average Concentrations of AMNJ | ug m-3 | XYT | CCTM_ACONC | True |
| ACORS  | Average Concentrations of ACORS | ug m-3 | XYT | CCTM_ACONC | True |
| ASOIL  | Average Concentrations of ASOIL | ug m-3 | XYT | CCTM_ACONC | True |
| NUMATKN  | Average Concentrations of NUMATKN | m-3 | XYT | CCTM_ACONC | True |
| NUMACC  | Average Concentrations of NUMACC | m-3 | XYT | CCTM_ACONC | True |
| NUMCOR  | Average Concentrations of NUMCOR | m-3 | XYT | CCTM_ACONC | True |
| SRFATKN  | Average Concentrations of SRFATKN | m2 m-3 | XYT | CCTM_ACONC | True |
| SRFACC  | Average Concentrations of SRFACC | m2 m-3 | XYT | CCTM_ACONC | True |
| SRFCOR  | Average Concentrations of SRFCOR | m2 m-3 | XYT | CCTM_ACONC | True |
| AORGH2OJ  | Average Concentrations of AORGH2OJ | ug m-3 | XYT | CCTM_ACONC | True |
| AH2OI  | Average Concentrations of AH2OI | ug m-3 | XYT | CCTM_ACONC | True |
| AH2OJ  | Average Concentrations of AH2OJ | ug m-3 | XYT | CCTM_ACONC | True |
| AH2OK  | Average Concentrations of AH2OK | ug m-3 | XYT | CCTM_ACONC | True |
| AH3OPI  | Average Concentrations of AH3OPI | ug m-3 | XYT | CCTM_ACONC | True |
| AH3OPJ  | Average Concentrations of AH3OPJ | ug m-3 | XYT | CCTM_ACONC | True |
| AH3OPK  | Average Concentrations of AH3OPK | ug m-3 | XYT | CCTM_ACONC | True |
| ASEACAT  | Average Concentrations of ASEACAT | ug m-3 | XYT | CCTM_ACONC | True |
| AISO3J  | Average Concentrations of AISO3J | ug m-3 | XYT | CCTM_ACONC | True |
| AOLGAJ  | Average Concentrations of AOLGAJ | ug m-3 | XYT | CCTM_ACONC | True |
| AOLGBJ  | Average Concentrations of AOLGBJ | ug m-3 | XYT | CCTM_ACONC | True |
| AGLYJ  | Average Concentrations of AGLYJ | ug m-3 | XYT | CCTM_ACONC | True |
| AMTNO3J  | Average Concentrations of AMTNO3J | ug m-3 | XYT | CCTM_ACONC | True |
| AMTHYDJ  | Average Concentrations of AMTHYDJ | ug m-3 | XYT | CCTM_ACONC | True |
| APOCI  | Average Concentrations of APOCI | ug m-3 | XYT | CCTM_ACONC | True |
| APOCJ  | Average Concentrations of APOCJ | ug m-3 | XYT | CCTM_ACONC | True |
| APNCOMI  | Average Concentrations of APNCOMI | ug m-3 | XYT | CCTM_ACONC | True |
| APNCOMJ  | Average Concentrations of APNCOMJ | ug m-3 | XYT | CCTM_ACONC | True |
| APCSOJ  | Average Concentrations of APCSOJ | ug m-3 | XYT | CCTM_ACONC | True |
| ALVPO1I  | Average Concentrations of ALVPO1I | ug m-3 | XYT | CCTM_ACONC | True |
| ALVPO1J  | Average Concentrations of ALVPO1J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVPO1I  | Average Concentrations of ASVPO1I | ug m-3 | XYT | CCTM_ACONC | True |
| ASVPO1J  | Average Concentrations of ASVPO1J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVPO2I  | Average Concentrations of ASVPO2I | ug m-3 | XYT | CCTM_ACONC | True |
| ASVPO2J  | Average Concentrations of ASVPO2J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVPO3J  | Average Concentrations of ASVPO3J | ug m-3 | XYT | CCTM_ACONC | True |
| AIVPO1J  | Average Concentrations of AIVPO1J | ug m-3 | XYT | CCTM_ACONC | True |
| ALVOO1I  | Average Concentrations of ALVOO1I | ug m-3 | XYT | CCTM_ACONC | True |
| ALVOO1J  | Average Concentrations of ALVOO1J | ug m-3 | XYT | CCTM_ACONC | True |
| ALVOO2I  | Average Concentrations of ALVOO2I | ug m-3 | XYT | CCTM_ACONC | True |
| ALVOO2J  | Average Concentrations of ALVOO2J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVOO1I  | Average Concentrations of ASVOO1I | ug m-3 | XYT | CCTM_ACONC | True |
| ASVOO1J  | Average Concentrations of ASVOO1J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVOO2I  | Average Concentrations of ASVOO2I | ug m-3 | XYT | CCTM_ACONC | True |
| ASVOO2J  | Average Concentrations of ASVOO2J | ug m-3 | XYT | CCTM_ACONC | True |
| ASVOO3J  | Average Concentrations of ASVOO3J | ug m-3 | XYT | CCTM_ACONC | True |
| AAVB1J  | Average Concentrations of AAVB1J | ug m-3 | XYT | CCTM_ACONC | True |
| AAVB2J  | Average Concentrations of AAVB2J | ug m-3 | XYT | CCTM_ACONC | True |
| AAVB3J  | Average Concentrations of AAVB3J | ug m-3 | XYT | CCTM_ACONC | True |
| AAVB4J  | Average Concentrations of AAVB4J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT1J  | Average Concentrations of AMT1J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT2J  | Average Concentrations of AMT2J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT3J  | Average Concentrations of AMT3J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT4J  | Average Concentrations of AMT4J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT5J  | Average Concentrations of AMT5J | ug m-3 | XYT | CCTM_ACONC | True |
| AMT6J  | Average Concentrations of AMT6J | ug m-3 | XYT | CCTM_ACONC | True |
| NH3  | Average Molar Mixing Ratio of NH3 | ppmV | XYT | CCTM_ACONC | True |
| SVISO1  | Average Molar Mixing Ratio of SVISO1 | ppmV | XYT | CCTM_ACONC | True |
| SVISO2  | Average Molar Mixing Ratio of SVISO2 | ppmV | XYT | CCTM_ACONC | True |
| SVSQT  | Average Molar Mixing Ratio of SVSQT | ppmV | XYT | CCTM_ACONC | True |
| LVPCSOG  | Average Molar Mixing Ratio of LVPCSOG | ppmV | XYT | CCTM_ACONC | True |
| SVMT1  | Average Molar Mixing Ratio of SVMT1 | ppmV | XYT | CCTM_ACONC | True |
| SVMT2  | Average Molar Mixing Ratio of SVMT2 | ppmV | XYT | CCTM_ACONC | True |
| SVMT3  | Average Molar Mixing Ratio of SVMT3 | ppmV | XYT | CCTM_ACONC | True |
| SVMT4  | Average Molar Mixing Ratio of SVMT4 | ppmV | XYT | CCTM_ACONC | True |
| SVMT5  | Average Molar Mixing Ratio of SVMT5 | ppmV | XYT | CCTM_ACONC | True |
| SVMT6  | Average Molar Mixing Ratio of SVMT6 | ppmV | XYT | CCTM_ACONC | True |
| O3_BCT  | Average Molar Mixing Ratio of O3_BCT | ppmV | XYT | CCTM_ACONC | True |
| W_VEL  | Vertical Wind Velocity | m s-1 | XYT | CCTM_ACONC | True |
| RH  | Fractional Relative Humidity | 1 | XYT | CCTM_ACONC | True |
| TA  | Air Temperature | K | XYT | CCTM_ACONC | True |
| PRES  | Air Pressure | Pa | XYT | CCTM_ACONC | True |
| NO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NO  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| O3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| H2O2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| N2O5  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HNO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HONO  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PNA  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SULF  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PAN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PACD  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AACD  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALD2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PANX  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| FORM  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| MEPX  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| MEOH  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ROOH  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NTR1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NTR2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| FACD  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CO  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALDX  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| GLYD  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| GLY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| MGLY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ETHA  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ETOH  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| KET  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PAR  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACET  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PRPA  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ETHY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ETH  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| OLE  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| IOLE  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ISOP  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ISPD  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| INTR  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ISPX  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| EPOX  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| TERP  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APIN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| MTNO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| BENZENE  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CRES  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| OPEN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| TOL  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| XOPN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| XYLMN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NAPH  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CAT1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CRON  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| OPAN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ECH4  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CL2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HOCL  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| FMCL  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HCL  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CLNO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| CLNO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SESQ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SOAALK  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VLVPO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVPO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVPO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVPO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VIVPO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VLVOO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VLVOO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVOO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVOO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| VSVOO3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| PCVOC  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| FORM_PRIMARY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALD2_PRIMARY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| BUTADIENE13  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACROLEIN  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACRO_PRIMARY  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| TOLU  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HG  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HGIIGAS  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVAVB1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVAVB2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVAVB3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVAVB4  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASO4I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASO4J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASO4K  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANH4I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANH4J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANH4K  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANO3I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANO3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANO3K  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANAI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ANAJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACLI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACLJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACLK  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AISO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AISO2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASQTJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AORGCJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AECI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AECJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AOTHRI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AOTHRJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AFEJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AALJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASIJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ATIJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACAJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMGJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AKJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMNJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ACORS  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASOIL  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASEACAT  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AISO3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AOLGAJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AOLGBJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AGLYJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMTNO3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMTHYDJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APOCI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APOCJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APNCOMI  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APNCOMJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| APCSOJ  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVPO1I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVPO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVPO1I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVPO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVPO2I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVPO2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVPO3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AIVPO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVOO1I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVOO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVOO2I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ALVOO2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVOO1I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVOO1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVOO2I  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVOO2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| ASVOO3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AAVB1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AAVB2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AAVB3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AAVB4J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT1J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT2J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT3J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT4J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT5J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| AMT6J  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NH3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVISO1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVISO2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVSQT  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT1  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT2  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT3  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT4  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT5  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| SVMT6  | hourly dry deposition values | kg/hectare | XYT | CCTM_DRYDEP | True |
| O3_BCT  | hourly dry deposition values | ---- | XYT | CCTM_DRYDEP | True |
| NH3_Emis  | hourly emission values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NH3_Flux  | hourly flux values | kg/hectare | XYT | CCTM_DRYDEP | True |
| HONO_Het  | hourly surface heterogenous production values | kg/hectare | XYT | CCTM_DRYDEP | True |
| NO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NO  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| O3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| H2O2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| N2O5  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HNO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HONO  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| PNA  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SULF  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| PAN  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| PACD  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AACD  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALD2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| PANX  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| FORM  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| MEPX  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| MEOH  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ROOH  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NTR1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NTR2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| FACD  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| CO  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALDX  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| INTR  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ISPX  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| MTNO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| BENZENE  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| CRON  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| OPAN  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ECH4  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| CL2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HOCL  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| FMCL  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HCL  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| CLNO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| CLNO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VLVPO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVPO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVPO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVPO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VIVPO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VLVOO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VLVOO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVOO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVOO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| VSVOO3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| PCVOC  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| FORM_PRIMARY  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALD2_PRIMARY  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| TOLU  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HG  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HGIIGAS  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVAVB1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVAVB2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVAVB3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVAVB4  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASO4I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASO4J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASO4K  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANH4I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANH4J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANH4K  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANO3I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANO3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANO3K  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANAI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ANAJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ACLI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ACLJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ACLK  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AISO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AISO2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASQTJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AORGCJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AECI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AECJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AOTHRI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AOTHRJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AFEJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AALJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASIJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ATIJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ACAJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMGJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AKJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMNJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ACORS  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASOIL  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASEACAT  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AISO3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AOLGAJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AOLGBJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AGLYJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMTNO3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMTHYDJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| APOCI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| APOCJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| APNCOMI  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| APNCOMJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| APCSOJ  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVPO1I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVPO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVPO1I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVPO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVPO2I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVPO2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVPO3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AIVPO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVOO1I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVOO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVOO2I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ALVOO2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVOO1I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVOO1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVOO2I  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVOO2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| ASVOO3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AAVB1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AAVB2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AAVB3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AAVB4J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT1J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT2J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT3J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT4J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT5J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| AMT6J  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NH3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVISO1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVISO2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVSQT  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT1  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT2  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT3  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT4  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT5  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| SVMT6  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| O3_BCT  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| HPLUS  | hourly wet deposition values | kg ha-1 | XYT | CCTM_WETDEP1 | True |
| NO2  | Variable NO2 | ppmV | XYZT | CCTM_CGRID | True |
| NO  | Variable NO | ppmV | XYZT | CCTM_CGRID | True |
| O  | Variable O | ppmV | XYZT | CCTM_CGRID | True |
| O3  | Variable O3 | ppmV | XYZT | CCTM_CGRID | True |
| NO3  | Variable NO3 | ppmV | XYZT | CCTM_CGRID | True |
| O1D  | Variable O1D | ppmV | XYZT | CCTM_CGRID | True |
| OH  | Variable OH | ppmV | XYZT | CCTM_CGRID | True |
| HO2  | Variable HO2 | ppmV | XYZT | CCTM_CGRID | True |
| H2O2  | Variable H2O2 | ppmV | XYZT | CCTM_CGRID | True |
| N2O5  | Variable N2O5 | ppmV | XYZT | CCTM_CGRID | True |
| HNO3  | Variable HNO3 | ppmV | XYZT | CCTM_CGRID | True |
| HONO  | Variable HONO | ppmV | XYZT | CCTM_CGRID | True |
| PNA  | Variable PNA | ppmV | XYZT | CCTM_CGRID | True |
| SO2  | Variable SO2 | ppmV | XYZT | CCTM_CGRID | True |
| SULF  | Variable SULF | ppmV | XYZT | CCTM_CGRID | True |
| SULRXN  | Variable SULRXN | ppmV | XYZT | CCTM_CGRID | True |
| C2O3  | Variable C2O3 | ppmV | XYZT | CCTM_CGRID | True |
| MEO2  | Variable MEO2 | ppmV | XYZT | CCTM_CGRID | True |
| RO2  | Variable RO2 | ppmV | XYZT | CCTM_CGRID | True |
| PAN  | Variable PAN | ppmV | XYZT | CCTM_CGRID | True |
| PACD  | Variable PACD | ppmV | XYZT | CCTM_CGRID | True |
| AACD  | Variable AACD | ppmV | XYZT | CCTM_CGRID | True |
| CXO3  | Variable CXO3 | ppmV | XYZT | CCTM_CGRID | True |
| ALD2  | Variable ALD2 | ppmV | XYZT | CCTM_CGRID | True |
| XO2H  | Variable XO2H | ppmV | XYZT | CCTM_CGRID | True |
| PANX  | Variable PANX | ppmV | XYZT | CCTM_CGRID | True |
| FORM  | Variable FORM | ppmV | XYZT | CCTM_CGRID | True |
| MEPX  | Variable MEPX | ppmV | XYZT | CCTM_CGRID | True |
| MEOH  | Variable MEOH | ppmV | XYZT | CCTM_CGRID | True |
| ROOH  | Variable ROOH | ppmV | XYZT | CCTM_CGRID | True |
| XO2  | Variable XO2 | ppmV | XYZT | CCTM_CGRID | True |
| XO2N  | Variable XO2N | ppmV | XYZT | CCTM_CGRID | True |
| XPAR  | Variable XPAR | ppmV | XYZT | CCTM_CGRID | True |
| XPRP  | Variable XPRP | ppmV | XYZT | CCTM_CGRID | True |
| NTR1  | Variable NTR1 | ppmV | XYZT | CCTM_CGRID | True |
| NTR2  | Variable NTR2 | ppmV | XYZT | CCTM_CGRID | True |
| FACD  | Variable FACD | ppmV | XYZT | CCTM_CGRID | True |
| CO  | Variable CO | ppmV | XYZT | CCTM_CGRID | True |
| HCO3  | Variable HCO3 | ppmV | XYZT | CCTM_CGRID | True |
| ALDX  | Variable ALDX | ppmV | XYZT | CCTM_CGRID | True |
| GLYD  | Variable GLYD | ppmV | XYZT | CCTM_CGRID | True |
| GLY  | Variable GLY | ppmV | XYZT | CCTM_CGRID | True |
| MGLY  | Variable MGLY | ppmV | XYZT | CCTM_CGRID | True |
| ETHA  | Variable ETHA | ppmV | XYZT | CCTM_CGRID | True |
| ETOH  | Variable ETOH | ppmV | XYZT | CCTM_CGRID | True |
| KET  | Variable KET | ppmV | XYZT | CCTM_CGRID | True |
| PAR  | Variable PAR | ppmV | XYZT | CCTM_CGRID | True |
| ACET  | Variable ACET | ppmV | XYZT | CCTM_CGRID | True |
| PRPA  | Variable PRPA | ppmV | XYZT | CCTM_CGRID | True |
| ROR  | Variable ROR | ppmV | XYZT | CCTM_CGRID | True |
| ETHY  | Variable ETHY | ppmV | XYZT | CCTM_CGRID | True |
| ETH  | Variable ETH | ppmV | XYZT | CCTM_CGRID | True |
| OLE  | Variable OLE | ppmV | XYZT | CCTM_CGRID | True |
| IOLE  | Variable IOLE | ppmV | XYZT | CCTM_CGRID | True |
| ISOP  | Variable ISOP | ppmV | XYZT | CCTM_CGRID | True |
| ISO2  | Variable ISO2 | ppmV | XYZT | CCTM_CGRID | True |
| ISOPRXN  | Variable ISOPRXN | ppmV | XYZT | CCTM_CGRID | True |
| ISPD  | Variable ISPD | ppmV | XYZT | CCTM_CGRID | True |
| INTR  | Variable INTR | ppmV | XYZT | CCTM_CGRID | True |
| ISPX  | Variable ISPX | ppmV | XYZT | CCTM_CGRID | True |
| HPLD  | Variable HPLD | ppmV | XYZT | CCTM_CGRID | True |
| OPO3  | Variable OPO3 | ppmV | XYZT | CCTM_CGRID | True |
| EPOX  | Variable EPOX | ppmV | XYZT | CCTM_CGRID | True |
| IEPOXP  | Variable IEPOXP | ppmV | XYZT | CCTM_CGRID | True |
| EPX2  | Variable EPX2 | ppmV | XYZT | CCTM_CGRID | True |
| TERP  | Variable TERP | ppmV | XYZT | CCTM_CGRID | True |
| APIN  | Variable APIN | ppmV | XYZT | CCTM_CGRID | True |
| TERPNRO2  | Variable TERPNRO2 | ppmV | XYZT | CCTM_CGRID | True |
| MTNO3  | Variable MTNO3 | ppmV | XYZT | CCTM_CGRID | True |
| TRPRXN  | Variable TRPRXN | ppmV | XYZT | CCTM_CGRID | True |
| BENZENE  | Variable BENZENE | ppmV | XYZT | CCTM_CGRID | True |
| CRES  | Variable CRES | ppmV | XYZT | CCTM_CGRID | True |
| BZO2  | Variable BZO2 | ppmV | XYZT | CCTM_CGRID | True |
| OPEN  | Variable OPEN | ppmV | XYZT | CCTM_CGRID | True |
| BENZRO2  | Variable BENZRO2 | ppmV | XYZT | CCTM_CGRID | True |
| TOL  | Variable TOL | ppmV | XYZT | CCTM_CGRID | True |
| TO2  | Variable TO2 | ppmV | XYZT | CCTM_CGRID | True |
| TOLRO2  | Variable TOLRO2 | ppmV | XYZT | CCTM_CGRID | True |
| XOPN  | Variable XOPN | ppmV | XYZT | CCTM_CGRID | True |
| XYLMN  | Variable XYLMN | ppmV | XYZT | CCTM_CGRID | True |
| XLO2  | Variable XLO2 | ppmV | XYZT | CCTM_CGRID | True |
| XYLRO2  | Variable XYLRO2 | ppmV | XYZT | CCTM_CGRID | True |
| NAPH  | Variable NAPH | ppmV | XYZT | CCTM_CGRID | True |
| PAHRO2  | Variable PAHRO2 | ppmV | XYZT | CCTM_CGRID | True |
| CRO  | Variable CRO | ppmV | XYZT | CCTM_CGRID | True |
| CAT1  | Variable CAT1 | ppmV | XYZT | CCTM_CGRID | True |
| CRON  | Variable CRON | ppmV | XYZT | CCTM_CGRID | True |
| OPAN  | Variable OPAN | ppmV | XYZT | CCTM_CGRID | True |
| ECH4  | Variable ECH4 | ppmV | XYZT | CCTM_CGRID | True |
| CL2  | Variable CL2 | ppmV | XYZT | CCTM_CGRID | True |
| CL  | Variable CL | ppmV | XYZT | CCTM_CGRID | True |
| HOCL  | Variable HOCL | ppmV | XYZT | CCTM_CGRID | True |
| CLO  | Variable CLO | ppmV | XYZT | CCTM_CGRID | True |
| FMCL  | Variable FMCL | ppmV | XYZT | CCTM_CGRID | True |
| HCL  | Variable HCL | ppmV | XYZT | CCTM_CGRID | True |
| CLNO2  | Variable CLNO2 | ppmV | XYZT | CCTM_CGRID | True |
| CLNO3  | Variable CLNO3 | ppmV | XYZT | CCTM_CGRID | True |
| SESQ  | Variable SESQ | ppmV | XYZT | CCTM_CGRID | True |
| SESQRXN  | Variable SESQRXN | ppmV | XYZT | CCTM_CGRID | True |
| SOAALK  | Variable SOAALK | ppmV | XYZT | CCTM_CGRID | True |
| H2NO3PIJ  | Variable H2NO3PIJ | ppmV | XYZT | CCTM_CGRID | True |
| H2NO3PK  | Variable H2NO3PK | ppmV | XYZT | CCTM_CGRID | True |
| VLVPO1  | Variable VLVPO1 | ppmV | XYZT | CCTM_CGRID | True |
| VSVPO1  | Variable VSVPO1 | ppmV | XYZT | CCTM_CGRID | True |
| VSVPO2  | Variable VSVPO2 | ppmV | XYZT | CCTM_CGRID | True |
| VSVPO3  | Variable VSVPO3 | ppmV | XYZT | CCTM_CGRID | True |
| VIVPO1  | Variable VIVPO1 | ppmV | XYZT | CCTM_CGRID | True |
| VLVOO1  | Variable VLVOO1 | ppmV | XYZT | CCTM_CGRID | True |
| VLVOO2  | Variable VLVOO2 | ppmV | XYZT | CCTM_CGRID | True |
| VSVOO1  | Variable VSVOO1 | ppmV | XYZT | CCTM_CGRID | True |
| VSVOO2  | Variable VSVOO2 | ppmV | XYZT | CCTM_CGRID | True |
| VSVOO3  | Variable VSVOO3 | ppmV | XYZT | CCTM_CGRID | True |
| PCVOC  | Variable PCVOC | ppmV | XYZT | CCTM_CGRID | True |
| PCSOARXN  | Variable PCSOARXN | ppmV | XYZT | CCTM_CGRID | True |
| FORM_PRIMARY  | Variable FORM_PRIMARY | ppmV | XYZT | CCTM_CGRID | True |
| ALD2_PRIMARY  | Variable ALD2_PRIMARY | ppmV | XYZT | CCTM_CGRID | True |
| BUTADIENE13  | Variable BUTADIENE13 | ppmV | XYZT | CCTM_CGRID | True |
| ACROLEIN  | Variable ACROLEIN | ppmV | XYZT | CCTM_CGRID | True |
| ACRO_PRIMARY  | Variable ACRO_PRIMARY | ppmV | XYZT | CCTM_CGRID | True |
| TOLU  | Variable TOLU | ppmV | XYZT | CCTM_CGRID | True |
| HG  | Variable HG | ppmV | XYZT | CCTM_CGRID | True |
| HGIIAER  | Variable HGIIAER | ppmV | XYZT | CCTM_CGRID | True |
| HGIIGAS  | Variable HGIIGAS | ppmV | XYZT | CCTM_CGRID | True |
| SVAVB1  | Variable SVAVB1 | ppmV | XYZT | CCTM_CGRID | True |
| SVAVB2  | Variable SVAVB2 | ppmV | XYZT | CCTM_CGRID | True |
| SVAVB3  | Variable SVAVB3 | ppmV | XYZT | CCTM_CGRID | True |
| SVAVB4  | Variable SVAVB4 | ppmV | XYZT | CCTM_CGRID | True |
| DMS  | Variable DMS | ppmV | XYZT | CCTM_CGRID | True |
| MSA  | Variable MSA | ppmV | XYZT | CCTM_CGRID | True |
| RHOJ  | Variable RHOJ | m kg m-3 | XYZT | CCTM_CGRID | True |
| ASO4I  | Variable ASO4I | ug m-3 | XYZT | CCTM_CGRID | True |
| ASO4J  | Variable ASO4J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASO4K  | Variable ASO4K | ug m-3 | XYZT | CCTM_CGRID | True |
| ANH4I  | Variable ANH4I | ug m-3 | XYZT | CCTM_CGRID | True |
| ANH4J  | Variable ANH4J | ug m-3 | XYZT | CCTM_CGRID | True |
| ANH4K  | Variable ANH4K | ug m-3 | XYZT | CCTM_CGRID | True |
| ANO3I  | Variable ANO3I | ug m-3 | XYZT | CCTM_CGRID | True |
| ANO3J  | Variable ANO3J | ug m-3 | XYZT | CCTM_CGRID | True |
| ANO3K  | Variable ANO3K | ug m-3 | XYZT | CCTM_CGRID | True |
| ANAI  | Variable ANAI | ug m-3 | XYZT | CCTM_CGRID | True |
| ANAJ  | Variable ANAJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ACLI  | Variable ACLI | ug m-3 | XYZT | CCTM_CGRID | True |
| ACLJ  | Variable ACLJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ACLK  | Variable ACLK | ug m-3 | XYZT | CCTM_CGRID | True |
| AISO1J  | Variable AISO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| AISO2J  | Variable AISO2J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASQTJ  | Variable ASQTJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AORGCJ  | Variable AORGCJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AECI  | Variable AECI | ug m-3 | XYZT | CCTM_CGRID | True |
| AECJ  | Variable AECJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AOTHRI  | Variable AOTHRI | ug m-3 | XYZT | CCTM_CGRID | True |
| AOTHRJ  | Variable AOTHRJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AFEJ  | Variable AFEJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AALJ  | Variable AALJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ASIJ  | Variable ASIJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ATIJ  | Variable ATIJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ACAJ  | Variable ACAJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AMGJ  | Variable AMGJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AKJ  | Variable AKJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AMNJ  | Variable AMNJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ACORS  | Variable ACORS | ug m-3 | XYZT | CCTM_CGRID | True |
| ASOIL  | Variable ASOIL | ug m-3 | XYZT | CCTM_CGRID | True |
| NUMATKN  | Variable NUMATKN | m-3 | XYZT | CCTM_CGRID | True |
| NUMACC  | Variable NUMACC | m-3 | XYZT | CCTM_CGRID | True |
| NUMCOR  | Variable NUMCOR | m-3 | XYZT | CCTM_CGRID | True |
| SRFATKN  | Variable SRFATKN | m2 m-3 | XYZT | CCTM_CGRID | True |
| SRFACC  | Variable SRFACC | m2 m-3 | XYZT | CCTM_CGRID | True |
| SRFCOR  | Variable SRFCOR | m2 m-3 | XYZT | CCTM_CGRID | True |
| AORGH2OJ  | Variable AORGH2OJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AH2OI  | Variable AH2OI | ug m-3 | XYZT | CCTM_CGRID | True |
| AH2OJ  | Variable AH2OJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AH2OK  | Variable AH2OK | ug m-3 | XYZT | CCTM_CGRID | True |
| AH3OPI  | Variable AH3OPI | ug m-3 | XYZT | CCTM_CGRID | True |
| AH3OPJ  | Variable AH3OPJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AH3OPK  | Variable AH3OPK | ug m-3 | XYZT | CCTM_CGRID | True |
| ASEACAT  | Variable ASEACAT | ug m-3 | XYZT | CCTM_CGRID | True |
| AISO3J  | Variable AISO3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AOLGAJ  | Variable AOLGAJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AOLGBJ  | Variable AOLGBJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AGLYJ  | Variable AGLYJ | ug m-3 | XYZT | CCTM_CGRID | True |
| AMTNO3J  | Variable AMTNO3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMTHYDJ  | Variable AMTHYDJ | ug m-3 | XYZT | CCTM_CGRID | True |
| APOCI  | Variable APOCI | ug m-3 | XYZT | CCTM_CGRID | True |
| APOCJ  | Variable APOCJ | ug m-3 | XYZT | CCTM_CGRID | True |
| APNCOMI  | Variable APNCOMI | ug m-3 | XYZT | CCTM_CGRID | True |
| APNCOMJ  | Variable APNCOMJ | ug m-3 | XYZT | CCTM_CGRID | True |
| APCSOJ  | Variable APCSOJ | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVPO1I  | Variable ALVPO1I | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVPO1J  | Variable ALVPO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVPO1I  | Variable ASVPO1I | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVPO1J  | Variable ASVPO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVPO2I  | Variable ASVPO2I | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVPO2J  | Variable ASVPO2J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVPO3J  | Variable ASVPO3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AIVPO1J  | Variable AIVPO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVOO1I  | Variable ALVOO1I | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVOO1J  | Variable ALVOO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVOO2I  | Variable ALVOO2I | ug m-3 | XYZT | CCTM_CGRID | True |
| ALVOO2J  | Variable ALVOO2J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVOO1I  | Variable ASVOO1I | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVOO1J  | Variable ASVOO1J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVOO2I  | Variable ASVOO2I | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVOO2J  | Variable ASVOO2J | ug m-3 | XYZT | CCTM_CGRID | True |
| ASVOO3J  | Variable ASVOO3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AAVB1J  | Variable AAVB1J | ug m-3 | XYZT | CCTM_CGRID | True |
| AAVB2J  | Variable AAVB2J | ug m-3 | XYZT | CCTM_CGRID | True |
| AAVB3J  | Variable AAVB3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AAVB4J  | Variable AAVB4J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT1J  | Variable AMT1J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT2J  | Variable AMT2J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT3J  | Variable AMT3J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT4J  | Variable AMT4J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT5J  | Variable AMT5J | ug m-3 | XYZT | CCTM_CGRID | True |
| AMT6J  | Variable AMT6J | ug m-3 | XYZT | CCTM_CGRID | True |
| NH3  | Variable NH3 | ppmV | XYZT | CCTM_CGRID | True |
| SVISO1  | Variable SVISO1 | ppmV | XYZT | CCTM_CGRID | True |
| SVISO2  | Variable SVISO2 | ppmV | XYZT | CCTM_CGRID | True |
| SVSQT  | Variable SVSQT | ppmV | XYZT | CCTM_CGRID | True |
| LVPCSOG  | Variable LVPCSOG | ppmV | XYZT | CCTM_CGRID | True |
| SVMT1  | Variable SVMT1 | ppmV | XYZT | CCTM_CGRID | True |
| SVMT2  | Variable SVMT2 | ppmV | XYZT | CCTM_CGRID | True |
| SVMT3  | Variable SVMT3 | ppmV | XYZT | CCTM_CGRID | True |
| SVMT4  | Variable SVMT4 | ppmV | XYZT | CCTM_CGRID | True |
| SVMT5  | Variable SVMT5 | ppmV | XYZT | CCTM_CGRID | True |
| SVMT6  | Variable SVMT6 | ppmV | XYZT | CCTM_CGRID | True |
| O3_BCT  | Variable O3_BCT | ppmV | XYZT | CCTM_CGRID | True |
| Gamma1  | aqueous phase concentration | mol L-1 | XYT | CCTM_MEDIA_CONC | True |
| Gamma2  | aqueous phase concentration | mol L-1 | XYT | CCTM_MEDIA_CONC | True |
| MHpsl1  | aqueous phase concentration | mol L-1 | XYT | CCTM_MEDIA_CONC | True |
| MHpsl2  | aqueous phase concentration | mol L-1 | XYT | CCTM_MEDIA_CONC | True |
| PTYPE  | NO emission pulse type | INTEGER | XYT | CCTM_BSOILOUT | True |
| PULSEDATE  | CMAQ starting date for NO emission pulse | YYYYDDD | XYT | CCTM_BSOILOUT | True |
| PULSETIME  | CMAQ starting time for NO emission pulse | HHMMSS | XYT | CCTM_BSOILOUT | True |
| RAINFALL01  | hrly cnv. & non-cnv. rainfall for 2022182:010000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL02  | hrly cnv. & non-cnv. rainfall for 2022182:020000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL03  | hrly cnv. & non-cnv. rainfall for 2022182:030000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL04  | hrly cnv. & non-cnv. rainfall for 2022182:040000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL05  | hrly cnv. & non-cnv. rainfall for 2022182:050000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL06  | hrly cnv. & non-cnv. rainfall for 2022182:060000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL07  | hrly cnv. & non-cnv. rainfall for 2022182:070000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL08  | hrly cnv. & non-cnv. rainfall for 2022182:080000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL09  | hrly cnv. & non-cnv. rainfall for 2022182:090000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL10  | hrly cnv. & non-cnv. rainfall for 2022182:100000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL11  | hrly cnv. & non-cnv. rainfall for 2022182:110000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL12  | hrly cnv. & non-cnv. rainfall for 2022182:120000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL13  | hrly cnv. & non-cnv. rainfall for 2022182:130000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL14  | hrly cnv. & non-cnv. rainfall for 2022182:140000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL15  | hrly cnv. & non-cnv. rainfall for 2022182:150000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL16  | hrly cnv. & non-cnv. rainfall for 2022182:160000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL17  | hrly cnv. & non-cnv. rainfall for 2022182:170000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL18  | hrly cnv. & non-cnv. rainfall for 2022182:180000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL19  | hrly cnv. & non-cnv. rainfall for 2022182:190000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL20  | hrly cnv. & non-cnv. rainfall for 2022182:200000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL21  | hrly cnv. & non-cnv. rainfall for 2022182:210000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL22  | hrly cnv. & non-cnv. rainfall for 2022182:220000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL23  | hrly cnv. & non-cnv. rainfall for 2022182:230000 | cm | XYT | CCTM_BSOILOUT | True |
| RAINFALL24  | hrly cnv. & non-cnv. rainfall for 2022183:000000 | cm | XYT | CCTM_BSOILOUT | True |
| ISOP  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| OLE  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| PAR  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| MEOH  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| APIN  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| TERP  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ETH  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ETOH  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ACET  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ALDX  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| IOLE  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| FORM  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| FORM_PRIMARY  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ALD2  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ALD2_PRIMARY  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| ETHA  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| FACD  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| AACD  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| KET  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| CO  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| SESQ  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| NO  | biogenic emissions of the indicated species | gm s-1 | XYT | CCTM_B3GTS_S | True |
| VD_SO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SULF  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NO  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_O3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HNO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_H2O2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ALD  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HCHO  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_OP  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PAA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ORA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NH3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PAN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HONO  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CO  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_METHANOL  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_N2O5  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_GEN_ALD  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CL2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HOCL  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HCL  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_FMCL  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HGIIGAS  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_HG  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_MPAN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PPN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ISPD  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NTRALK  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NTROH  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CLNO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_MTNO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_IEPOX  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVISO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVISO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVSQT  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_DMS  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_MSA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_METHANE  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_METH_NIT_PHEN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PCVOC  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_INTR  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ISPX  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ROOH  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_LVPCSOG  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VIVPO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VLVOO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VLVOO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VLVPO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVOO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVOO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVOO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVPO1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVPO2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_VSVPO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_FACD  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_KET  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ETH  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PNA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_GLY  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_GLYD  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_MGLY  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ETHA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ETOH  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PAR  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ACET  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_PRPA  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ETHY  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_OLE  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_IOLE  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_BENZ  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CRES  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_TOL  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_XYLMN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_NAPH  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CAT1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SESQ  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_TERP  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ISOP  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_OPEN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_XOPN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SOAALK  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_BUTADIENE13  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_ACROLEIN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT4  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT5  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVMT6  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVAVB1  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVAVB2  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVAVB3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_SVAVB4  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CLNO3  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VD_CLO  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VMASSI  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VMASSJ  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VMASSC  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VNUMATKN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VNUMACC  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VNUMCOR  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VSRFATKN  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VSRFACC  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| VSRFCOR  | Dry deposition velocity | cm s-1 | XYT | CCTM_DEPV | True |
| NO  | hourly average NO produced from lightning | mol s-1 | XYZT | CCTM_LTNGHRLY | True |
| NO  | Column NO produced from lightning | mol s-1 | XYT | CCTM_LTNGCOL | True |
| COSZENS  | Cosine of Solar Zenith Angle |  | XYT | CCTM_PHOTDIAG1 | True |
| OZONE_COLUMN  | Observed Total Ozone Column Density | DU | XYT | CCTM_PHOTDIAG1 | True |
| NO2_COLUMN  | Predicted nitrogen dioxide column density | petamolec cm-2 | XYT | CCTM_PHOTDIAG1 | True |
| CO_COLUMN  | Predicted carbon monoxide column density | petamolec cm-2 | XYT | CCTM_PHOTDIAG1 | True |
| SO2_COLUMN  | Predicted sulfur dioxide column density | petamolec cm-2 | XYT | CCTM_PHOTDIAG1 | True |
| HCHO_COLUMN  | Predicted formaldehyde column density | petamolec cm-2 | XYT | CCTM_PHOTDIAG1 | True |
| TROPO_O3_COLUMN  | Predicted Tropospheric Ozone Column density | DU | XYT | CCTM_PHOTDIAG1 | True |
| JNO2  | Photodissociation rate of NO2 | min-1 | XYT | CCTM_PHOTDIAG1 | True |
| JO3O1D  | Photodissociation rate of ozone producing O(1D) | min-1 | XYT | CCTM_PHOTDIAG1 | True |
| RESOLVED_CFRAC  | Resolved Cloud Fraction averaged over cloudy layers | 1 | XYT | CCTM_PHOTDIAG1 | True |
| RESOLVED_WBAR  | Resolved Cloud Hydrometeor Content averaged over cloudy layers | g m-3 | XYT | CCTM_PHOTDIAG1 | True |
| SUBGRID_CFRAC  | Subgrid Cloud Fraction averaged over cloudy layers | 1 | XYT | CCTM_PHOTDIAG1 | True |
| SUBGRID_WBAR  | Subgrid Cloud Hydrometeor Content averaged over cloudy layers | g m-3 | XYT | CCTM_PHOTDIAG1 | True |
| TRANS_DIFFUSE  | broad band transmission coefficient for diffuse radiation at surface | 1 | XYT | CCTM_PHOTDIAG1 | True |
| TRANS_DIRECT  | broad band transmission coefficient for direct radiation at surface | 1 | XYT | CCTM_PHOTDIAG1 | True |
| REFLECTION  | broad band reflection coefficient at top of atmosphere | 1 | XYT | CCTM_PHOTDIAG1 | True |
| CLR_TRANS_DIF  | broad band diffuse transmission for clear sky at surface | 1 | XYT | CCTM_PHOTDIAG1 | True |
| CLR_TRANS_DIR  | broad band direct transmission for clear sky at surface | 1 | XYT | CCTM_PHOTDIAG1 | True |
| CLR_REFLECTION  | broad band reflection for clear sky at top of atmosphere | 1 | XYT | CCTM_PHOTDIAG1 | True |
| TROPO_O3_EXCEED  | Average Exceedance of modeled ozone column from max fraction of Total Column,  a | 1 | XYT | CCTM_PHOTDIAG1 | True |
| N_EXCEED_TROPO3  | # of times predicted tropospheric ozone column exceeds observed total column per |  | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W294  | Total Downward Irradiance at surface at 294 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W294  | Total Aerosol Optical Depth at 294 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W294  | Absorption Aerosol Optical Depth at 294 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W294  | Cloud Optical Depth at 294 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W294  | Total Optical Depth at294 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W294  | Optical Depth of O3 above model domain at 294 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W294  | Surface Albedo at the wavelength at 294 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W303  | Total Downward Irradiance at surface at 303 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W303  | Total Aerosol Optical Depth at 303 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W303  | Absorption Aerosol Optical Depth at 303 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W303  | Cloud Optical Depth at 303 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W303  | Total Optical Depth at303 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W303  | Optical Depth of O3 above model domain at 303 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W303  | Surface Albedo at the wavelength at 303 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W310  | Total Downward Irradiance at surface at 310 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W310  | Total Aerosol Optical Depth at 310 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W310  | Absorption Aerosol Optical Depth at 310 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W310  | Cloud Optical Depth at 310 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W310  | Total Optical Depth at310 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W310  | Optical Depth of O3 above model domain at 310 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W310  | Surface Albedo at the wavelength at 310 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W316  | Total Downward Irradiance at surface at 316 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W316  | Total Aerosol Optical Depth at 316 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W316  | Absorption Aerosol Optical Depth at 316 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W316  | Cloud Optical Depth at 316 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W316  | Total Optical Depth at316 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W316  | Optical Depth of O3 above model domain at 316 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W316  | Surface Albedo at the wavelength at 316 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W333  | Total Downward Irradiance at surface at 333 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W333  | Total Aerosol Optical Depth at 333 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W333  | Absorption Aerosol Optical Depth at 333 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W333  | Cloud Optical Depth at 333 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W333  | Total Optical Depth at333 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W333  | Optical Depth of O3 above model domain at 333 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W333  | Surface Albedo at the wavelength at 333 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W381  | Total Downward Irradiance at surface at 381 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W381  | Total Aerosol Optical Depth at 381 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W381  | Absorption Aerosol Optical Depth at 381 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W381  | Cloud Optical Depth at 381 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W381  | Total Optical Depth at381 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W381  | Optical Depth of O3 above model domain at 381 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W381  | Surface Albedo at the wavelength at 381 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| ETOT_SFC_W607  | Total Downward Irradiance at surface at 607 nm | W m-2 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W607  | Total Aerosol Optical Depth at 607 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| AOD_ABS_W607  | Absorption Aerosol Optical Depth at 607 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_CLOUD_W607  | Cloud Optical Depth at 607 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAU_TOT_W607  | Total Optical Depth at607 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| TAUO3_TOP_W607  | Optical Depth of O3 above model domain at 607 nm |  | XYT | CCTM_PHOTDIAG1 | True |
| ALBEDO_W607  | Surface Albedo at the wavelength at 607 nm | 1 | XYT | CCTM_PHOTDIAG1 | True |
| AOD_W550_ANGST  | Aerosol Optical Depth at 550 nm based on an Angstrom Interpolation |  | XYT | CCTM_PHOTDIAG1 | True |
| AAOD_W550_ANGST  | Aerosol Absorption Optical Depth at 550 nm based on an Angstrom Interpolation |  | XYT | CCTM_PHOTDIAG1 | True |
| NO2_IUPAC10  | Photolysis rates calculated based on data file; NO2_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| O3_O3P_IUPAC10  | Photolysis rates calculated based on data file; O3_O3P_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| O3_O1D_IUPAC10  | Photolysis rates calculated based on data file; O3_O1D_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| H2O2_IUPAC10  | Photolysis rates calculated based on data file; H2O2_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| NO3NO2_06  | Photolysis rates calculated based on data file; NO3NO2_06 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| NO3NO_06  | Photolysis rates calculated based on data file; NO3NO_06 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| N2O5_IUPAC10  | Photolysis rates calculated based on data file; N2O5_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| HONO_IUPAC10  | Photolysis rates calculated based on data file; HONO_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| HNO3_IUPAC10  | Photolysis rates calculated based on data file; HNO3_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| PNA_IUPAC10  | Photolysis rates calculated based on data file; PNA_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| PAN_IUPAC10  | Photolysis rates calculated based on data file; PAN_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| MEPX_IUPAC10  | Photolysis rates calculated based on data file; MEPX_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| NTR_IUPAC10  | Photolysis rates calculated based on data file; NTR_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| FORM_R_IUPAC13  | Photolysis rates calculated based on data file; FORM_R_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| FORM_M_IUPAC13  | Photolysis rates calculated based on data file; FORM_M_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| ALD2_R_IUPAC13  | Photolysis rates calculated based on data file; ALD2_R_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| ALDX_R_IUPAC13  | Photolysis rates calculated based on data file; ALDX_R_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| GLYD_IUPAC13  | Photolysis rates calculated based on data file; GLYD_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| GLY_R_IUPAC13  | Photolysis rates calculated based on data file; GLY_R_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| MGLY_IUPAC10  | Photolysis rates calculated based on data file; MGLY_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| KET_IUPAC10  | Photolysis rates calculated based on data file; KET_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| ACET_IUPAC10  | Photolysis rates calculated based on data file; ACET_IUPAC10 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| ISPD  | Photolysis rates calculated based on data file; ISPD | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| HPALD  | Photolysis rates calculated based on data file; HPALD | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| CL2_IUPAC04  | Photolysis rates calculated based on data file; CL2_IUPAC04 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| HOCL_IUPAC04  | Photolysis rates calculated based on data file; HOCL_IUPAC04 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| FMCL_IUPAC04  | Photolysis rates calculated based on data file; FMCL_IUPAC04 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| CLNO2_IUPAC13  | Photolysis rates calculated based on data file; CLNO2_IUPAC13 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| CLONO2_1  | Photolysis rates calculated based on data file; CLONO2_1 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| CLONO2_2  | Photolysis rates calculated based on data file; CLONO2_2 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| IC3ONO2  | Photolysis rates calculated based on data file; IC3ONO2 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| ACRO_09  | Photolysis rates calculated based on data file; ACRO_09 | min-1 | XYT | CCTM_PHOTDIAG2 | True |
| AERO_SCAT_W294  | Aerosol Scattering of layer at 294 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W294  | Aerosol Asymmetry Factor at 294 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W294  | Total Extinction of layer for 294 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W294  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 294 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W294  | Aerosol Extinction in layer for 294 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W294  | Net Actinic Flux, 294 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W303  | Aerosol Scattering of layer at 303 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W303  | Aerosol Asymmetry Factor at 303 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W303  | Total Extinction of layer for 303 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W303  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 303 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W303  | Aerosol Extinction in layer for 303 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W303  | Net Actinic Flux, 303 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W310  | Aerosol Scattering of layer at 310 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W310  | Aerosol Asymmetry Factor at 310 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W310  | Total Extinction of layer for 310 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W310  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 310 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W310  | Aerosol Extinction in layer for 310 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W310  | Net Actinic Flux, 310 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W316  | Aerosol Scattering of layer at 316 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W316  | Aerosol Asymmetry Factor at 316 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W316  | Total Extinction of layer for 316 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W316  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 316 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W316  | Aerosol Extinction in layer for 316 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W316  | Net Actinic Flux, 316 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W333  | Aerosol Scattering of layer at 333 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W333  | Aerosol Asymmetry Factor at 333 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W333  | Total Extinction of layer for 333 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W333  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 333 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W333  | Aerosol Extinction in layer for 333 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W333  | Net Actinic Flux, 333 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W381  | Aerosol Scattering of layer at 381 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W381  | Aerosol Asymmetry Factor at 381 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W381  | Total Extinction of layer for 381 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W381  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 381 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W381  | Aerosol Extinction in layer for 381 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W381  | Net Actinic Flux, 381 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_SCAT_W607  | Aerosol Scattering of layer at 607 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| AERO_ASYM_W607  | Aerosol Asymmetry Factor at 607 nm |  | XYT | CCTM_PHOTDIAG3 | True |
| EXT_W607  | Total Extinction of layer for 607 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| GAS_EXT_W607  | Total Extinction from Rayleigh scattering NO2 and O3 in layer for 607 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W607  | Aerosol Extinction in layer for 607 nm | Km-1 | XYT | CCTM_PHOTDIAG3 | True |
| ACTINIC_FX_W607  | Net Actinic Flux, 607 nm | W m-2 | XYT | CCTM_PHOTDIAG3 | True |
| CFRAC_3D  | Resolved Cloud Fraction in grid cell | 1 | XYT | CCTM_PHOTDIAG3 | True |
| EXT_AERO_W550  | Aerosol Extinction of layer for 550 nm based on an Angstrom Interpolation | Km-1 | XYT | CCTM_PHOTDIAG3 | True |

<!-- BEGIN COMMENT -->

[<< Previous Chapter](CMAQ_UG_ch06_model_configuration_options.md) - [Home](README.md) - [Next Chapter >>](CMAQ_UG_ch08_analysis_tools.md)<br>
CMAQv6.0 User's Guide <br>

<!-- END COMMENT -->

<!-- START_OF_COMMENT -->

[link_7_nml]: ../../CCTM/src/MECHS/cb6r3_ae7_aq/AE_cb6r3_ae7_aq.nml  

<!-- END_OF_COMMENT -->

[link_7_nml]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/MECHS/cb6r3_ae7_aq/AE_cb6r3_ae7_aq.nml  
