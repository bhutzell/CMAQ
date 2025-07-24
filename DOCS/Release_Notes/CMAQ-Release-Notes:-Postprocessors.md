# Post-processors

## [sitecmp](../../POST/sitecmp/README.md) 
### Update handling of CASTNET QA Flags for ozone, increase number of species allowed in expressions 
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov)], U.S. Environmental Protection Agency    
**Type of update**: Improved functioality 
**Release Version/Date**:  CMAQv6.0 

**Description**:   

The code changes make two updates to the functionality of the **sitecmp** post-processing tool.

- Update the CASTNET O3 QA flags recognized by sitecmp. Starting in 2019, the AMET-ready CASTNET hourly files with meteorology and ozone observations use additional QA flags for ozone (H, J, and Y) that are currently not recognized by sitecmp when checking for QA flags. Since these flags indicate invalid observations, not screening for them can lead to sporadic errors in model evalulation by including invalid observations in the analysis.
- Increase the number of species allowed in the expressions that define the observations and model values to be matched. In the current code, the maximum number of observed and modeled variables allowed in sitecmp species matching expressions is 20. This number is insufficient when defining pairs for the new AMET AQS_Daily_VOC network used in CRACMM evaluations because some of these pairs include more than 20 individual observed compounds. The code update increases the maximum number of allowed species to 50.

**Significance and Impact**:  
In a test case for summer 2022, updating the CASTNET O3 QA flags recognized by **sitecmp** was found to have only a small effect on domain-wide model performance statistics (changing the bias by 0.1 ppb), but locally the impacts of not fully screening for observations flagged as invalid can be more pronounced and will depend on the time period and domain to be modeled. The increase in the number of species allowed in expressions defining observation/model pairs allows the evaluation of additional VOC species simulated by CRACMM.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1376 (URL to be added)]() | [PR#1376](https://github.com/USEPA/CMAQ_Dev/pull/1376)  |  


## [sitecmp_dailyo3](../../POST/sitecmp_dailyo3/README.md) 
### Update handling of CASTNET QA Flags for ozone 
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov)], U.S. Environmental Protection Agency    
**Type of update**: Improved functioality 
**Release Version/Date**:  CMAQv6.0 

**Description**:   

The code changes update the CASTNET O3 QA flags recognized by sitecmp_dailyo3. Starting in 2019, the AMET-ready CASTNET hourly files with meteorology and ozone observations use additional QA flags for ozone (H, J, and Y) that are currently not recognized by sitecmp_dailyo3 when checking for QA flags. Since these flags indicate invalid observations, not screening for them can lead to sporadic errors in model evalulation by including invalid observations in the analysis.

**Significance and Impact**:  
In a test case for summer 2022, updating the CASTNET O3 QA flags recognized by **sitecmp_dailyo3** was found to have only a small effect on domain-wide model performance statistics (changing the bias by 0.1 ppb), but locally the impacts of not fully screening for observations flagged as invalid can be more pronounced and will depend on the time period and domain to be modeled. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1376 (URL to be added)]() | [PR#1376](https://github.com/USEPA/CMAQ_Dev/pull/1376)  |  


## [calc_tmetric](../../POST/calc_tmetric/README.md) 
### Expand functionality of calc_tmetric
[William T. Hutzell](mailto:hutzell.bill@epa.gov)], U.S. Environmental Protection Agency    
**Type of update**: New Feature 
**Release Version/Date**:  CMAQv6.0 

**Description**:   

The updates attempts to expand functions of the **calc_tmetric** post-processing tool by the below changes.

- Produce output files with statistical metrics over a period equal to N time steps based on the input file(s). The metrics are sequential over time with a frequency one over N. The value of N is defined at run time.
- Increase possible metrics to include maximum, minimum, and range over the selected period.  
- Improve efficiency in processing large grid files covering days to weeks by calculating metrics for input file(s) variables in parallel rather than in serial.  
- Replace I/O API functions for getting environment variables.
- Update Fortran syntax closer to the current standard.  

The goal seeks to ease visualizing (e.g., via VERDI) or analyzing (e.g., via R) large data file(s) such as combine extracts or CMAQ output files coverings days to weeks.

**Significance and Impact**:  
Make **calc_tmetric** a more useful tool for examining CMAQ inputs or output files from long simulations.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1226](https://github.com/USEPA/CMAQ/commit/6847b80ad4ffa39f2cd13b748a704b7f064312c1) | [PR#1226](https://github.com/USEPA/CMAQ_Dev/pull/1226)  |  

No changes were made to this tool in CMAQv5.5.


## [combine](../../POST/combine/README.md)
### Allow compilation of combine with gcc10+
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Improve code robustness   
**Release Version/Date**: CMAQv6.0

**Description**:   
Allows the compilation of combine with gcc compiler versions 10 and higher without having to resort to using the "-fallow-argument-mismatch" compiler flag

**Significance and Impact**:    
The code update removes lines that prevented the code from compiling with gcc compiler versions 10 and higher. The removed lines were only invoked when using a wrfout file as one of the input files to combine, and the vertical grid information of the wrfout file obtained by the removed function calls in these lines was not actually used by combine in any way. Therefore, removing these lines does not impact any output files but does allow the code to compile.  


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1159](https://github.com/USEPA/CMAQ/commit/5e9753318a3708546298879b68b2ca0ef2dc4be3) | [PR#1159](https://github.com/USEPA/CMAQ_Dev/pull/1159)  |  

### Correct cadmium in SpecDef_Conc_cb6r5hap_ae7_aq.txt   
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**:  CMAQv6.0

**Description**:   
The COMBINE concentration definitions file for cb6r5hap uses ACD for aerosol cadmium but the aerosol species' bulk name has changed ACADMIUM. This update corrects the model species name in the   SpecDef_Conc_cb6r5hap_ae7_aq.txt.   

**Significance and Impact**:   
The update allows using the  SpecDef_Conc_cb6r5hap_ae7_aq.txt file for COMBINE processing of CCTM output files.

**Internal PRs**: 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1213](https://github.com/USEPA/CMAQ/commit/bad219d39b65205052d2768c1e9c0c5f9eccbdb4) | [PR#1213](https://github.com/USEPA/CMAQ_Dev/pull/1213)  |  



### Improve Checks on Formulas Used by COMBINE
[William T. Hutzell](mailto:Hutzell.Bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: The update to COMBINE detects and reports syntax errors in a species definitions files listed below. Error messages list the syntax error so the user can more easily correct the definitions file. Note that COMBINE stops at the first detected error so correcting a species definitions file is an iterative process if the file contains several errors.   
````
O3_ERROR1 , ppmV, O3[1]/       
O3_ERROR2 , ppmV, O3[1]\*    
O3_ERROR3 , ppmV, O3[1]+
O3_ERROR4 , ppmV, O3[1].1000.   
O3_ERROR5 , ppmV, O3[1]y1000.
O3_ERROR5 , ppmV, O3[1]O3[1]    
````     

**Significance and Impact**: Prevents Errors in Results from Postprocessing by COMBINE  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1091](https://github.com/USEPA/CMAQ/commit/b4ff5c9631caa1361593656c38b70715211e11f7) | [PR#1091](https://github.com/USEPA/CMAQ_Dev/pull/1091)  |  

### Corrected Deposition Species Definition (SpecDef_Dep) files for missing nitrogen species
[Jesse Bash](mailto:Bash.Jesse@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: There are a couple of issues with the SpecDef_Dep files resulting in the underestimation of total nitrogen deposition. The following changes were made to be more consistent with new NADP wet deposition and field scale dry deposition measurements: 
1. HNO4 (PNA) was missing from oxidized dry deposition 
2. NO3 radicle was omitted from oxidized dry and wet deposition
3. CLNO2 and CLNO3 were omitted from oxidized dry and wet deposition   
4. cb6 was missing MTNO3J in the organic N deposition
5. PANT was not added to the organic N deposition 

**Significance and Impact**: This does not change model results and only modifies post processing. The impact on post processed wet deposition results are minimal. The omissions from the dry deposition variables can amount to about a 1% increase in the post processed results. However, the largest increases are in remote vegetated and coastal areas which tend to be more sensitive to nitrogen loading.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1063](https://github.com/USEPA/CMAQ/commit/24c0840315978f94541d8b6288163e7a54c8694d) | [PR#1063](https://github.com/USEPA/CMAQ_Dev/pull/1063)  | 

### CRACMM SpecDef Deposition Updates for HNO4 and CLNO2
[Havala Pye](mailto:Pye.Havala@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: Removes any formulas that include "CLNO2", CRACMM does not have "CLNO2". Fixes any formulas that contain "PNA", which is named "HNO4" in CRACMM.   
**Significance and Impact**: Enables automated post processing of deposition via combine. Does not affect model results.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#966](https://github.com/USEPA/CMAQ/commit/1433cdb44cdc64b4f8209cf388c034510b18856e) | [PR#966](https://github.com/USEPA/CMAQ_Dev/pull/966)  |  

## [hr2day](../../POST/hr2day/README.md)
### Updating tz.csv to Natural Earth  
**Primary Contact**: [Barron Henderson](mailto:henderson.barron@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Change in input file  
**Release Version/Date**: CMAQv5.5  

**Description**: The tz.csv file is used to assign a time zone offset to each CMAQ grid cell.  This is then used by hr2day to calculate daily metrics in Local Standard Time (LST).  The previous tz.csv seems to have unrealistic boundaries and what must be typos in other places. In addition, the tz.csv file has origins that are lost to history. We do not know on what database it was founded on or how it was converted.

The previous tz.csv file was compared to two other time zone databases: tz_world.geojson [1] and Natural Earth [2]. The other databases were found to be more consistent in terms of the time zone boundaries. In addition, there were several locations in the western US in the previous tz.csv file with an offset of -5 UTC in otherwise Mountain time zones, which were likely typos.  

A new tz.csv file was created from the Natural Earth 10m time zone shapefile (v4.1.0)[2]. The new tz.csv file is 2.5 MB, compared to the 4 MB original. The new file is smaller because it does not attempt to hug coastal boundaries, but instead allows for time zones that extend into the water to do so.  

**Significance and Impact**: This will not affect model concentrations, but it will impact h2day calculations because small areas have updated time zones. This tends to matter most along the edges of time zones.  The original, new, and difference in "TZ hours behind UTC" are shown below.  

<img width="523" alt="image" src="https://github.com/user-attachments/assets/cf3ab1fb-fa09-48c9-ba7e-c21a18434121">

**References**:  
[1] http://efele.net/maps/tz/world/  
[2] https://www.naturalearthdata.com/downloads/10m-cultural-vectors/timezones/  
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1017](https://github.com/USEPA/CMAQ/commit/d7d24f8f2ca536353fc9983fd4301319da8fbead) | [PR#1017](https://github.com/USEPA/CMAQ_Dev/pull/1017)  |   

### Clarification of W126 Daily Index Computation and Minor Code Corrections
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: documentation, minor bug fix  
**Release Version/Date**: CMAQv5.4  
**Description**: Updated the README and inline code documentation to clarify that the W126 option computes the W126 daily index value as a weighted average of ozone concentrations between 8am & 7pm and that these daily index values are only an intermediate step in computing the W126 metric for secondary ozone standard analyses. These daily index values can then be used to calculate annual W126 values for the secondary ozone standard by using different tools to first compute 3-month sums of daily W126 index values and then determine the consecutive three month period with the largest 3-month sum of daily W126 values. There were also minor code corrections that fixed two issues that may have caused problems for certain compilers in debug mode but did not affect the results of the computations in the tests conducted.   
**Significance and Impact**: No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#859](https://github.com/USEPA/CMAQ/commit/3fbf66df2a123de5d4cc4c75352c08016a1123d2) | [PR#859](https://github.com/USEPA/CMAQ_Dev/pull/859)  |    

## [sitecmp](../../POST/sitecmp/README.md)
 No changes were made to this tool in CMAQv5.5.

## [sitecmp_dailyo3](../../POST/sitecmp_dailyo3/README.md)
### Increase of station ID character limit  
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: expand code functionality    
**Release Version/Date**: CMAQv5.5  
**Description**: The previous version of sitecmp_dailyo3 code imposed a maximum station ID character limit of 9. While sufficient for all observational networks previously processed through sitecmp_dailyo3, this constraint caused problems when processing emerging networks with hourly data which had station IDs exceeding that limit. The increase of the maximum station ID length to 20 remedies this problem and makes the limit consistent with the one being used in sitecmp.    
**Significance and Impact**:
There is no impact on results for stations with IDs not exceeding 9 characters. When the previous code encountered a station ID with more than 9 characters, each such site was processed and written out 24 times per day (rather than just once), and the observed daily metrics were missing for each of these 24 output records per day. The modeled daily metrics were correct and repeated for each of the 24 output records written for each day. Due to this 24-fold repetition of matched records for each day in such cases, the output files created with the previous code were substantially larger than they should have been, and they also contained no valid observed metrics at such stations. The updated code corrects this behavior.  
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#987](https://github.com/USEPA/CMAQ/commit/8d67a79d9cfd45bf421fcc864f3c0c70960df17b) | [PR#987](https://github.com/USEPA/CMAQ_Dev/pull/987)  |    

## Removal of appendwrf, bldoverlay and blockextract  
**Type of update**: Model Clean-up  
**Release Version/Date**: CMAQv5.4    
**Description**: 
The POST tools appendwrf, bldoverlay, and blockextract are no longer maintained and have been removed from the CMAQ code repository beginning with version 5.4. These tools can still be accessed through previous CMAQ versions.
