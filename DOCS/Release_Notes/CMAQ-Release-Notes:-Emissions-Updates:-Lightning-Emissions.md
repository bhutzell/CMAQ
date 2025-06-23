### Lightning Emissions Science Update
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science update and code maintenance  
**Release Version/Date**: CMAQv6.0  

**Description**:   
- Cosmetic code changes to use more meaningful names for environmental variables and variables in the code.
- Removed the external lightning NOx emissions file in the online lightning NOx module, because this option can be now easily achieved using DESID's capability.
- Added more options for using satellite GLM flashes/energy and the synergized methodology with WWLLN flashes/energy. These options are fully developed and tested using the 2022v1/12US1/CRACMM3 CMAQv5.5 platform. The environmental variable LNO_OPTION can take 5 values: 1-5.
  1. Default option, the same as with previouse releases.
  2. Use only flash data from GLM (the near realtime satellite datasets, good for air quality forecast and near realtime applications)
  3. Use GLM energy to produce LNOx emissions (https://doi.org/10.1029/2022JD037406).
  4. Snergizing GLM and WWLLN energy (reduce limitation of data detection from single network).
  5. Snerigzing GLM and WWLLNs energy with ICCG treatment in the case of WWLLN energy is ued.   

**Significance and Impact**:   
The default option will not impact model concentrations.  Consistent and meaningful names for environmental variables and code variables are beneficial for users and developers. Removing redundant statements or duplicated codes makes the scripts/code easy to understand and less error-prone. Lightning NOx production options involves GLM and WWLLN data to provide users for more flexibility for different data/feature options. These options have been tested with annual simuations and evaluated for the impact on air qualtiy and the results are being analyzed.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1169](https://github.com/USEPA/CMAQ/commit/d5c78138ad8ccd78b99b41971f59a51a664009c6) | [PR#1169](https://github.com/USEPA/CMAQ_Dev/pull/1169)  |

### Lightning Emissions Bug Fix
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.4  
**Description**:  
The time steps in the lightning NOx diagnostic files were from 1 t0 24 in the previous versions. For CMAQv5.4, the time steps for the diagnostic files are from 0 to 23 in agreement with other output files.    
**Significance and Impact**: No impact on results.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#819](https://github.com/USEPA/CMAQ/commit/a3626ee9e8b60f9b0526d942f4fd44a4e9db0fc5) | [PR#819](https://github.com/USEPA/CMAQ_Dev/pull/819)  |
