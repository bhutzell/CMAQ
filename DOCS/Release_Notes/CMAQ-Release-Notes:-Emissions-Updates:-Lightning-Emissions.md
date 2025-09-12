### Lightning Emissions Science Update
[Daiwen Kang](mailto:kang.daiwen@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science update and code maintenance  
**Release Version/Date**: CMAQv6.0  

**Description**:   
- Cosmetic code changes to use more meaningful names for environmental variables and variables in the code.
- Removed the external lightning NOx emissions file in the online lightning NOx module, because this option can be now easily achieved using DESID's capability.
- Added more options for using satellite GLM flashes/energy and the synergized methodology with WWLLN data. These options have been tested with preliminary evaluations with the CMAQv6.0 platform. The default option is the same as before and using it will not impact mode results. 

**Significance and Impact**:   
The default option will not impact model concentrations.  Consistent and meaningful names for environmental variables and code variables are beneficial for users and developers. Removing redundant statements or duplicated codes makes the scripts/code easy to understand and less error-prone. Other lightning NOx production options involves GLM data are implemented as place holder and will be fully developed and tested when the 2022/2023 simulation platform is available, though the standalone code for these options have been tested.

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
