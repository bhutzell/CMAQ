# Wind Blown Dust Emissions
### Brown vegetation added to Windblown Dust Module
[Jeff Willison](mailto:willison.jeff@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Science Update 
**Release Version/Date**:  CMAQv6.0

**Description**:   
Brown (or non-photosynthetically active) vegetation, like photosynthetically active vegetation, creates a sheltering effect that reduces windblown dust emissions. In this update, the vegetation fraction, the vegetation height, and the vegetation roughness are updated using the MODIS spectral mixture analysis (SMA) following the method described by [Huang and Foroutan](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021JD035243) (2022, HF22).

**Significance and Impact**:   
The addition of NPV to the windblown dust model reduces 2022 annual dust emissions from 1909 Tg to 598 Tg for the Northern Hemisphere. This means we've gone from possibly a bit high to probably a bit low. Annual average distribution shows the decrease as a result of including NPV (left) relative to the base simulation (right).

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1336](https://github.com/USEPA/CMAQ/commit/c2b3f39eec76029f29e4895aab9c8d2bec3d8c6f) | [PR#1336](https://github.com/USEPA/CMAQ_Dev/pull/1336)  |


### Correction for NLCD40 Land Use Mapping in Windblown Dust Module
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Bug Fix    
**Release Version/Date**:  CMAQv6.0 (first released with [5.5.0.1](https://github.com/USEPA/CMAQ/releases/tag/CMAQv5.5.01_19Mar2025))  

**Description**:   
Resolves excessive inline windblown dust (WBD) emissions when using WRF simulations with NLCD40 land use (LU). The excessive emissions were caused by mapping two NLCD40 categories (“shrub/scrub” and “dwarf scrub” to the wrong internal BELD3 LU class (“barren or sparsely vegetated” instead of “shrubland”) that is being used in the WBD module.

**Significance and Impact**:   
Without this fix, users enabling the inline WBD module and using WRF simulations with NLCD40 LU as input to their CCTM simulations will likely experience excessive contributions from WBD emissions to PM2.5 mass concentrations. Over a 12km contiguous modeling domain, the effect was found to be most pronounced during springtime over the Southwestern U.S. Testing of the bug fix for 2018 with two different configurations of WRF using NLCD40 LU showed that annual total WBD emissions over the modeling domain were reduced by a factor of 3-4.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1181](https://github.com/USEPA/CMAQ/commit/b506277bdf8aaedc567348b8604e2bb854f3a707) | [PR#1181](https://github.com/USEPA/CMAQ_Dev/pull/1181)  |


### Updates to Windblown Dust Emissions 
[Jeff Willison](mailto:willison.jeffrey@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update and Bug Fix  
**Release Version/Date**: CMAQv5.4  
**Description**: 
Several updates have been made to the windblown dust routine for CMAQ 5.4. The module has been updated to include additional PX soil texture information from WRF when available. The fugitive dust subroutines tfabove.F and tfbelow.F have been removed, since these were not originally intended to be used with windblown dust sources. 

The option of using BELD3 as a windblown dust input has been removed. BELD3 is outdated and in CMAQ 5.3 the windblown dust module did not support BELD4 or BELD5. Beginning in CMAQ 5.4 the necessary land use information for windblown dust is taken from MCIP input files or WRF. For CMAQ 5.4 we strongly recommend the use of WRFv4.1+ and the PX LSM when enabling windblown dust emissions. 

Lastly, a bug was corrected that was causing low erodibility values and significantly lower dust emissions when using WRFv4 inputs. 

**Significance and Impact**:

A consequence of removing BELD as an option from windblown dust is that the DUST_LU* files are no longer needed. They have been removed from the CCTM code and the run scripts.

The following plot summarizes the impact of the remaining changes above: 

![image](https://user-images.githubusercontent.com/47453034/192348532-00cc147a-df4f-47dc-a5d5-b57ff315a9ec.png)

Again, note, in the image above, the impacts of changing windblown dust input data from BELD to MCIP/WRF is not shown, but was not found to be a large contributor to the changes seen. As can be seen the largest change on modeled windblown dust is a result of the bug fix that was causing low erodibility values. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#877](https://github.com/USEPA/CMAQ/commit/632673aa0abc81a4f88223e67744a3744174708d) | [PR#877](https://github.com/USEPA/CMAQ_Dev/pull/877)  |
