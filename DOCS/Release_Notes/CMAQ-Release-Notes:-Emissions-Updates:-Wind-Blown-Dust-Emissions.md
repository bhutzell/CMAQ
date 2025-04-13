# Wind Blown Dust Emissions

### Correction for NLCD40 Land Use Mapping in Windblown Dust Module
[Christian Hogrefe](mailto:hogrefe.christian@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Bug Fix    
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 1 (first released on [5.5+ branch](../CMAQ-Bugfix-Branch.md#record-of-changes-to-cmaqv55) on 2024-12-06)  

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
