# Detailed Emissions Scaling Isolation and Diagnostics Module (DESID)

### Minor Bug Fix to DESID and ISAM
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix   
**Release Version/Date**:  CMAQv6.0 

**Description**:
Issue1) DESID is incapable of meeting at least one mapping feature. It cannot create a new rule for a species just over a specific region of the domain. For example, if the species ETOX is scaled to CO emissions for wildfires in CANADA:

"CANADA",  "WILDFIRE",  "CO",  "ETOX",  "GAS",  0.0002, "a"
will not produce emissions. However, if this rule is preceded by a rule that defines ETOX emissions (even if they're zero), then the correct emissions are produced.

"CANADA",  "WILDFIRE",  "CO",  "ETOX",  "GAS",  0.0, "a"
"CANADA",  "WILDFIRE",  "CO",  "ETOX",  "GAS",  0.0002, "a"
The first rule doesn't even have to be just over CANADA:

"EVERYWEHERE",  "WILDFIRE",  "CO",  "ETOX",  "GAS",  0.0, "a"
"CANADA",  "WILDFIRE",  "CO",  "ETOX",  "GAS",  0.0002, "a"
This PR revises the algorithm for defining unique regions. Instead of setting the region of the first scale factor manually to the full domain, the new algorithm initializes the unique region vector to 0 and adds every unique region, including the full domain explicitly.

Issue 2) Chloride was omitted from the implementation of PM_IONS in ISAM. This is now added.

**Significance and Impact**:
This PR will ensure that mapping with DESID works even in complex cases like scaling a new species just over a specific region of the full domain. It will also make sure chloride aerosol concentrations are reported for source apportionment simulations when the PM_IONS option is chosen for ISAM.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1360](https://github.com/USEPA/CMAQ_Dev/commit/0e2e923b2c07e6b76ea63dbf6d0f792253f4ed80) | [PR#1360](https://github.com/USEPA/CMAQ_Dev/pull/1360)  | 


### Improve DESID Error Checking for Negative Emissions  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix   
**Release Version/Date**:  CMAQv6.0  

**Description**:  
CMAQ users including Bonyoung Koo (Bay Area Air Quality Management District) and Calvin Howes (South Coast Air Quality Management District) have reported issues where an aerosol mass emission rate goes below zero but is within the tolerance set in DESID (-1.0e-7). When extrapolated to particle surface area or number emission though, this value can exceed the static threshold. Their suggestion to relax tolerances for particle number and surface area emissions is appropriate and useful.

See for example:   
https://forum.cmascenter.org/t/tolerance-for-negative-emissions-in-the-desid-module/5241  
https://forum.cmascenter.org/t/cmaqv5-5-emission-error-with-negative-emissions/5389/7  

DESID has been updated to allow number emissions down to -1 x 10<sup>-11</sup> s<sup>-1</sup> and surface area emissions down to -0.1 m<sup>2</sup> s<sup>-1</sup>.

This update also improves error reporting by giving the user the gridcell location of the negative value detected in reference to the global grid, not the local sub-domain. If only one processor is used, the local and global units are assumed to be the same.  

**Significance and Impact**:   
There should be no impact on results, but users are now given more information to diagnose the cause of negative emission rates calculated in DESID.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1222](https://github.com/USEPA/CMAQ/commit/d17cbcb6a715aa63a32ade77ebcbd2784b040c1a) | [PR#1222](https://github.com/USEPA/CMAQ_Dev/pull/1222)  | 

### DESID Area-Normalized Conversion Factor  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)    
**Type of update**: Bug Fix    
**Release Version/Date**: CMAQv6.0  

**Description**:    
The DESID conversion factor assigned to translate area-based emissions is erroneously inverted. This update takes the reciprocal of that value.

**Significance and Impact**:    
This will have almost no effect for most simulations and users since area-normalized emission rates are rarely used. However, for cases that utilize area-weight emissions, this fix will dramatically affect predictions for any species that includes emissions normalized to a specific area that is not in units of meters. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1160](https://github.com/USEPA/CMAQ/commit/8767b9671cef369aad28d9a484f4b86f961958fa) | [PR#1160](https://github.com/USEPA/CMAQ_Dev/pull/1160)  | 

### Streamline Emissions Unit Conversions in DESID
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)    
**Type of update**: Code Improvement   
**Release Version/Date**:  CMAQv6.0  

**Description**:   
This update streamlines and centralizes the unit conversions in DESID so that all aerosol and gas units are the same when passing from online emission modules to DESID and the unit conversion subroutine uses one approach for converting all scalars from total emission rate to volume-normalized emission rate. Erroneous comments in the dust module that inaccurately describe units for key variables are also resolved.

There has been a long-standing criticism of the overly complex unit conversions in the CMAQ emissions workflow going back at least to v4.7.1 where variables in different phases are converted at different points in the algorithm and with individually defined conversion formulae.

Now, units for aerosols from all sources, including dust and sea spray, are in g/s when entering DESID. The DESID workflow converts these units to ppmv/s for aerosol mass using the same conversion as it does for gases. Centralization of these steps is critical for maintenance of the offline, WRF-CMAQ, and MPAS-CMAQ systems, where different conversions are needed on different platforms.

**Significance and Impact**:    
No impact on results.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1177](https://github.com/USEPA/CMAQ/commit/44fa7764e3632063d2459e383d650ccaea491d95) | [PR#1177](https://github.com/USEPA/CMAQ_Dev/pull/1177)  | 

### Streamlining DESID code  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)     
**Type of update**: Code Maintenance   
**Release Version/Date**:  CMAQv6.0  

**Description**:    
DESID variables for online emission streams indices (e.g. IBIOSRM, IMGSRM, etc.) are largely unnecessary and can be removed in almost all cases. This simplification makes it easier to introduce new online emission modules to DESID. There are fewer places in the code to modify.

**Significance and Impact**:    
No impact on results.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1267](https://github.com/USEPA/CMAQ/commit/684b45ca253c04a854278ca929bb25968583fe3e) | [PR#1267](https://github.com/USEPA/CMAQ_Dev/pull/1267)  | 

### Updates needed for MPAS-CMAQ implementation  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)    
**Type of update**: Bug Fix  
**Release Version/Date**: v5.5  

**Description**:  Two issues were found related to emissions processing algorithms in DESID.  

(1) In desid_module.F, when the emission scale factor is applied, there are conditionals that govern whether the scale factor (FAC) should be adjusted by the map scale factor or grid cell area. These potential adjustments are applied within a loop over vertical levels and so their impact will accumulate as the algorithm treats higher model layers. This issue was not resolved earlier since area adjustments are seldom needed in the current CMAQ workflow.

(2) A variable in AERO_EMIS (EMISM3) was allocated every time the subroutine was called, and a better approach is to define EMISM3 as a saved variable and allocate it once. It is a large variable so avoiding repeated allocations may have an impact on memory management.

**Significance and Impact**: In addition to correcting potential vulnerabilities, these changes are needed to harmonize the code between offline CMAQ and MPAS-CMAQ.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1007](https://github.com/USEPA/CMAQ/commit/109ac7ef5b972dee37fd4e1f66ec865b277d77c2) | [PR#1007](https://github.com/USEPA/CMAQ_Dev/pull/1007)  |

### Reconcile Emission Molecular Weight Table with CRACMM Speciation  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Mechanism Support  
**Release Version/Date**: CMAQv5.5  

**Description**:  DESID uses molecular weight to convert between moles and mass for gas and aerosol emission variables. In general, these MW's are not needed because gases and aerosols are provided in terms of moles and mass, respectively. However, if emissions for a CMAQ species are provided in a unit that requires this conversion, the MW must be provided or a value of 1.0 will be assumed.   

**Significance and Impact**: To fully support CRACMM development and implementation.  In most cases, there is no impact on results. If MW is needed for a unit conversion, then the impact can be very significant (1-2 orders of magnitude for the affected species).   

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1006](https://github.com/USEPA/CMAQ/commit/bea8e25dccc0c416e42924cda3890ab098470b58) | [PR#1006](https://github.com/USEPA/CMAQ_Dev/pull/1006)  |

### Chemical Family Support
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5   

**Description**: Allows for aerosol bulk names to be used on emission input files (e.g. APOC or ASO4). These variable names do not include the mode suffixes. DESID should be able to map them to an internal CMAQ species using a rule like:
```
Region             Stream      Emission    CMAQ     Phase       Scale      Basis    Operator
                               Variable    Species            Factor
'EVERYWHERE', 'ALL'          ,'APOC',     'APOC'      ,'FINE',    1.0,        'UNIT',   'a',
```
but it currently cannot because it automatically stores all aerosol bulk names as families with members equivalent to the list of aerosol species matching that chemical (i.e. APOC contains APOCI and APOCJ). If an input file has APOC on it, and the user tries to map to it, DESID looks for APOCI and APOCJ, can't find them, and reports a problem (but moves on and runs). Previous CMAQ versions (5.3.3) had this capability and the introduction of aerosol bulk name families compromised it.

In the new approach, if the algorithm detects that a variable name is an aerosol bulk name, it looks for an emission variable matching that name, instead of breaking it apart into its members. On the other hand, if the name is used in the CMAQ species column (i.e. the second occurrence of APOC in the example above), then DESID will match the components and scale them each as desired.

Other improvements in this update include a check when families are defined to make sure that if a user defines a family that has the same name as an active aerosol bulk name, then the user definition is preferred and the number of chemical families is reduced.

**Significance and Impact**: No impact on model results. Restoration of features available in previous model versions and better error checking and handling.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#963](https://github.com/USEPA/CMAQ/commit/f7182ebf94524be13a6db804a26a148f863dc3f2) | [PR#963](https://github.com/USEPA/CMAQ_Dev/pull/963)  | 


### Restructuring and Miscellaneous updates to the DESID Interface and Processing Features
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Interface Update  
**Release Version/Date**: CMAQv5.4  

**Description**: 
The DESID Emission Control file has been restructured and several minor updates have been made to the functionality and features available in the DESID interface. Specific details as follows:
- The Emission control file has been streamlined and split to improve maintainability. There are now three control files including CMAQ_Control_Misc.nml which contains input parameters for non-DESID modules like ELMO and the Budget tool, CMAQ_Control_DESID.nml which contains DESID parameters that are independent of chemical mechanism (e.g. region and are size distribution parameters), and CMAQ_Control_DESID_${MECH}.nml which contains mechanism-dependent scaling rules.
- Area normalization - if offline streams are provided in units of area-normalized flux, DESID can now be told to multiply the flux inputs by the area of each grid cell projected to the real area on the Earth's surface, if appropriate.
- A computational inefficiency was discovered that has been corrected for cases when there are more than 200,000 point sources on a single sub-domain block.
- Diagnostic options have been expanded so that users can choose which variables and sources will populate individual output files.
- A bug in the processing of region-based scaling parameters was discovered and revised.
- Variables and comments have been revised for clarity.
- Chemical families have been revised to so they are accessed globally by other modules like ELMO.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#817](https://github.com/USEPA/CMAQ/commit/d3a8d13f63746a83854a4babde3a2a5d9747e15d) | [PR#817](https://github.com/USEPA/CMAQ_Dev/pull/817)  | 
|[Merge for PR#809](https://github.com/USEPA/CMAQ/commit/c6f906165301f28ad45a0118acec9d9a6666db98) | [PR#809](https://github.com/USEPA/CMAQ_Dev/pull/809)  | 
|[Merge for PR#766](https://github.com/USEPA/CMAQ/commit/9b90649c8fb9316c5abc163f79e3a1245644880f) | [PR#766](https://github.com/USEPA/CMAQ_Dev/pull/766)  | 
|[Merge for PR#714](https://github.com/USEPA/CMAQ/commit/804498d39c73e648b1aa72fcb807697fd1dc67b1) | [PR#714](https://github.com/USEPA/CMAQ_Dev/pull/714)  | 
|[Merge for PR#709](https://github.com/USEPA/CMAQ/commit/c510f9b3be031cd799d1f7dd5a106674c29e58b6) | [PR#709](https://github.com/USEPA/CMAQ_Dev/pull/709)  | 
|[Merge for PR#648](https://github.com/USEPA/CMAQ/commit/22d519fdac9c8fcfec9aeb2c186b2f0e3f77ad8b) | [PR#648](https://github.com/USEPA/CMAQ_Dev/pull/648)  | 
|[Merge for PR#894](https://github.com/USEPA/CMAQ/commit/baced51d9047a7814846cb54533e6a0dfe14832c) | [PR#894](https://github.com/USEPA/CMAQ_Dev/pull/894)  | 
|[Merge for PR#638](https://github.com/USEPA/CMAQ/commit/cac8f6cbd55558c330549278d396a1d3920d2f80) | [PR#638](https://github.com/USEPA/CMAQ_Dev/pull/638)  | 
|[Merge for PR#772](https://github.com/USEPA/CMAQ/commit/60af2e025519ccaed3a51feebda622d48e9f020a) | [PR#772](https://github.com/USEPA/CMAQ_Dev/pull/772)  | 
