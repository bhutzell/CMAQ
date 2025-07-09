### KMT2 rate updates and minor bugfix
[Kathleen Fahey](mailto:fahey.kathleen@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Rate coefficient update and bug fix  
**Release Version/Date**: CMAQv6.0  

**Description**: 
This PR updates rate coefficients for the glyoxylic acid and glyoxylate reactions with hydroxyl in cloud water following Tan et al., 2009. Additionally, there was a minor bug fix (removal of a parenthesis) following a recent update. 

**Significance and Impact**:  
Impacts are expected to be minor. Now the rate coefficients are more consistent with those used in Fahey et al. (2025).

**References**: 
Fahey, K.M., Sareen, N., Carlton, A.G., and Hutzell, W.T.: Updated In-Cloud Secondary Aerosol Production in the Northern Hemisphere Predicted by the Community Multiscale Air Quality Modeling System. ACS Earth and Space Chemistry, 9 (5), 1043-1059,
doi: 10.1021/acsearthspacechem.4c00370, 2025. 

Tan, Y., Perri, M.J., Seitzinger, S.P., Turpin, B.J.: Effects of precursor concentration and acidic sulfate in aqueous glyoxal-OH radical oxidation and implications for secondary organic aerosol. Env. Sci. Technol.,
43, 8105-8112, doi: 10.1021/es901742f, 2009. 


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1339](https://github.com/USEPA/CMAQ/commit/ccb92633ebe4cd73ef738e4f16899c4eb95e2f0d) | [PR#1339](https://github.com/USEPA/CMAQ_Dev/pull/1339)  |

### Streamline Mapping to Default Cloud Chemistry Solver
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Infrastructure Improvement  
**Release Version/Date**: CMAQv6.0    

**Description**:  
This current PR is aimed at some minor updates to simplify the mapping of cldproc vectors to the aqueous chemistry routine via **AQ_DATA**.  
- High-level variables have been renamed for less confusion. For example, ngas is replaced with n_aq_gas to indicate it is relevant to the aqueous chemistry solver. Likewise naddaer and naer are replaced with one variable, n_aq_aer.
- akn, acc, and cor are replaced with global variables defined in the AERO_DATA module: iait, iacc, and icor.
- The series of 'req_XXX' variables for gases and aerosols are eliminated. If these species are required, then they can be given explicit, local indices which is now done.
- nmodes is replaced by the global variable n_mode from AERO_DATA. 
- The aerosol surrogate table is revamped. Currently, some rows contain all modes of a species, while others, like PHG and TRACER contain only one mode and leave the rest blank. Many additional aerosols are added in code below. This PR updates the table to be explicit in defining the 31 aerosol species that will be treated by the aqueous chemistry solver. Each species is further assigned a local index that will not change.
- Map vectors like MAP_CGRIDtoAQGAS are defined to make mapping easier to follow from cloudproc to aqchem.
- Code that previously built the aerosol surrogate table is now deleted since the aerosol table is defined explicitly.

The following updates were made to **aq_map**:
- Code for passing concentrations and calculating contributions to surrogates is streamlined.
- The approach for accounting for the Aitken mode contribution to surrogates is streamlined and generalized so that it extends automatically to any surrogate defined in the future, not just the 4 that were available currently, PRI, POA, SOA, and TRACER.
- Mode indices are generalized

The following limited updates were made to **aqchem**:
- Variable names like nliqs were updated to N_AQ_LIQS
- AKN, ACC, and COR were updated to IAIT, IACC, and ICOR.
- Commented code was removed.

**Significance and Impact**:  These updates are needed to support transparency in model development and maintenance. Specifically, the streamlined mapping approach will support addition of aerosol modes and support of aerosol size section options in the future.

**Internal PRs**: [PR#1130](https://github.com/USEPA/CMAQ_Dev/pull/1130)  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1130](https://github.com/USEPA/CMAQ/commit/8d607848cdfe7b1f1b139dbb968145f092fb9714) | [PR#1130](https://github.com/USEPA/CMAQ_Dev/pull/1130)  |

### Minor bug fixes to cloud processing
**Primary Contact**: [Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Maintenance  
**Release Version/Date**:  CMAQv6.0   

**Description**:  
Two issues are addressed.   

(1) The model crashes when compiled in column mode with gcc in debug mode. The problem is that two arrays are added together that don't have the same dimension lengths. An explicit loop is added to address this.

(2) The model crashes with Sulfur-Tracking on. The aqueous chemistry surrogate for SULF_ICBC does not exist, and has never existed. Before the cloud chemistry mapping was updated, there was no check for this error. Now the model correctly crashes because this error exists. 


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1263](https://github.com/USEPA/CMAQ/commit/2f07b7813e6a632f256041c3d901f39e1fd96d90) | [PR#1263](https://github.com/USEPA/CMAQ_Dev/pull/1263)  |

### Removal of acm_ae6_mp Cloud Module
**Primary Contact**: [Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Maintenance  
**Release Version/Date**:  CMAQv6.0   

**Description**:  The acm_ae6_mp cloud mechanism was previously used with multipollutant configurations of the CMAQ model. However, it no longer works with any existing chemical mechanism and is scientifically obsolete. This unused option is removed in this PR.  

**Significance and Impact**: Since it has not been possible to use this option for several model releases, its removal is not expected to have any significant impacts on the user community.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1236](https://github.com/USEPA/CMAQ/commit/72f92b2a7e26b4d9ef16f2f7fbaa976bf77fbac3) | [PR#1236](https://github.com/USEPA/CMAQ_Dev/pull/1236)  |

###  Cleanup of unit conversions and wetdep output mapping  
[Ben Murphy](mailto:murphy.benjamin@epa.gov), U.S. Environmental Protection Agency      
**Type of update**: Science Update    
**Release Version/Date**: CMAQv5.5   

**Description**:  
In CMAQv5.4 and prior, there are at least 3 or 4 different mapped vectors through which data is passed when translating from CGRID to the local arrays within scavwdep and aqchem. Each of these steps constrains the flexibility to expand gas and aerosol components and necessitates the differentiation of codes to maintain separate modules. For example, 'mp', and 'kmt' flavors require special versions of AQ_DATA and/or aq_map to handle mapping. Unfortunately, when the various optional modules for cloud chemistry diverge, it becomes much less likely that instrumented codes like STM and ISAM will be applied across all options. It is also more difficult to keep cloud process codes like CONVCLD and RESCLD consistent across all options.

With refactored mapping procedures, these can all be merged and all the cloud chemical schemes can take advantage of STM, ISAM, and other improvements made to the base code. These improvements will also reduce maintenance needed.

This code update simplifies the unit conversions in cldproc and the mapping of wet deposition rates. Vectors supporting these maps are now available in CGRID_SPCS and don't need to be recalculated in cldproc. Scavwdep was updated to take advantage of aerosol masks. Further updating of scavwdep will occur in subsequent PR's. DDM and ISAM codes were modified for consistency with the new changes.

**Significance and Impact**: This code update is designed to improve the transparency and flexibility of mapping, speciation, and instrumentation within the cloud chemistry/removal processor.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#651](https://github.com/USEPA/CMAQ/commit/cd7b0e8939552ca8056e348fc3758513280cc095) | [PR#651](https://github.com/USEPA/CMAQ_Dev/pull/651)  |
  

### Bug fix for AE2AQ surrogates and redistribution of aerosol species after cloud processing 
[Kathleen Fahey](mailto:fahey.kathleen@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**: 
This is a bug fix updating the treatment of aerosol-to-aqueous surrogates (AE2AQ) for those species in the aerosol namelists that had a mismatch in number of modes of a chosen surrogate and the CMAQ aerosol species, leading to incomplete or incorrect treatment for the affected aerosol species during cloud processing (e.g., if an aerosol species with an I and J mode was assigned to an AE2AQ surrogate with only a J-mode, Aitken scavenging was not calculated for that aerosol species). This update also corrects the redistribution of an aqueous surrogate to "CEND" aerosol species after cloud processing if (1) the surrogate has an I and J mode and (2) the I and J mode fractional compositions of the surrogate are different.

**Significance and Impact**:  
These updates correct the in-cloud treatment and post-cloud redistribution of inert aerosol species. PM2.5 impacts are minor. 

![Fahey_AE2AQ_BugFix](https://github.com/user-attachments/assets/4a16562f-faa3-4c0c-a59b-817d30cda615)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#976](https://github.com/USEPA/CMAQ/commit/53d0884fc138ab2cb48cf733de961be448b4395d) | [PR#976](https://github.com/USEPA/CMAQ_Dev/pull/976)  | 
