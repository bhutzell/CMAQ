# Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM)

### Updated mechanism CRACMM3
[Havala Pye](mailto:pye.havala@epa.gov),  U.S. Environmental Protection Agency    
**Type of update**: Science Update  
**Release Version/Date**: CMAQv6.0 beta1 and beta 2     

**Description**: 
 

**Significance and Impact**:  
 

**References**:  


### Halogen chemistry in CRACCM3M
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv6.0 beta 1 and beta 2    

**Description**:  
This update contains four different items: (1) NOY definition in the current SpecDef files for CRACMM2 and CRACMM3 contain an error which is now fixed (2) It adds halogen (Cl, Br, I) chemistry to CRACMM3 and creates a new marine mechanism (CRACMM3M). Current model (MGEMIS.F) contains an error for grid-cell area calculation for halogen emissions which is fixed in the pull request. A new Euler Backward Iterative (EBI) solver is developed. (3) CMAQ with cb6r5m_ae7_aq did not compile due to changes made in CRACMM3M. Several heterogeneous reactions in cb6r5m_ae7_aq are relabeled (without making any chemistry changes). Update made in MGEMIS.F for grid-cell area calculation also affects halogen emissions in cb6r5m_ae7_aq. (4) CMAQ with cb6r5_ae7_aq was also tested due to the update in MGEMIS.F. 

**Significance and Impact**:  
Item #1: Correcting NOY definition:  
It does not directly affect CMAQ results and no test involving CMAQ was performed.

Item #2: Halogen chemistry with CRACMM3 (CRACMM3M):  
Halogen chemistry reduces O3 over seawater and land by up to 8.0 ppb. Larger reductions occur over the seawater than over the land. Halogen chemistry reduces surface O3 by 13% over the seawater (annually). However, halogen chemistry has marginal effects on model PM2.5 concentrations. 

Item #3: Updates in cb6r5m_ae7_aq:  
Update in MGEMIS.F increases ozone and reduces sulfate over low latitude areas due to the changes in gid-cell area estimates. Incorporation of the map-scale factor (msfx2) into the calculation lowers the grid-cell area estimates near the equator and subsequently reduces halogen and DMS emissions. 

Item #4: Updates in cb6r5_ae7_aq:  
Update in MGEMIS.F has minimum impacts on ozone and sulfate over the contiguous US.

**References**:   None

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1212](https://github.com/USEPA/CMAQ_Dev/commit/8d512cc361675212430b579adc766c010309bdd1) | [PR#1212](https://github.com/USEPA/CMAQ_Dev/pull/1212)  |

### Photolysis of aerosol nitrate in CRACCM3M
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2   

**Description**:  
This updates adds photolysis of aerosol nitrate (ANO3) to the CRACMM3 marine mechanism (CRACMM3M) following the procedure described in Sarwar et al., 2024. A new Euler Backward Iterative (EBI) solver is developed.

**Significance and Impact**:   
Model ozone (O3) concentrations without the photolysis of aerosol nitrate are shown in Figure 1a. Higher values are predicted over the land than over seawater. Model O3 enhancements with the photolysis of aerosol nitrate are shown Figure 1b. Consistent with the results shown in Sarwar et al. (2024) for CB6, aerosol nitrate photolysis enhances O3 over seawater and land by large margins. Larger enhancement occur over the western U.S. than over the eastern U.S.

![image](https://github.com/user-attachments/assets/1b276ad3-11eb-4e5b-a9f1-d8d9a2b5a2a3)
Figure 1: (a) CMAQ predicted O3 with CRACMM2M (without the aerosol nitrate photolysis) in May (b) Impact of the aerosol nitrate photolysis on O3 compared to those without the aerosol nitrate photolysis in May

Monthly Mean Bias was calculated by using model predicted daily maximum 8 hour average (MDA8) O3 and observed data from the AQS monitoring network over the western and eastern U.S (Figure 2(a-b)). Over the western U.S., model without the aerosol nitrate photolysis underpredicts observed data in most months while model with the aerosol nitrate photolysis eliminates the negative bias. Over the eastern U.S., model without the aerosol nitrate photolysis has mixed impacts on model performance producing negative bias in January-May and positive bias in June-December. Model with the aerosol nitrate photolysis eliminates the negative bias in January-May, but slightly deteriorates bias in June-December.

![image](https://github.com/user-attachments/assets/efeef818-6491-45d1-9983-c717e841d97b)
Figure 2: (a) Monthly Mean Bias of DMA8 O3 without and with aerosol nitrate photolysis at AQS sites over the western U.S. (b) Monthly Mean Bias of DMA8 O3 without and with aerosol nitrate photolysis at AQS sites over the eastern U.S. 

Model PM2.5 concentrations without the photolysis of aerosol nitrate are shown in Figure 3a. Higher values are predicted over land than over seawater. Changes in model PM2.5 concentrations with the photolysis of aerosol nitrate are shown Figure 3b. It only affects model PM2.5 concentrations by small margins. Reductions occur due to the loss aerosol nitrate by photolysis while the enhancements occur from the changes in secondary aerosols due to the changes in oxidant levels. 

![image](https://github.com/user-attachments/assets/a229bc46-7396-482d-808c-e16e14a614cf)
Figure 3: (a) CMAQ predicted mean PM2.5 wth CRACMM2M (without the aerosol nitrate photolysis) in May (b) Impact of the aerosol nitrate photolysis on PM2.5 compared to those without the aerosol nitrate photolysis in May

Monthly Mean Bias was calculated by using predicted daily mean PM2.5 and observed data from the AQS monitoring network over the western and eastern U.S (Figure 4(a-b)). Bias without and with the aerosol nitrate photolysis in each month is similar over western and eastern U.S. Thus, the aerosol nitrate photolysis has low impacts on model performance for PM2.5.

![image](https://github.com/user-attachments/assets/3ad7743e-e8c3-40b4-963f-eb45d0b84435)
Figure 4: (a) Monthly Mean Bias of daily mean PM2.5  without and with aerosol nitrate photolysis at AQS sites over the western U.S. (b) Monthly Mean Bias of daily mean PM2.5  without and with aerosol nitrate photolysis at AQS sites over the eastern U.S. 

**References**:   
Sarwar, G., Henderson, B.H., Hogrefe, C., Mathur, R., Gilliam, R., Callaghan, A., B., Lee, J., Carpenter, L. J.: Examining the Impact of the photolysis of aerosol nitrate over Northern Hemisphere, Science of the Total Environment, 917, 170406, 2024. 

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1214](https://github.com/USEPA/CMAQ_Dev/commit/f807233e2354b0d270aba2b2207393ddacb4a1af) | [PR#1214](https://github.com/USEPA/CMAQ_Dev/pull/1214)  |

### Photolysis of aerosol nitrate in CRACMM3  
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update  
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2    

**Description**:   
This pull request adds photolysis of aerosol nitrate (ANO3) to CRACMM3 following the procedure described in Sarwar et al., 2024. It adds a new aerosol species, ASEAST, which represents entire fine-mode sea-salt with a molecular weight of 31.3 grams per mole. Molecular weight of ASEAST is calculated using sea-salt composition data and molecular weight of individual chemical species represented in AERO_DATA.F. ASEAT and ANO3, and their molecular weights are used to calculate an enhancement factor which is then multiplied by the photolysis frequency of nitric acid to calculate the photolysis frequency of ANO3. A new Euler Backward Iterative (EBI) solver is developed.

**Significance and Impact**:   
Model ozone (O3) concentrations without the photolysis of aerosol nitrate are shown in Figure 1a. Higher values are predicted over the southern portion than the northern portion. Model O3 enhancements with the photolysis of aerosol nitrate are shown Figure 1b. Enhancements occur only over small areas and are much smaller than those obtained with the hemispheric CMAQ model (Sarwar et al., 2024). The majority of the enhancements in the hemispheric model occurs over the ocean and are transported over land. The impacts on O3 are small since the continental US domain contains only a small oceanic area. 

![image](https://github.com/user-attachments/assets/ff8c770c-9450-4147-b4bc-134f8411f123)
Figure 1: (a) Model O3 without the aerosol nitrate photolysis (b) Impact of the aerosol nitrate photolysis on O3 compared to those without the aerosol nitrate photolysis 

Model aerosol nitrate concentrations without the photolysis of aerosol nitrate are shown in Figure 2a. Higher values are predicted only over the Mid-west. Changes in model aerosol nitrate concentrations with the photolysis of aerosol nitrate are shown Figure 2b. Aerosol nitrate concentrations decrease over some oceanic areas. However, the impacts over land areas are small.

![image](https://github.com/user-attachments/assets/d15437df-954d-4508-858a-312a3c196058)

Figure 2: (a) Model aerosol nitrate concentrations without the aerosol nitrate photolysis (b) Impact of the aerosol nitrate photolysis on aerosol nitrate concentrations compared to those without the aerosol nitrate photolysis 

**References**:   
Sarwar, G., Henderson, B.H., Hogrefe, C., Mathur, R., Gilliam, R., Callaghan, A., B., Lee, J., Carpenter, L. J.: Examining the Impact of the photolysis of aerosol nitrate over Northern Hemisphere, Science of the Total Environment, 917, 170406, 2024. https://doi.org/10.1016/j.scitotenv.2024.170406

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1185](https://github.com/USEPA/CMAQ_Dev/commit/189dc7f9b7e60b87efe76f5ff9af53088c2b469a) | [PR#1185](https://github.com/USEPA/CMAQ_Dev/pull/1185)  |

### Updating the condensed halogen chemistry and renaming of "INO2" to "ISONP"  
[Golam Sarwar](mailto:sarwar.golam@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update      
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2  

**Description**:    
This pull request contains two updates: (1) CRACMM3 contains a chemical species “INO2” (isoprene nitrate peroxy radical). Detailed halogen chemistry for CRACMM3 also contains INO2 (iodine nitrite). To avoid conflict, INO2 in CRACMM3 is changed to ISONP. (2) Condensed halogen chemistry used in CRACMM3 was previously developed using hemispheric results of detailed halogen chemistry in the Carbon Bond chemical mechanism. The condensed halogen chemistry is re-derived using hemispheric results of detailed halogen chemistry in CRACMM3 following the procedure described in Sarwar et al., 2015 and is included in this pull request. A new Euler Backward Iterative (EBI) solver is developed since the name of a chemical species is changed.

**Significance and Impact**:   
Predicted ozone (O3) concentrations with the updated condensed halogen chemistry increase over ocean and coastal areas compared to those obtained with the existing halogen chemistry. Ozone losses with the updated halogen chemistry are lower than those obtained with the existing halogen chemistry. First order O3 loss rate coefficient in the updated condensed halogen chemistry is lower than that in the existing condensed halogen chemistry primarily due to changes in grid-cell area calculation. The existing model did not include the map scale factor for calculating grid-cell area which is revised in the updated model. Values of msfx2 (map scale factor squared) are higher than 1.0 over low latitude areas which decreases grid-cell area and subsequently halogen emissions. Lower halogen emissions reduce halogen initiated ozone loss in the hemispheric model leading to the lower the first order O3 loss rate coefficient.

**References**:     
Sarwar, G., Gantt, B., Schwede, D., Foley, K., Mathur, R., Saiz-Lopez, A: Impact of enhanced ozone deposition and halogen chemistry on tropospheric ozone over the Northern Hemisphere, Environmental Science & Technology, 49(15):9203-9211, 2015.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1183](https://github.com/USEPA/CMAQ_Dev/commit/0353213dc5b72c961735cba164aca67e859d8231) | [PR#1183](https://github.com/USEPA/CMAQ_Dev/pull/1183)  |

### Consistent treatment of styrene and ethylbenzene across CMAQ   
[Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update   
**Release Version/Date**:  6.0 beta 1 and beta 2   

**Description**:   
The representation of the chemistry of styrene and ethylbenzene in CRACMM3 has been updated to be consistent with the treatment of styrene and ethylbenzene in the CMAQ reactive tracer module. This allows for CRACMM styrene and ethylbenzene species to be used directly in modeling of air toxics without the need for styrene and ethylbenzene reactive tracers. Reactions of styrene with ozone and the nitrate radical have been added in CRACMM3 with chemistry based on the Master Chemical Mechanism. These are minor channels compared to reaction with OH (which was previously added in CRACMM2), but they are being added to ensure consistent treatment of styrene in CRACMM and the CMAQ reactive tracer module. The reaction rate constant of styrene+OH has also been updated to use a value from an experimental study. For ethylbenzene, a reaction with NO3 has been added, and there has been a small change in the rate constant for reaction with OH.

**Significance and Impact**:   
Styrene concentrations are reduced, primarily due to additional losses through reaction with ozone. Decreases are on the order of 10% in areas with the highest styrene concentrations. There are also some resulting small increases in benzaldehyde and formaldehyde which are major products of styrene oxidation. Impacts on ozone are negligible (<0.01 ppb).  Impacts on ethylbenzene are negligible (on the order of 0.1 ppt or less). 

**References**:   
* MCM chemistry: [Jenkin et al., 2003](https://doi.org/10.5194/acp-3-181-2003) and [Bloss et al., 2005](https://doi.org/10.5194/acp-5-641-2005)
* OH+styrene rate constant: [Cho et al., 2014](https://doi.org/10.1021/jp501380j)
* O3+styrene rate constant: [Le Person et al., 2008](https://doi.org/10.1016/j.jphotochem.2007.09.006)
* NO3 rate constants (styrene and ethylbenzene): [Atkinson, 1991](https://doi.org/10.1063/1.555887)
* OH+ethylbenzene rate constant: [Shaw et al., 2018](https://doi.org/10.5194/acp-18-4039-2018)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1153](https://github.com/USEPA/CMAQ_Dev/commit/c89b9afe89e86875b8cfcdc4af3f0a83b8db0762) | [PR#1153](https://github.com/USEPA/CMAQ_Dev/pull/1153)  |

### Correct conservation of nitrogen for 4 reactions in CRACMM3  
**Primary Contact**: [Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Secondary Contact**:  [Havala Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency (could make Havala primary instead)    
**Type of update**: Maintenance   
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2   

**Description**:   
Greg Yarwood noted 4 RACM2 reactions that do not properly conserve nitrogen. The reactions are also in CRACMM. He proposed an update and Bill Stockwell agreed. The following proposed fixes from Greg Yarwood have been implemented in CRACMM3:  
R159 MPAN + NO3→MACP + NO2 + HNO3  
R246 ADCN + HO2→ ONIT  
R273 MCP + MO2→ HO2 + 1.500 HCHO + 0.500 HKET + 0.250 MOH + 0.250 ROH  
R309 MCP + ACO3→ 0.500 HO2 + HCHO + 0.500 HKET + 0.500 MO2 + 0.500 ORA2  

**Significance and Impact**:  
Errors in conservation of nitrogen for select reactions ported from RACM2 into CRACMM should be corrected to be more accurate. One of the long-term goals of CRACMM is to balance nitrogen within the mechanism, and this is a step towards that goal. Impacts on simulated concentrations are minimal.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1205](https://github.com/USEPA/CMAQ_Dev/commit/a0f806bd2666d217ff25c4a2b3b04d2347c1d607) | [PR#1205](https://github.com/USEPA/CMAQ_Dev/pull/1205)  |



### CRACMM Reaction Metadata File  
[[Havala Pye](mailto:pye.havala@epa.gov)], U.S. Environmental Protection Agency    
**Type of update**: Documentation   
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2   

**Description**:   
Metadata file to document updates to CRACMM chemistry at the reaction level.  

**Significance and Impact**:   
This file provides information on CRACMM updates at the reaction level. This file will be posted on github.com/USEPA/CRACMM upon public release of CRACMM in CMAQ. This file will feed efforts to link chemical reactions across EPA and specifically the Chemical Transformations Database (CheT, https://ccte-cced-chet.epa.gov/).

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1258](https://github.com/USEPA/CMAQ_Dev/commit/e9aeb46561f7c4e7f497ed65617e4ea53ce2f25b) | [PR#1258](https://github.com/USEPA/CMAQ_Dev/pull/1258)  |

### Updated mechanism CRACMM2  
[Nash Skipper](mailto:skipper.nash@epa.gov) and [Havala Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency      
**Type of update**: Science Update   
**Release Version/Date**: CMAQv5.5   

<img src="https://github.com/user-attachments/assets/e0eeceb1-da06-4e94-a456-e83e50455b15" width="400">

**Description**:  
CRACMM2 includes several updates to CRACMM1. Many updates are intended to improve the representation of secondary formaldehyde (HCHO) in CRACMM. These include the incorporation of the AMORE v1.2 isoprene condensation into the primary CRACMM mechanism, updates to HCHO yields from monoterpenes, and the addition of styrene as a new explicit species. Some other opportunistic updates (mostly unrelated to formaldehyde) are changes to monoterpene nitrates that affect SOA formation and NOx recycling, the inclusion of emitted methane (ECH4), heterogeneous uptake of HO2 and NO3 radicals, and changes in how emissions of certain aromatic species are mapped to CRACMM species. CRACMM1 bug fixes noted below have also been incorporated into CRACMM2.  

**Significance and Impact**:   
Formaldehyde is a hazardous air pollutant (HAP) and is a major contributor of health risks from air toxics; however, it is biased low in CRACMM1 by about a factor of two. Formaldehyde performance should be improved to provide a more accurate estimate of risk from ambient exposure. Formaldehyde can also be sensed remotely from satellites and is often used (as a proxy for VOC abundance) along with satellite-based estimates of NO2 to diagnose ozone production regimes. Improvements to HCHO in CRACMM may allow for more meaningful comparisons between observed and modeled chemical regime. The updates in CRACMM2 also tend to increase ozone and decrease organic aerosol, particularly in the summer and particularly in the southeastern US.  

**References**:  
Skipper, T. N., D'Ambro, E. L., Wiser, F. C., McNeill, V. F., Schwantes, R. H., Henderson, B. H., Piletic, I. R., Baublitz, C. B., Bash, J. O., Whitehill, A. R., Valin, L. C., Mouat, A. P., Kaiser, J., Wolfe, G. M., St. Clair, J. M., Hanisco, T. F., Fried, A., Place, B. K., and Pye, H. O. T.: Role of chemical production and depositional losses on formaldehyde in the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM), EGUsphere, 2024, 1-34, https://doi.org/10.5194/egusphere-2024-1680, 2024.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1095](https://github.com/USEPA/CMAQ/commit/5c9f5441b24efd94ad6be5ad939c5f6fc8af2980) | [PR#1095](https://github.com/USEPA/CMAQ_Dev/pull/1095)  |

### workaround for gcc incompatibility with CRACMM namelists 
[Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Script update    
**Release Version/Date**:  CMAQv5.5   

**Description**:   
Remove trailing comments in CRACMM species namelists when building CMAQ with gcc  

**Significance and Impact**:   
CRACMM uses trailing comments in the species namelists (e.g., `GC_cracmm2.nml`, `AE_cracmm2_nml`, and `NR_cracmm2.nml` files) for species metadata. If CMAQ is compiled with gcc, the model will crash at runtime because gcc does not allow trailing comments in namelist files. Previously if a user wanted to use CRACMM with gcc, they would have to either obtain versions of the namelist files without trailing comments from the CMAS Center or remove the trailing comments themselves. The removal of trailing comments in the namelist files has now been automated at build time if the `bldit_cctm.csh` script detects that a gcc compiler is used with a CRACMM mechansim.  

|Merge Commit | Internal record|
|:------:|:-------:|
| [Merge for PR#1154](https://github.com/USEPA/CMAQ/commit/c31983b72a3049d708138da3f57227875333eb39) | [PR#1154](https://github.com/USEPA/CMAQ_Dev/pull/1154) |  


### Bug fixes for CRACMM1 biogenic emissions using MEGAN
[Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5   

**Description**:  
The following fixes have also been implemented for CRACMM1 biogenic emissions using MEGAN:
* Fix emission mapping of CO when using MEGAN biogenic emissions. In CMAQv5.4 CO emissions were mapped to species SLOWROC for CRACMM1. Note: other CMAQ mechanisms were not affected by this CO mapping issue.
* Add mapping of semivolatile ROC species included in MEGAN biogenic emissions to CRACMM1 DESID file. In CMAQv5.4 emissions of these species would not be added because there was not an existing rule in the DESID file to map them to a model species.

**Significance and Impact**:   
These updates only impact applications that use the CRACMM1 or CRACMM1AMORE chemical mechanisms and the MEGAN biogenic emissions model. Applications using BEIS or using any other chemical mechanism are not impacted.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1095](https://github.com/USEPA/CMAQ/commit/5c9f5441b24efd94ad6be5ad939c5f6fc8af2980) | [PR#1095](https://github.com/USEPA/CMAQ_Dev/pull/1095)  | 

### Bug fixes for CRACMM1 IEPOX uptake rate
[Kathleen Fahey](mailto:fahey.kathleen@epa.gov) and [Nash Skipper](mailto:skipper.nash@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  

**Description**:   
A typo in the rate of sulfate catalyzed IEPOX where the nucleophile was HSO4 has been corrected. The incorrect rate resulted in excessive uptake of IEPOX through this pathway.

**Significance and Impact**:   
These updates only impact applications that use the CRACMM1 or CRACMM1AMORE chemical mechanisms. The impact of this fix is to increase sulfate (as less sulfate is taken up as organosulfate) and to decrease organic aerosol.

**References**:   
Vannucci, P., K. Foley, B. Murphy, C. Hogrefe, R. Cohen and H. Pye: Temperature-dependent composition of summertime PM2.5 in observations and model predictions across the Eastern U.S., ACS Earth Space Chem. 2024, 8, 2, 381–392. [https://doi.org/10.1021/acsearthspacechem.3c00333](https://doi.org/10.1021/acsearthspacechem.3c00333)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#976](https://github.com/USEPA/CMAQ/commit/53d0884fc138ab2cb48cf733de961be448b4395d) | [PR#976](https://github.com/USEPA/CMAQ_Dev/pull/976)  |


### CRACMM DESID Input File Updates  
[Karl Seltzer](mailto:seltzer.karl@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Input File Update   
**Release Version/Date**: CMAQv5.5  

**Description**:   
Added ROC-ALK series of "Emission Surrogates" to ensure emission input files with and without the "ALK" identifier are processed/emitted.  

**Significance and Impact**:   
Impacts results (SOA and PM2.5 predictions) if emissions were prepared using ROC-ALK species names. This fix is needed to propagate ROC-ALK emissions to the proper model species. Users should check at least one processor log to ensure emissions were properly mapped. Depending on the emission preparation method, ROC-ALK may or may not be populated. Newer versions of emissions prepared with gspro files created by [S2S-Tool](https://github.com/USEPA/S2S-Tool) are more likely to use consistent names.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#970](https://github.com/USEPA/CMAQ/commit/17ce8e23b04ddec4fcf32dd20b55d5c8902d6c29) | [PR#970](https://github.com/USEPA/CMAQ_Dev/pull/970)  |

### The Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) Version 1.0
[Havala O. T. Pye](mailto:pye.havala@epa.gov), U.S. Environmental Protection Agency   
**Type of update**: Science Update   
**Release Version/Date**: CMAQv5.4   

**Description**:   
The Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) builds on the history of the Regional Atmospheric Chemistry Mechanism, Version 2 (RACM2) and aims to couple gas- and particle-phase chemistry by treating the entire pool of atmospheric reactive organic carbon (ROC) relevant to present-day emissions. CRACMM species were developed to represent the total emissions of ROC, considering the OH reactivity, ability to form ozone and secondary organic aerosol (SOA), and other properties of individual emitted compounds. The chemistry of CRACMM, which includes autoxidation, multigenerational oxidation, and the treatment of semivolatile and intermediate volatility compounds, was built using a variety of sources including literature and other mechanisms (MCM, GECKO, and SAPRC18/mechgen). Compared to RACM2, the number of traditional volatile organic carbon species is reduced and the number of oxygenated and semivolatile to intermediate volatility precursors are increased in the mechanism. In addition, explicit hazardous air pollutants (toluene; 1,3-butadiene; and acrolein) are added to better characterize exposures relevant for human health. 

CRACMMv1 is available in two versions: base CRACMMv1 and CRACMMv1AMORE. The development of base CRACMMv1 is described by Pye et al. (2022) and the application of CRACMMv1 within CMAQ to the northeast U.S. in summer 2018 as well as comparison with other mechanisms is presented by Place et al. (in prep.). CRACMMv1AMORE replaces the base isoprene chemistry of CRACMMv1 (which was largely ported from RACM2) with a graph theory-based condensation of a detailed isoprene mechanism developed by Prof. Faye McNeill's team at Columbia University. The AMORE version is documented in work by Wiser et al. (2022).

One feature of CRACMM is the specification of representative structures for all species in the mechanism. Metadata, including a representative compound name, description of explicit vs lumped nature, a [SMILES string](https://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system), and DTXSID identifier in the [EPA Chemicals Dashboard](https://comptox.epa.gov/dashboard/) (if available) are appended to the species namelists (GC, NR, and AE). This information is leveraged to determine conservation of mass across chemical reactions (see the CHEMMECH README in the UTIL directory), determination of species properties such as solubility, and to communicate how species are conceptualized. Representative compound information from the namelists are matched with species descriptions (a verbose string description in cracmm1_speciesdescription.csv) using python to provide markdown file descriptions of the mechanism species. See the CMAQ Users' Guide Chapter 6 for more information on CRACMM.

Supporting data for CRACMM, including information on how to map emissions to the mechanism, will be available in a [CRACMM github repository](https://github.com/USEPA/CRACMM). Information on getting started with CRACMM is available in [a tutorial](../Users_Guide/Tutorials/CMAQ_UG_tutorial_CRACMM.md).

**Significance and Impact**:   
CRACMM couples SOA formation with radical chemistry and updates the representation of a number of chemical systems. CRACMM is being released as a research mechanism so that it may undergo testing in various applications with the aim of making it the default chemistry option in the future. A fact sheet describing the CRACMM effort is available on [EPA's CMAQ website](https://www.epa.gov/cmaq/cmaq-fact-sheets).

**References**:   
1. Pye, H. O. T., Place, B. K., Murphy, B. N., Seltzer, K. M., D'Ambro, E. L., Allen, C., Piletic, I. R., Farrell, S., Schwantes, R. H., Coggon, M. M., Saunders, E., Xu, L., Sarwar, G., Hutzell, W. T., Foley, K. M., Pouliot, G., Bash, J., and Stockwell, W. R.: Linking gas, particulate, and toxic endpoints to air emissions in the Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 1.0, Atmos. Chem. Phys. Discuss. [preprint], https://doi.org/10.5194/acp-2022-695, in review, 2022.
2. Place, B. K., Hutzell, W. T., Appel, K. W., Farrell, S., Valin, L., Murphy, B. N., Seltzer, K. M., Sarwar, G., Allen, C., Piletic, I., D'Ambro, E., Saunders, E., Simon, H., Torres-Vasquez, A., Pleim, J., Schwantes, R., Coggon, M., Xu, L., Stockwell, W. R., and Pye, H. O. T.: Initial evaluation of the CRACMMv1.0 chemical mechanism: Surface ozone predictions across the Northeast US summer 2018 in CMAQ, in preparation for Atmospheric Chemistry and Physics.
3. Wiser, F., Place, B., Sen, S., Pye, H. O. T., Yang, B., Westervelt, D. M., Henze, D. K., Fiore, A. M., and McNeill, V. F.: AMORE-Isoprene v1.0: A new reduced mechanism for gas-phase isoprene oxidation, Geosci. Model Dev. Discuss. [preprint], https://doi.org/10.5194/gmd-2022-240, in review, 2022.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#908](https://github.com/USEPA/CMAQ/commit/7ea4c901f754376ccdb1ad8b0b82c4a5efd3a6ba) | [PR#908](https://github.com/USEPA/CMAQ_Dev/pull/908)  |

