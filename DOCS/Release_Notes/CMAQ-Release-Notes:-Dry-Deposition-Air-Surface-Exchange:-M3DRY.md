# M3DRY
### Runtime Deposition Options
[Jon Pleim](mailto:pleim.jon@pa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update  
**Release Version/Date**:  V6.0   

**Description**:   
Allows the user to specify M3Dry (default) or STAGE dry deposition options from the run script. 

**Significance and Impact**:   

Updated relative reactivity for carbon species following Raoult’s law changing the minimum value from 1 to 0.01 as well as recalculating this value for CO and HCHO for M3Dry. This update aligns the relative reactivity with the methodology used for new CRACMM and PFAS species. Revised relative reactivities in M3Dry generally result in a higher VOC concentrations and improves the evaluation against AQS observations. In all cases, the change in VOC concentrations are relatively small and generally less than 5% of the existing biases.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1321](https://github.com/USEPA/CMAQ_Dev/pull/1321/commits/d387fd332650055d71fb4de373dc25d92ab86730)| [PR#1321](https://github.com/USEPA/CMAQ_Dev/pull/1321)  |

### Updates of minimum Kz for M3Dry
[Jon Pleim](mailto:pleim.jon@pa.gov), U.S. Environmental Protection Agency    
**Type of update**: Science Update  
**Release Version/Date**:  V6.0   

**Description**:   
Changes the minimum Kz from 1.0 m2/s to 0.01 m2/s when the Kzmin flag is set to False.  This minimum Kz is the same as used in WRF.  Also, when Kzmin flag is set to True, changes the height above ground where Kzmin is set according to fraction of urban LU from 500 m to the PBLHT.

**Significance and Impact**:   
Change #1 results in CMAQ using the same minimum Kz as WRF when Kzmin = False.  This makes the PBL processes in CMAQ for chemical species identical to PBL processes in WRF.
Change #2 modifies the parameterization in CMAQ when Kzmin = True that is meant to compensate for the incorrect PBL treatment in urban areas when Urban schemes are not used in WRF.
When using the new UACM option in WRF-CMAQ Kzmin should be False.   


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1283](https://github.com/USEPA/CMAQ/commit/f92834f5643f39312fb13cd19c0041c61c0791e0)| [PR#1283](https://github.com/USEPA/CMAQ_Dev/pull/1283)  |


### Revised dry dep flux for NH3
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.5    

**Description**:   
Changed the calculation of NH3 dry dep flux and NH3 emission flux.
Now the NH3 dry dep flux is calculated in ABFLUX by setting the ground and stomatal NH3 concentrations to zero. Emission flux is then the Net Flux - Dry Dep flux.  

Also, fixed error in cuticle resistance to ammonia when not using bi-directional flux.
 
**Significance and Impact**:  
First part only changes NH3 and NH3_Emis output in CCTM_DRYDEP files.
Second part changes NH3 deposition when running without ammonia bi-directional exchange (ABFLUX). This change causes a minor increase in PM and decreases O3 (see figure below), providing a more accurate NH3 dry dep flux estimate.


<table>
<thead>
<tr>
<th><img width="100%" src="./images/dry-deposition/cmaqv6.0_seasonal_mean_difference_ozone_mixing_ratio.png"></th>
<th><img width="100%" src="./images/dry-deposition/cmaqv6.0_seasonal_mean_difference_PM2.5_concentration.png"></th>
</tr>
</table>

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1073](https://github.com/USEPA/CMAQ/commit/c58dbf7b0f60d4bd04188205236e664fab7902cd) | [PR#1073](https://github.com/USEPA/CMAQ_Dev/pull/1073)  |



### New Aerosol Deposition Model (aero_depv)  
[Jon Pleim](mailto:pleim.jon@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  

**Description**:  
A new aerosol deposition model has been developed and added to CMAQv5.4.  The model development, description, and testing are described in a new Journal article to be published in JAMES (Pleim et al, 2022).
The inspiration for this new model comes from comparisons of currently used aerosol dry deposition models to a compendia of published field measurement studies in various landscapes that show very poor agreement over a wide range of particle sizes.  The new aerosol dry deposition model that is a modification of the current model in CMAQv5.3 agrees much better with measured dry deposition velocities across particle sizes.  The key innovation is the addition of a second inertial impaction term for microscale obstacles such as leaf hairs, microscale ridges, and needleleaf edge effects.  

**Significance and Impact**:  
The most significant effect of the new model is to increase the mass dry deposition of the accumulation mode aerosols in CMAQ.  Accumulation mode mass dry deposition velocities increase by almost an order of magnitude in forested areas with lesser increases for shorter vegetation.  Peak PM2.5 concentrations are reduced in some forested areas by up to 40% in CMAQ simulations.   

**References**:   
Pleim, J. E., Ran, L., Saylor, R. D., Willison, J., & Binkowski, F. S. (2022). A new aerosol dry deposition model for air quality and climate modeling. Journal of Advances in Modeling Earth Systems, 14, e2022MS003050. [https://doi.org/10.1029/2022MS003050](https://doi.org/10.1029/2022MS003050)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#842](https://github.com/USEPA/CMAQ/commit/289701974ba9610cf92043e9f223fbbf0f888bbd) | [PR#842](https://github.com/USEPA/CMAQ_Dev/pull/842)  |
