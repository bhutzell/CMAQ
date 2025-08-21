# Aerosol Dynamics

### Generalized Dynamic and Equilibrium Partitioning of Inorganic Aerosols
**Ben Murphy**, U.S. Environmental Protection Agency  
**[Havala Pye](mailto:pye.havala@epa.gov)**, U.S. Environmental Protection Agency   
**Type of update**: Science Update  
**Release Version/Date**:  CMAQv6.0 

**Description**:  
Currently, CMAQ solves mass transfer of inorganic species (sulfate, ammonium, nitrate, chloride) to and from particles using an equilibrium approach for the Aitken and Accumulation modes, and a dynamic approach for the Coarse mode.

As horizontal resolution increases and time steps get very small, it becomes advantageous to use the dynamic approach for all modes. The computation time savings for the equilibrium approach is negligible, and the assumption of equilibrium across sub-5 min time steps becomes uncertain.

**Significance and Impact**:   
The base code is unstable at very short time steps because the equilibrium assumption for fine aerosols leads to too much mass transferring to/from particles in one time step. There is no significant and persistent impact on model results when the hybrid configuration is selected. The quantitative impact of forcing all modes to be dynamic or equilibrium is still to be determined.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1340](https://github.com/USEPA/CMAQ/commit/447d8f3a585b6e7f1839fbd26aaecc2c2e9ac524) | [PR#1340](https://github.com/USEPA/CMAQ_Dev/pull/1340)  | 

### SOA Mapping: Revise Logfile Output
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Logfile Messaging  
**Release Version/Date**:  CMAQv6.0 

**Description**:  
In CMAQv5.5 and previous, warnings are written to the logfile about aerosol and gas species not being able to be found when they are actually not needed by some mechanisms at all. This PR revises the logfile messaging approach to only alert when a species is required and not found (i.e. the model crashes). 

Instead, logfile messages are now added to state when aerosol and gas species are successfully mapped.

**Significance and Impact**:   
No impact on results.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1186](https://github.com/USEPA/CMAQ/commit/99d4354f5f4f16956f45c9a276f357e942b555cc) | [PR#1186](https://github.com/USEPA/CMAQ_Dev/pull/1186)  | 
  

### Remove option for Aerosol Initial Condition surface area and update settings for Boundary Condition Surface Area
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Run script updates  
**Release Version/Date**:  CMAQv6.0 

**Description**:   
The run script options for whether or not to use aerosol surface area from initial and boundary conditions have been updated. 

*IC_AERO_M2USE:* In CMAQv5.5, this option instructs CMAQ to use surface area (i.e. the second moment, M2) of each aerosol mode from the initial condition file. If the run is a new start (NEW_START = TRUE) and this flag is set to False, then CMAQ applies a default modal standard deviation prescribed in AERO_DATA. This flag is ignored for runs that are restarts. Therefore, the impact of this option should be negligible after model spin-up, so it has been decided that this option should be removed.

*IC_AERO_M2WET:* this option prescribes whether to treat the initial condition surface area as applicable for the wet or dry aerosol size distribution. Because IC_AERO_M2USE is removed, this option is removed as well.

*BC_AERO_M2USE:* this option instructs CMAQ to use aerosol surface area from the boundary condition files. Because the definition of the second moment changed with PR #890, when SOA is now considered part of the dry aerosol components, all boundary conditions created before May 3, 2022 have incompatible aerosol surface area with current CMAQ. Therefore, run scripts that point to boundary conditions created before May 3, 2022 should have BC_AERO_M2USE set to False, and run scripts with boundary conditions created after this date should be set to True.

*BC_AERO_M2WET:* generally, aerosol surface area is assumed to be dry and this option should be set to False by default. If a user knows their aerosol surface area applies to the wet size distribution, this option may be used to enforce compatibility.

These variables are now explained in the User Guide Appendix A.

**Significance and Impact**:  
These changes improve transparency and compatibility across CMAQ scenarios.

Simulations were performed on 2018 CONUS domain for a summertime period. The map below shows the difference between concentrations for total aerosol with the BC_AERO_M2USE option set to F vs. T. 
![image](https://github.com/user-attachments/assets/9f048187-ab68-4f64-9ba9-925cb6c6d218)
Deviation in total fine particle mass concentration in $\mu g \ m^{-3}$.

![image](https://github.com/user-attachments/assets/80068a34-2643-49ac-bb0a-cce3e5859d41)
Deviation in total coarse particle mass concentration in $\mu g \ m^{-3}$.
The majority of the difference between these simulations comes from coarse mode particles. The impact on the fine mode particle mass is negligible.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1174](https://github.com/USEPA/CMAQ/commit/e6c581cd140c4248d7220ed7f2a1ec77a391da7e) | [PR#1174](https://github.com/USEPA/CMAQ_Dev/pull/1174)  | 



### Improve Aerosol Boundary Condition Processing
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5   

**Description**:   
When aerosol boundary conditions are provided from a source that omits particle number concentration, CMAQv5.4 and prior will detect the default concentrations (1.0E-30) as acceptable. This creates physically unrealistic size distributions at the boundaries, which lead to negative aerosol concentrations when operated on by processes like aerosol sedimentation. Collaborators and users have reported FLOOR files indicating these problems when using boundary conditions from sources like GEOSChem. 

With this update, the lower bound is raised to 1.1E-30 which should be able to detect missing boundary concentration data in most cases. The aerosol mode variables (Number and Second Moment) are reset to values that are consistent with realistic size distributions, negative concentrations are resolved, and FLOOR files are no longer generated.  

**Significance and Impact**:   
The update is needed for any runs performed with GEOSChem boundary conditions, especially if particle predictions are an output of interest. The impact on PM2.5 is mostly seen at the boundaries although some differences in the internal domain can also be observed. There should be minimal impact on ozone concentrations. 

There is no impact on simulations run with Hemispheric CMAQ boundary conditions, which include particle number and surface are variables on the boundary condition input files.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1080](https://github.com/USEPA/CMAQ/commit/5638ccd750eb63e48664a53d7fe8a14c2a410e58) | [PR#1080](https://github.com/USEPA/CMAQ_Dev/pull/1080)  | 
  

### Two-Moment Option for Particle Coagulation
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix/Science Update  
**Release Version/Date**: CMAQv5.4  

**Description**:   
The current three-moment approach for modeling coagulation for the Aitken and Accumulation modes (intermodal and intramodal coagulation) was found to be inconsistent with sectional and particle-resolved approaches via box modeling. A two-moment approach was documented by Whitby et al. (1991) and is implemented here as a default option.

**Significance and Impact**:   
Minimal change to PM concentration predictions or code structure. Non-negligible impact on fine-mode particle sizes and number concentrations.

**References**:   
Whitby, McMurry, Shankar, and Binkowski. Modal Aerosol Dynamics Modeling, Report to the Atmospheric Research and Exposure Assessment Laboratory, US EPA, 1991.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#879](https://github.com/USEPA/CMAQ/commit/ae76f45de41c78bdd76afd65187b3f900f606c8a) | [PR#879](https://github.com/USEPA/CMAQ_Dev/pull/879)  |  

### Sulfuric Acid Conservation in VOLINORG
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.4  

**Description**:    
The current implementation of sulfuric acid mass transfer from the gas to particle phase in the VOLINORG subroutine assumes that the sulfuric acid concentration leaving the subroutine equals the concentration calculated using a pseudo-steady-state assumption with the gas production rate divided by the condensation sink. This concentration could be higher or lower than the abundance of sulfuric acid predicted after execution of the chemistry model, so sulfur is not conserved. Since H2SO4 at the beginning of VOLINORG already has the gas-phase production in it from the chemistry module, a more conservative approach is now used, equivalent to:
H2SO4_FINAL = H2SO4_INITIAL - [Change in ASO4]* MWSO4/MWH2SO4.

**Significance and Impact**:   
Differences in sulfuric acid concentrations at the surface up to a factor of 100, but these are always during cases where the absolute concentration is below 0.1 ug m-3. Generally dry and wet deposition decrease, but not by large amounts when run on the Southeast benchmark case. 

**References**:   
**Internal PRs** [PR#636](https://github.com/USEPA/CMAQ_Dev/pull/636) 

### Update Aerosol Size Distribution Check for ICs and BCs
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Bug Fix/Science Update   
**Release Version/Date**: CMAQv5.4  

**Description**:  
The aerosol size distribution parameters (number, surface area, and speciated mass) from the initial and boundary conditions is read in and 'conditioned' by the aero_check_icbc routine in AERO_DATA.F. This routine checks to make sure that the total aerosol mass, number and surface area are above zero and that they combine to give appropriate modal diameter and standard deviation (i.e. within the bounds asserted in AERO_DATA.F). If the parameters do not meet the constraints imposed, then the number and/or surface area may be adjusted in order to fit reference values for diameter and standard deviation.

There are complexities to this issue that were not adequately accounted for in the current implementation. First, the current implementation only supports inputs where the surface area specifies the dry distribution, but inputs could be configured for the wet deposition and thus contradict the expectations of the conditioning module. An option is now provided in the CCTM runscript to specify the nature of the surface area input for initial and boundary conditions independently.

Secondly, it is possible that no surface area parameter is provided on the inputs. This use case is now supported with options on the CCTM runscript that control the initial and boundary conditions independently.

Third, the consideration of grid cells has been limited to just those actually on the boundaries of the grid domain. All boundary cells for interior subdomains and interior boundary cells for subdomains on the domain edge are ignored. This saves model computation time.

Finally, the logical flow has been made more thorough and robust to catch extreme errors in diameter and standard deviation, but pass minor errors (i.e. sigma = 1.04999 instead of 1.05) and correct them without warning.

Deprecated variables including AVISDIAG, ZERO_PCSOA and AOD are also removed since their functionality is no longer supported.

**Significance and Impact**:  
Impact on surface concentrations and domain-wide budget were assessed for a July 2016 case. Average PM2.5 surface concentrations changed by less than 0.025 ug m-3. Average Accumulation and Aitken mode diameters changed by less than 2.5 nm.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#874](https://github.com/USEPA/CMAQ/commit/c6bb6fe9d84fc4d6cd277c761715a83031ac5611) | [PR#874](https://github.com/USEPA/CMAQ_Dev/pull/874)  |  

### Reclassify all SOA species as 'dry' aerosol
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)  
**Type of update**: Science Update  
**Release Version/Date**: CMAQv5.4  

**Description**:   
SOA is currently categorized as 'wet' aerosol for the purposes of calculating aerosol surface area for transport. CMAQ recalculates the surface area ignoring wet aerosol components in order to prevent unstable calculations during vertical updrafts. The approach of considering SOA as one of these wet components was made more than a decade ago and doesn't appear necessary for current CMAQ numerical stability.

This PR builds on top of PR #874 by using the improved IC/BC size distribution conditioning. It's important to note that if all organics are treated as dry aerosol, then there is an inconsistency with existing boundary conditions, which have aerosol surface area consistent with assuming the SOA is wet. Until new boundary conditions are generated with SOA as dry, it is strongly recommended to ignore aerosol surface area inputs from ICs and BCs. This is achieved with a runscript option introduced in PR #874 (IC_AERO_M2USE and BC_AERO_M2USE).

**Significance and Impact**:   
Modest decreases to organic aerosol concentrations throughout the US domain for a July 1-31, 2016 case. Some reductions in bias, but changes are not very large.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#890](https://github.com/USEPA/CMAQ/commit/d5bdbad4e83e945533b851ab74dfef6b59b366a7) | [PR#890](https://github.com/USEPA/CMAQ_Dev/pull/890)  |

### Simplify Aerosol Chemical Namelist
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)   
**Type of update**: Interface Update  
**Release Version/Date**: CMAQv5.4  

**Description**:   
Aerosol species are currently provided to the AE namelist with mode suffixes indicating the size of the particles the species is applicable for (e.g. aitken, accumulaiton, or coarse). This PR removes the suffix from all chemical species names on the AE namelist, removes duplicative species names so that only one entry for each chemical species persists, and then adds 3 columns with T/F values indicating for which modes this species should be considered. The CGIRD_SPC routine reads in the aerosol chemical species and distributes the species in modal space using a new routine added to the AERO_DATA file outside of the AERO_DATA module. This routine must be outside of AERO_DATA because AERO_DATA uses CGRID_SPCS for information as well.

Code has also been added to chemmech to process the new namelist format correctly. The dependence on the AERO_DATA table has been removed here so there are in effect fewer checks for chemmech than there are for the CCTM, but users will still encounter the checks when they eventually run the CCTM. The alternative is to have AERO_DATA tables in two places and this is not desired at this time.

**Significance and Impact**:   
No impact on predictions should be observed.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#885](https://github.com/USEPA/CMAQ/commit/0012539ab2c4927ed6fb28107d71c505bb1a9fe1) | [PR#885](https://github.com/USEPA/CMAQ_Dev/pull/885)  |
