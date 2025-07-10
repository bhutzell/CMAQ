# Frequently Asked Questions for Using the CMAQv6.0 ALPHA Version

## Table of Contents:
* [What is a alpha version?](#what_alpha)
* [Do I need to update from v5.5 to v6.0alpha?](#update_v55_v60b)
* [What do I need to do to update from v5.5 to v6.0alpha?](#update_v55_v60b)
  * [What differences should I expect in the required model input files?](#diff_v55_v60b_input_files)
  * [What differences should I expect in my model output files?](#diff_v55_v60b_output_files)
* [Are there new benchmark data and documentation updates?](#data_and_docs)
* [How to cite CMAQ](#how_to_cite)
* [Additional FAQ](#additional_faq)
* [Technical support for CMAQ](#tech_support)

<a id=what_alpha></a>
## What is an alpha version?
We are making this early version of the code available for testing, evaluation, and demonstration purposes before the official, general release. We also intend this version to facilitate community contributions. While this alpha version has undergone preliminary testing, it may contain bugs and may not function in the same way that it will in the final release. 

<a id=update_v55_v60b></a>
## Do I need to update from v5.5 to v6.0alpha?
CMAQv6.0alpha is not intended as an immediate replacement for CMAQv5.5. However, CMAQv6.0alpha does contain advances in both scientific and user-support algorithms.

### Instrumented Models (CMAQ-ISAM, CMAQ-DDM3D)
- **Updates to the Integrated Source Apportionment Method (CMAQ-ISAM)**  
CMAQv6.0 introduces CMAQ-ISAM compatibility with CRACMM2, CRACMM3, & CRACMM3M as well as several improvements that target CMAQ-ISAM robustness for all chemical mechanisms.
  - **Add ISAM support for CRACMM3M**  
    Code changes allow CCTM-ISAM simulations to use the cracmm3m chemical mechanism. [Release Note](../Release_Notes/CMAQ-Release-Notes%3A-Instrumented-Models%3A-CMAQ-ISAM.md#add-cracmm3m-to-mechanisms-supported-by-isam)

  - **Improve stability in ISAM apportionment output**    
  Addresses errors in aerosol/gas partitioning in ISAM, preventing mass from being incorrectly allocated to distant tags. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#improve-stability-in-isam-apportionment-output)

  - **Fix ISAM erroneous mass attribution**    
  Fixes a bug that caused tagged mass to appear far from emission sources due to improper handling of aerosol mass concentrations.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#erroneous-mass-attribution)

  - **Add pcVOC and NOy species to ISAM species lists**   
   The addition of pcVOC, CRON, and OPAN to the ISAM VOC and NOy species lists corrects discrepancies in source apportionment, ensuring comprehensive accounting of SOA precursors and NOy species in PM and ozone analyses. This update improves the accuracy of ISAM results, particularly for fine PM and SOA predictions, with only minor effects on ozone. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#adds-pcvoc-and-noy-species-to-isam-species-lists)
  
   - **ISAM control file**    
  Increases control file character limits and improves logging for ISAM tag summaries, aiding troubleshooting. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#isam-control-file)
  
- **Updates to the Decoupled Direct Method in Three Dimensions (CMAQ-DDM3D)**   
  - **Address DDM3D instability in sensitivity fields**   
   Fixes DDM3D instablity in after hetereogenous chemistry due to inconsistent unit conversions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-DDM3D.md)

  - **DDM3D control file character limit increase**   
  Increases the character limit for specification strings in the DDM-3D control file to support more complex applications.   [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-DDM3D.md#ddm-3d-control-file-character-limit-increase)
  
### Chemistry
- **Updates to Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM)**   
CMAQv6.0 introduces CRACMM version 3. CRACMM3 includes several updates to CRACMM2. These updates improve the representation of gas-phase and aerosol chemistry in multiple environments. In addition to the base CRACMM3 mechanism, CMAQv6.0alpha includes CRACMM3M, with extended marine chemistry, and CRACMM3HAPS which includes additional Hazardous Air Pollutants.  If you are interested in learning more, please see the CRACMM3 release notes:

  - **Heterogeneous chemistry of sulfur species**   
  Adds heterogeneous sulfur chemistry to CRACMM3, improving model performance in high PM pollution areas during winter.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#heterogeneous-chemistry-of-sulfur-species)

  - **Adding chlorine chemistry in CRACMM3**   
  Incorporates chlorine chemistry from CRACMM3M into CRACMM3, using a reduced set of organic reactions to limit computational demand. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#adding-chlorine-chemistry-in-cracmm3)

  - **Photolysis of aerosol nitrate in CRACMM3**   
  Adds photolysis of aerosol nitrate and a new aerosol species to CRACMM3, with updated molecular weights and solver enhancements. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#photolysis-of-aerosol-nitrate-in-cracmm3)

  - **Updates to aromatic system chemical compound identity**    
   The identities and representative structures of aromatic oxidation products in CMAQv6.0 were updated to better reflect their sources and yields, with phenol and catechol now explicitly represented and emission mappings revised for improved chemical accuracy.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updates-to-aromatic-system-chemical-compound-identity)

  - **Peroxy radical products from monoterpene ozonolysis and monoterpene aldehyde photolysis**    
  The formation of highly oxygenated organic molecules (HOM) from monoterpene ozonolysis and monoterpene aldehyde photolysis was enhanced by updating peroxy radical product yields and reaction pathways. These changes increase organic aerosol concentrations, particularly in regions with high biogenic emissions, with the largest impact from monoterpene ozonolysis. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#peroxy-radical-products-from-monoterpene-ozonolysis-and-monoterpene-aldehyde-photolysis)

  - **Peroxy radical reaction rate updates for temperature**   
    Temperature dependence was added to several peroxy radical reaction rates in CRACMM3, resulting in increased ozone formation, especially during summer in Southern California and the Midwest US. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#peroxy-radical-reaction-rate-updates-for-temperature)

  - **Photolysis of monoterpene derived SOA**    
    Photolysis of monoterpene-derived secondary organic aerosol (SOA) was implemented in CRACMM3, incorporating laboratory-based yields and a photo-recalcitrant fraction to better represent SOA losses. This update decreases organic aerosol concentrations and improves model agreement with observations. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md##photolysis-of-monoterpene-derived-soa)
 
  - **CRACMM3 Benzaldehyde chemistry**     
 The benzaldehyde chemistry in CRACMM3 was updated to align with the Master Chemical Mechanism (MCM), including the addition of missing reactions and improved structural assignments. These changes result in minor ozone increases (<0.1 ppb) and ensure proper carbon mass conservation in oxidation product reactions.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#cracmm3-benzaldehyde-chemistry)

  - **Updates to CRACMM based on carbon balance**    
  Several CRACMM reactions were updated to improve carbon balance by adding CO₂ as a product, adjusting CO yields, and correcting aldehyde product assignments. These changes enhance carbon tracking with minimal impact on ozone, particulate matter, or other key species.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updates-to-cracmm-based-on-carbon-balance)

  - **Updates to Henry's Law constants for CRACMM3**    
    Henry's Law constants for CRACMM3 species were updated to use measured or OPERA model-calculated values instead of surrogate species values. This change better reflects the properties of representative compounds and has minimal impact on ozone and PM2.5 concentrations. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updates-to-henrys-law-constants-for-cracmm3)
    
  - **Add CRACMM3HAPS Chemical mechanism**  
     The CRACMM3HAPS chemical mechanism was added to extend CRACMM3 with the ability to simulate hazardous air pollutants (HAPs), including hydrogen cyanide and mercury species, for improved risk assessment applications. This mechanism aligns with the latest atmospheric chemistry science and supports studies like EPA's AirToxScreen, while maintaining consistency with criteria pollutant predictions from CRACMM3.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).mdd#add-cracmm3haps-chemical-mechanism)
   
  - **Representative structures for CRACMM3HAPs tracers**  
     Representative chemical structures were assigned to all CRACMM3HAPs species, including explicit HAPs and nine lumped PAH groups, to better communicate species information and support property assignments like solubility. For each lumped PAH, a representative structure was selected based on EPA documentation and chemical similarity searches.  [Release Note](../Release_Notes/)
   
  - **Photolysis update in CRACMM3 and CRACMM3M**    
  Updates photolysis data and reactions for several species, adds new reactions, and incorporates recent scientific findings for improved accuracy. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#photolysis-update-in-cracmm3-and-cracmm3m)

  - **Halogen chemistry in CRACMM3M**   
  Fixes errors in NOY definition, adds halogen chemistry and a new marine mechanism, and introduces a new EBI solver. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#halogen-chemistry-in-cracmm3m)

  - **Photolysis of aerosol nitrate in CRACMM3M**   
  Adds photolysis of aerosol nitrate to the marine mechanism, following recent scientific procedures and solver updates. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#photolysis-of-aerosol-nitrate-in-cracmm3m)

  - **Updating the condensed halogen chemistry and renaming of "INO2" to "ISONP"**   
  Renames a chemical species to avoid conflicts and re-derives condensed halogen chemistry using updated hemispheric results. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updating-the-condensed-halogen-chemistry-and-renaming-of-ino2-to-isonp)

  - **Consistent treatment of styrene and ethylbenzene across CMAQ**   
  Aligns the treatment of styrene and ethylbenzene chemistry in CRACMM3 with the reactive tracer module, ensuring consistency for air toxics modeling. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#consistent-treatment-of-styrene-and-ethylbenzene-across-cmaq)

  - **Correct conservation of nitrogen for 4 reactions in CRACMM3**   
Implements fixes to ensure nitrogen conservation in four specific reactions, based on expert recommendations. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#correct-conservation-of-nitrogen-for-4-reactions-in-cracmm3)

  - **CRACMM Reaction Metadata File**   
Provides a metadata file documenting updates to CRACMM chemistry at the reaction level.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#cracmm-reaction-metadata-file)

  - **Updated visibility index information to follow IMPROVE**     
   The visibility index values for aerosol species in CMAQ were updated to align with the [Second IMPROVE equation](https://vista.cira.colostate.edu/Improve/the-improve-algorithm/), specifically adjusting "small organic mass" values from 4.0 to 2.8. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updated-visibility-index-information-to-follow-improve)

  - **Updated CRACMM species names**     
 CRACMM species names were updated to consistently use V/A prefixes for gas and aerosol phases, and AGLY was renamed to AGLYOLIG to clarify its structure. These changes are cosmetic and do not affect model concentrations, but users should update species mappings when using initial or boundary conditions from previous versions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updated-cracmm-species-names)

  - **Remove duplicate OP3 reaction with OH**    
A duplicate reaction of OP3 with OH that was present in CRACMM1 and CRACMM2 was removed from CRACMM2 to correct an error carried over from earlier versions. This bug fix has a negligible impact on model results, with changes in PM2.5 averaging less than 0.01 µg/m³.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#remove-duplicate-op3-reaction-with-oh)

  - **Streamline chemical mechanisms**   
CMAQv6.0 no longer supports the following mechanisms: CB6R3_AE7_AQ, CRACMM1_AQ, CRACMM1AMORE_AQ, RACM2_AE6_AQ, and SAPRC07TIC_AE7i_AQKMT2. The update ensures consistency across remaining mechanisms and utilities. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#delete-obsolete-chemical-mechanisms)

- **Bug fix to CarbonBond 6 Mechanism with Hazardous Air Pollutants (CB6r5HAP)**  
    - **Correct loss of reactive tracer styrene from ozone reaction**    
Bug fix to cb6r5hap_ae7_aq mechanism corrects a severe underestimation of the model species styrene, a hazardous air pollutant. No other model species are impacted. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Carbon-Bond-6-Mechanism-(CB6)-with-Hazardous-Air-Pollutants.md#correct-loss-of-reactive-tracer-styrene-from-ozone-reaction)

- **Bug fix to State Air Pollution Research Center (SAPRC) mechanisms**  
  - **Fix bug preventing CMAQ from running using SAPRC mechanisms**    
  Resolves an inconsistency so that CLNO2 now undergoes dry deposition as specified in the SAPRC mechanisms. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-State-Air-Pollution-Research-Center-(SAPRC).md#fix-bug-preventing-cmaq-from-running-using-saprc-mechanisms)

- **Updates to Aqueous Chemistry Scavenging and Wet Deposition**  
  - **KMT2 rate updates and minor bugfix**   
 Updates rate coefficients for key reactions in cloud water and fixes a minor bug in the code. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Aqueous-Chemistry-Scavenging-and-Wet-Deposition.md#kmt2-rate-updates-and-minor-bugfix)

  - **Streamline Mapping to Default Cloud Chemistry Solver**   
 Simplifies and clarifies variable naming and mapping for cloud chemistry routines, and updates the aerosol surrogate table for explicit species definition.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Aqueous-Chemistry-Scavenging-and-Wet-Deposition.md#streamline-mapping-to-default-cloud-chemistry-solver)

- **Updates to Aerosol Dynamics**   
  - **Generalized Dynamic and Equilibrium Partitioning of Inorganic Aerosols**   
  Extends the dynamic approach for mass transfer of inorganic aerosols to all modes, improving accuracy for high-resolution, short time step simulations. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Aerosol-Dynamics.md#generalized-dynamic-and-equilibrium-partitioning-of-inorganic-aerosols)

  - **SOA Mapping: Revise Logfile Output**   
  Refines logfile messaging to only warn when required species are missing, reducing unnecessary warnings and improving clarity. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Aerosol-Dynamics.md#soa-mapping-revise-logfile-output)

  - **Remove option for Aerosol Initial Condition surface area and update settings for Boundary Condition Surface Area**   
  Updates and simplifies run script options for aerosol surface area in initial and boundary conditions, clarifying usage and compatibility. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Aerosol-Dynamics.md#remove-option-for-aerosol-initial-condition-surface-area-and-update-settings-for-boundary-condition-surface-area)
  

- **Updates to Photolysis**
  - **Fix photolysis loss process in reactive tracer module**   
  Corrects unit conversion and initialization errors in the reactive tracer module’s photolysis loss process, and updates a conversion factor for consistency with gas phase chemistry.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#fix-photolysis-loss-process-in-reactive-tracer-module)

  - **Updated OMI.dat file that contains data from 2005 through 2024**   
  Provides a new, higher-resolution O3 column input file for the photolysis routine, extending data coverage through 2024.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#updated-omidat-file-that-contains-data-from-2005-through-2024)

  - **Remove uninitialized variable and correct a diagnostic in CCTM's inline module for photolysis frequencies**   
  Fixes a double conversion error for total extinction and resolves a model crash caused by uninitialized variables in certain cloud conditions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#remove-uninitialized-variable-and-correct-a-diagnostic-in-cctms-inline-module-for-photolysis-frequencies)

  - **Updates to diagnostics for Inline Photolysis**   
  This update improves the Inline Photolysis diagnostics by replacing total extinction coefficients with cloud extinction coefficients and adding calculations for aerosol properties when the sun is below the horizon. These changes correct a unit conversion error and provide more informative and comprehensive diagnostic data for evaluating light attenuation and aerosol optical properties. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#updates-to-diagnostics-for-inline-photolysis)


### Dry Deposition Air Surface Exchange  

- **Improve behavior of the runtime minimum eddy diffusivity option called KZMIN in STAGE and M3DRY**   
The KZMIN option, first introduced in CMAQv4.5, is a parameterization to allow the mixing in the planetary boundary layer (PBL) to respond to the land-use characteristics. If the runtime environmental variable KZMIN is set to 'True/Yes', the land-use based parameterized minimum eddy diffusivity will now be applied through the PBL, whereas previously it was limited to 500 meters above ground. If KZMIN is set to 'False/No', a constant minimum value of 0.01 m<sup>2</sup>/s is applied everywhere at all times. This change primarily impacts nighttime concentrations, specifically in grid cells where the PBL is lower than 500 meters. In those grid cells, primary emitted species concentrations will increase, whereas ozone mixing ratios will decrease due to increased NOx titration. [STAGE Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#update-to-minimum-kz-and-kz0ut-in-the-stage-deposition-option) | [M3DRY Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-M3DRY.md#updates-of-minimum-kz-for-m3dry)

- **Additional STAGE Updates**  
  - **Enhancements to deposition and resistance parameterizations**  
    The STAGE module was updated to improve the parameterization of aerodynamic and stomatal resistances, correct tiled LAI representation in coastal areas, and model deposition to partially snow-covered land as parallel rather than series resistances, resulting in higher deposition rates. Additional updates include revised leaf micro wetness and NH₃ cuticular resistance parameterizations, as well as improved scaling of stomatal and aerodynamic resistances by land use and meteorological conditions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#stage-updates-and-bug-fix)
  
  - **Diagnostic NH3 Emissions from Agriculture and Biogenic Sources**   
  Adds diagnostics for NH3 emissions from agricultural and biogenic sources in the STAGE module (no further description provided). [Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#diagnostic-nh3-emissions-from-agriculture-and-biogenic-sources)

  - **Add Support for DDM-3D**   
  Adds DDM-3D support to the STAGE module.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#runtime-deposition-options-and-add-support-for-ddm-3d)

  - **Restored the impact of dry deposition factor on diagnostic deposition velocity outputs**   
  Ensures diagnostic outputs reflect user-selected dry deposition factors, improving sensitivity analysis. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#restored-the-impact-of-dry-deposition-factor-on-diagnostic-deposition-velocity-outputs)

  - **STAGE gcc debug flag bug fix**   
  Fixes a crash caused by uninitialized arrays when running STAGE with gcc debug flags and bidirectional NH3 exchange.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#stage-gcc-debug-flag-bug-fix)

### Emissions
- **Introducing new soil emissions module**   
Introduces the Soil – Atmosphere Gaseous Emissions (SAGE) module for estimating soil NO and HONO emissions generally following the BDSNP parameterization (Hudman et al. 2012). SAGE  provides a simple, meteorological dependent soil NO and HONO emissions for regional to global applications. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Soil-Atmospheric-Gaseous-Emissions-(SAGE)-Emissions.md#introducing-new-soil-emissions-module)

- **New global vegetation dataset improves windblown dust emissions**    
Introduces a new satellite-based global vegetation dataset to modulate windblown dust emissions. The vegetation dataset accounts for the effect of non-photosynthetic vegetation (or brown vegetation) by updating vegetation fraction, the vegetation height, and the vegetation roughness following the method outlined in [Huang and Foroutan (2022, HF22)](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021JD035243). The overall impact is to reduce windblown dust emissions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Wind-Blown-Dust-Emissions.md)

- **Correction for NLCD40 Land Use Mapping in Windblown Dust Module**     
Fixes a bug related to the estimation of windblown dust emissions within CMAQ when using the NLCD40 land-use classification scheme. To estimate the emissions of windblown dust, grid cell land-use information along with meteorological conditions are needed. In this case, when using WRF with NLCD40 land-use, two categories of NLCD40 (“shrub/scrub” and “dwarf scrub”) were being mapped to the wrong internal categories (“barren or sparsely vegetated” instead of “shrubland”). These internal categories are used in the windblown dust module to assign parameter values controlling erodibility, which in this case was overestimated. Depending on the domain, year and approach to specify vegetation fraction in WRF, this bug fix corrects excessive "soil" PM2.5 and total PM2.5 mass concentrations when using NLCD40 land-use. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Wind-Blown-Dust-Emissions.md#correction-for-nlcd40-land-use-mapping-in-windblown-dust-module)

- **Bug fix for the estimation of marine-gas halogen emissions**   
To estimate the emissions of gaseous halogens in marine environments, the grid cell area covering the spatial extent within the domain is needed. Because CMAQ horizontal domains are defined by projecting a map onto a 2-D plane a map-scale factor must be applied when converting physical areas to projected space, which was not taken into account when estimating halogen emissions within this module. This bug fix impacts halogen emission estimates in grid cells in which map scale factors are not unity. For example, if using a northern polar stereographic map projection, this will lead to increased ozone (less ozone is destroyed by halogens) and decreases sulfate (less is produced via dimethyl sulfide) mostly over lower latitude areas. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#halogen-chemistry-in-cracmm3m)

- **Lightning Emissions Science Update**   
 Improves code clarity, removes an obsolete emissions file, and adds new options for using satellite and synergized lightning data for NOx emissions, supporting multiple methodologies and platforms.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Lightning-Emissions.md#lightning-emissions-science-update)

 - **Improve DESID Error Checking for Negative Emissions**   
 Relaxes tolerances for negative emissions, improves error reporting, and provides more precise gridcell location information for detected issues. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Detailed-Emissions-Scaling-Isolation-and-Diagnostics-Module-(DESID).md#improve-desid-error-checking-for-negative-emissions)

- **DESID Area-Normalized Conversion Factor**   
  Corrects an inversion error in the area-normalized conversion factor for emissions.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Detailed-Emissions-Scaling-Isolation-and-Diagnostics-Module-(DESID).md#desid-area-normalized-conversion-factor)

- **Streamline Emissions Unit Conversions in DESID**   
  Centralizes and standardizes unit conversions for emissions, simplifying maintenance and improving consistency across platforms. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Detailed-Emissions-Scaling-Isolation-and-Diagnostics-Module-(DESID).md#streamline-emissions-unit-conversions-in-desid)

- **Streamlining DESID code**   
  Removes unnecessary variables for online emission streams, making it easier to add new modules. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Detailed-Emissions-Scaling-Isolation-and-Diagnostics-Module-(DESID).md#streamlining-desid-code)

- **Implement Online Met-Dependent Emission Module (MetEmis)** ***[community contribution]***   
CMAQv6.0 introduces the MetEmis module to dynamically calculate meteorology-induced hourly gridded on-road mobile emissions within CMAQ, using simulated meteorology without any computational burden to the CMAQ modeling system. The impact is to improve the spatiotemporal representation of mobile emissions based on the simulated meteorology inputs when compared to the static scenario. For detailed information see Baek et al., 2023. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Online-Met-Dependent-Emission-(MetEmis)-Module.md#implement-online-met-dependent-emission-module-metemis)


### Diagnostic Options
- **Introducing ELMO version 2.1**    
CMAQv6.0alpha introduces ELMO version 2.1 expands the features of ELMO to include gas concentrations, deposition variables, ISAM output, and DDM output. It improves transparency and offers greater flexibility for defining custom aggregates of raw model species (e.g., NOY, NOz, etc.) and assigning them to output files. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#elmo-version-2)

- **Redirect ELMO for indicator of ozone production regime**   
  This update streamlines the process for determining whether the ozone production regime is VOC- or NOx-limited by moving the relevant output variable setting from the EBI solver subroutines to the ISAM routine. The change reduces redundant computations and simplifies code maintenance by centralizing the regime indicator logic. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#redirect-elmo-for-indicator-of-ozone-production-regime)
 
- **Bugfix to the Budget Tool output file**    
  Prevents crashes by correcting the log header output in the Budget Tool's ASCII output file. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#bugfix-to-the-budget-tool-output-file)

- **Reduce model runtime by changing default settings**    
Revises model default from `Budget_Diag = .TRUE.` to `Budget_Diag = .FALSE.`. This change turns off the budget diagnostic tool reducing rutime by 10%. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#changes-in-henrys-law-computation-and-budget-tool)
  
### Structural Improvements
- **Reorganize Aero Module**  
  Refactors the Aerosol module for better code organization and clarity, including renaming and regrouping routines.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#reorganize-aero-module)
  
- **Replace CONST.EXT include file with module and update constant values**   
Replaces the CONST.EXT file with a Fortran module to define model fundamental physical, chemical, and mathematical constants (e.g., PI, MWAIR, etc.). Additionally, the values of several constants have been updated to be consistent with 2019 NIST and SI standards, and an approximation to the error function ERF has been removed.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#replace-constext-include-file-with-module-and-update-constant-values)

- **Reduce model runtime by rewriting HLCONST module**   
The HLCONST module computes Henry's Law constants used in CCTM, to use integer tokens instead of strings. Restructing the code in this module reduces model runtime by approximately 8%. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#changes-in-henrys-law-computation-and-budget-tool)

- **Improvements to compiling with GCC**   
Reduces compiler warnings and updates code to be more compatible with GCC, making it easier to spot new issues.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#improvements-to-compiling-with-gcc)

- **Enable parallel I/O for Lightning and ELMO files**   
  Ensures files are opened on all processors when using parallel file systems, preventing model crashes. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#enable-parallel-io-for-lightning-and-elmo-files)

- **Cap log_message at 1000 lines**   
  Adds a safeguard to prevent infinite loops by capping log_message output at 1000 lines. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#cap-log_message-at-1000-lines)

- **Simplify RETRIEVE_OCEAN_DATA**   
  Streamlines the logic for retrieving ocean data in the centralized I/O module.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#simplify-retrieve_ocean_data)

- **Correct desid_module.F for serial version of CCTM**  
  This update fixes a compile error in desid_module.F that occurred when building a serial version of CCTM by adding logic to handle both parallel and non-parallel cases. As a result, users can now successfully compile and run CCTM in serial mode for debugging and diagnostics.  [Release Notes](../Release_Notes/CMAQ-Release-Notes%3A-Structural-Improvements.md#correct-desid_modulef-for-serial-version-of-cctm)


### On-line coupling of CMAQ with meteorological models
- **Introducing a new unified coupler for WRF-CMAQ and MPAS-CMAQ**  
New module couples WRF-CMAQ and MPAS-CMAQ in a consistent "one-code" framework. Previously, the WRF-CMAQ implementation was built using [I/O API buffered](https://www.cmascenter.org/ioapi/documentation/all_versions/html/BUFFERED.html#buf) files to transfer data from the two models during runtime. A number of infrastructure changes were made were made to implement the system. [Release Note](../Release_Notes/CMAQ-Release-Notes:-WRF-CMAQ-Coupled-Model.md#wrf-cmaq-coupled-model)

### Utilities
- **Add species composition data to RXNS modules and add new reactive rate constant type**    
  This update adds species composition data to the RXNS modules, enabling assessment of elemental conservation in CRACMM-based mechanisms, and introduces a new reactive rate constant type to address a limitation in the previous implementation. These changes improve chemical mechanism documentation and flexibility but do not affect CMAQ model predictions.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Utilities.md#add-species-composition-data-to-rxns-modules-and-add-new-reactive-rate-constant-type)

- **Increase JPROC parameters for input file size**   
  Increases parameter limits in the JPROC utility to support more wavelengths, resolving issues with new mechanisms. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Utilities.md#increase-jproc-parameters-for-input-file-size)

- **Remove pgi compile failure for ebi solver for cracmm3m mechanism**    
This update modifies the create_ebi utility to fix a compile error with the pgi/nvhpc compiler for the cracmm3m EBI solver, caused by exceeding the allowed number of line continuations. As a result, users can now run source apportionment simulations with ISAM and cracmm3m using these compilers.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Utilities.md#remove-pgi-compile-failure-for-ebi-solver-for-cracmm3m-mechanism)

### Pre-processors
- **Update to Meteorology - Chemistry Interface Processor (MCIP)**   
  Introduces several enhancements, including support for WRF urban local climate zones, improved handling of MODIS land use classifications, and corrections to latitude and longitude calculations for polar stereographic projections. The update also restores options for tangent Lambert conformal projections and selective output of time-independent files. These changes expand MCIP's functionality and flexibility for a wider range of use cases, particularly in post-processing, but do not affect CCTM modeling results. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Preprocessors.md#additional-mcip-release-notes-can-be-found-under-the-docsmciphttpsgithubcomusepacmaqtreemainprepmcipdocs-folder)

- **Feature updates to create_omi PREP tool**   
  Updates the create_omi tool to output files in netcdf4 format, removes I/O API dependencies, and improves usability for diagnostics and visualization.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Preprocessors.md#feature-updates-to-create_omi-prep-tool)

### PYTOOLS
- **Script fix for shp2cmaq**
The shp2cmaq.py script was fixed to work properly as both a module and a standalone script, and its output was updated to use a TFLAG and SDATE of 0 for better compatibility as a time-independent file. These changes improve usability but do not affect CMAQ model results. [Release Note](../Release_Notes/CMAQ-Release-Notes:-PYTOOLS.md#shp2cmaq-script-fix)

### Post-processors
- **Expand functionality of calc_tmetric**   
  Enhances the calc_tmetric tool to support more statistical metrics (i.e., min/max over a selected period, etc.), parallel processing, and improved compatibility with visualization tools. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#expand-functionality-of-calc_tmetric)

- **Allow compilation of combine with gcc10+**   
  Enables the combine tool to compile with gcc version 10 and above without special compiler flags. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#allow-compilation-of-combine-with-gcc10)

- **Correct cadmium in SpecDef_Conc_cb6r5hap_ae7_aq.txt**   
  Updates the species name for aerosol cadmium in the concentration definitions file to match the model’s current naming convention.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#correct-cadmium-in-specdef_conc_cb6r5hap_ae7_aqtxt)


<a id=update_v55_v60b></a>
## What do I need to do to update from v5.5 to v6.0alpha?
* If you have already successfully migrated to v5.5, you will not need any additional input to run with the analogous options in v6.0. However, users should note that the CCTM runscripts have changed, so older user-created runscripts  may need to be adapted to be compatible with v6.0. Additionally, if you are trying to run with the newest released version of CRACMM in v6.0, users will have to generate or map existing emissions to CRACMMv3.0. For additional information on emissions for CRACMM please see the CRACMM GitHub page ( https://usepa.github.io/CRACMM/).
  
<a id=diff_v55_v60b_input_files></a>
### What differences should I expect in the required model input files?
* If you have already migrated to v5.5, you will not need any additional input files to run with the analogous options in v6.0 alpha.  CRACMM2 emission inputs can be used to run CMAQv6.0 alpha with the new CRACMM3 mechanism without any modifications. 

* CRACMM1 emissions inputs can be mapped to CRACMM2 species with minor adjustments following this guidance: https://usepa.github.io/CRACMM/emissions/README.html

* Users should always check the main log file and at least one processor log file for any new simulation to verify emissions were properly configured for the simulation.


<a id=diff_v55_v60b_output_files></a>
### What differences should I expect in my model output files?
* CMAQv6.0alpha updates to ELMOv2 replace ELMOv1.0 diagnostic output files. The CCTM defaults now produce the CCTM_ELMO1 which supersedes older CCTM_ELMO files while adding additional gas and particle phase diagnostic aggregates not available in ELMOv1.0. ELMOv2.1 also produces a CCTM_ELMO2_DEP file which includes gas and particle phase dry and wet deposition hourly aggregates.
  
* CMAQv6.0alpha updates two of the three photolysis diagnostic files (CCTM_PHOTDIAG1 and CCTM_PHOTDIAG3). The CCTM_PHOTDIAG1 file remains largely unchanged, with the only changes being in the diagnostic variables: AOD_W550_ANGST (Aerosol Optical Depth at 550 nm based on an Angstrom Interpolation) and AAOD_W550_ANGST (Aerosol Absorption Optical Depth at 550 nm based on an Angstrom Interpolation). The updates now enable calculation of AOD_550 values when the sun is below the horizon. The CCTM_PHOTDIAG3 file changed in three different ways. (1) The variable representing total extinction, the sum of absorption and scattering at various wavelengths from gas, aerosols and clouds, is no longer reported; this variable is now replaced by the cloud extinction; the total extinction can be computed manually as the sum of gas, aerosol and cloud extinction. (2) The photolysis diagnostic variable names "EXT_AERO_W" are changed to "AERO_EXT_W", making the naming convention consistent with how the gas is reported. (3)  The photolysis diagnostics for AOD_550 and aerosol asymmetry and extinction are now available at all simulation hours, independent of the position of the sun.  [Release Note #1](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#remove-uninitialized-variable-and-correct-a-diagnostic-in-cctms-inline-module-for-photolysis-frequencies),  [Release Note #2](../Release_Notes/CMAQ-Release-Notes:-Photolysis.md#remove-uninitialized-variable-and-correct-a-diagnostic-in-cctms-inline-module-for-photolysis-frequencies)
  
<a id=data_and_docs></a>
## Are there new benchmark data and documentation updates?


|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Tutorial**| 
|:----:|:----:|:--------------:|:----:|:--------:|:----:|


<a id=how_to_cite></a>
## How to Cite CMAQ
Please see our 'How to Cite CMAQ' page if you are interested in referencing one of our released model versions, scientific algorithms, or model output in your own publication: https://www.epa.gov/cmaq/how-cite-cmaq. We recommend you cite both the code (using a zenodo doi) for transparency in model version and the peer-reviewed literature supporting the updates (journal article(s)) to credit the scientific work relevant to your simulation.

<a id=additional_faq></a>
## Additional FAQ
A more general list of Frequent CMAQ Questions can be found on our website: https://www.epa.gov/cmaq/frequent-cmaq-questions

<a id=tech_support></a>
## Technical support for CMAQ
Technical support for CMAQ, including questions about model inputs, downloading, compiling, and running the model, 
and pre- and post-processing utilities, should be directed to the [CMAS Center User Forum](https://forum.cmascenter.org/). 
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

<a id=mainbody_references></a>
## References
Baek, B. H., Coats, C., Ma, S., Wang, C.-T., Li, Y., Xing, J., Tong, D., Kim, S., and Woo, J.-H.: Dynamic Meteorology-induced Emissions Coupler (MetEmis) development in the Community Multiscale Air Quality (CMAQ): CMAQ-MetEmis, Geosci. Model Dev., 16, 4659–4676, https://doi.org/10.5194/gmd-16-4659-2023, 2023.

NIST, The International System of Units (SI). Newell, D.B. and Tiesinga, E., eds. NIST Special Publication 330, 2019. doi: 10.6028/nist.sp.330-2019

