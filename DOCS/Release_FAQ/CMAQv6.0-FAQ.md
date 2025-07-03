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
- **Add ISAM support for all CRACMM versions**  
CMAQv6.0alpha introduces CMAQ-ISAM compatibility with CRACMM2, CRACMM3, & CRACMM3M as well as several improvements that target CMAQ-ISAM robustness for all chemical mechanisms. [Release Note](../Release_Notes/CMAQ-Release-Notes%3A-Instrumented-Models%3A-CMAQ-ISAM.md)
 
- **Improve stability in ISAM apportionment output**   
  Addresses errors in aerosol/gas partitioning in ISAM, preventing mass from being incorrectly allocated to distant tags.  
  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#improve-stability-in-isam-apportionment-output)
  
- **Fix ISAM erroneous mass attribution**  
  Fixes a bug that caused tagged mass to appear far from emission sources due to improper handling of aerosol mass concentrations.  
  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-ISAM.md#erroneous-mass-attribution)

- **Address DDM3D instability in sensitivity fields**  
   CMAQv6.0 fixes DDM3D instablity in after hetereogenous chemistry due to inconsistent unit conversions. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-DDM3D.md)

- **DDM3D control file character limit increase**  
  Increases the character limit for specification strings in the DDM-3D control file to support more complex applications.  
  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Instrumented-Models:-CMAQ-DDM3D.md#ddm-3d-control-file-character-limit-increase)
  
### Chemistry
- **Updates to Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM)**
CMAQv6.0alpha introduces CRACMM version 3. CRACMM3 includes several updates to CRACMM2. These updates improve the representation of gas-phase and aerosol chemistry in multiple environments. In addition to the base CRACMM3 mechanism, CMAQv6.0alpha includes CRACMM3M, with extended marine chemistry, and CRACMM3HAPS which includes additional Hazardous Air Pollutants.  If you are interested in learning more, please see the CRACMM3 release notes:
  - **CRACMM3 Overview**  
 [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updated-mechanism-cracmm3)

  - **Heterogeneous chemistry of sulfur species**  
  Adds heterogeneous sulfur chemistry to improve particulate sulfur predictions in areas with high winter PM pollution. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#heterogeneous-chemistry-of-sulfur-species)
  - **Adding chlorine chemistry in CRACMM3**  
  Incorporates chlorine chemistry from CRACMM3M into CRACMM3, using a reduced set of organic reactions to limit computational demand. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#adding-chlorine-chemistry-in-cracmm3)

  - **Photolysis of aerosol nitrate in CRACMM3**  
  Adds photolysis of aerosol nitrate and a new aerosol species to CRACMM3, with updated molecular weights and solver enhancements. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#photolysis-of-aerosol-nitrate-in-cracmm3)

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

  - **Correct conservation of nitrogen for 4 reactions in CRACMM3**  Implements fixes to ensure nitrogen conservation in four specific reactions, based on expert recommendations.  
  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#correct-conservation-of-nitrogen-for-4-reactions-in-cracmm3)

  - **CRACMM Reaction Metadata File**  
 Provides a metadata file documenting updates to CRACMM chemistry at the reaction level.  [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#cracmm-reaction-metadata-file)

  - **Streamline chemical mechanisms**
CMAQv6.0alpha no longer supports the following mechanisms: CB6R3_AE7_AQ, CRACMM1_AQ, CRACMM1AMORE_AQ, RACM2_AE6_AQ, and SAPRC07TIC_AE7i_AQKMT2. The update ensures consistency across remaining mechanisms and utilities. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#delete-obsolete-chemical-mechanisms)

- **Updates to CarbonBond 6 Mechanism with Hazardous Air Pollutants (CB6r5HAP)**
    - **Correct rate constant for styrene's reaction with ozone**   
Bug fix to cb6r5hap_ae7_aq mechanism corrects a severe underestimation of the model species styrene, a hazardous air pollutant. No other model species are impacted. [Release Note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Carbon-Bond-6-Mechanism-(CB6)-with-Hazardous-Air-Pollutants.md#correct-loss-of-reactive-tracer-styrene-from-ozone-reaction)
  

  
* CMAQv6.0alpha no longer supports the following mechanisms: CB6R3_AE7_AQ, CRACMM1_AQ, CRACMM1AMORE_AQ, RACM2_AE6_AQ, and SAPRC07TIC_AE7i_AQKMT2. Users that are interested in using these mechanisms will need to downgrade their CMAQ version. 
  
#### Vertical Diffusion & Air Surface Exchange
* CMAQv6.0alpha changes the behavior of the runtime minimum eddy diffusivity (Kz) option called KZMIN. This option, first introduced in CMAQv4.5, is a parameterization to allow the mixing in the planetary boundary layer (PBL) to respond to the land-use characteristics. If the runtime environmental variable KZMIN is set to 'True/Yes', the land-use based parameterized minimum eddy diffusivity will now be applied through the PBL, whereas previously it was limited to 500 meters above ground. If KZMIN is set to 'False/No', a constant minimum value of 0.01 m<sup>2</sup>/s is applied everywhere at all times. This change primarily impacts nighttime concentrations, specifically in grid cells where the PBL is lower than 500 meters. In those grid cells, primary emitted species concentrations will increase, whereas ozone mixing ratios will decrease due to increased NOx titration. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Dry-Deposition-Air-Surface-Exchange:-Surface-Tiled-Aerosol-and-Gaseous-Exchange-(STAGE).md#update-to-minimum-kz-and-kz0ut-in-the-stage-deposition-option))

#### Emissions
* CMAQv6.0alpha introduces the Soil – Atmosphere Gaseous Emissions (SAGE) module for estimating soil NO and HONO emissions generally following the BDSNP parameterization (Hudman et al. 2012). SAGE  provides a simple, meteorological dependent soil NO and HONO emissions for regional to global applications. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Soil-Atmospheric-Gaseous-Emissions-(SAGE)-Emissions.md#introducing-new-soil-emissions-module))

* CMAQv6.0alpha introduces a new satellite-based global vegetation dataset to modulate windblown dust emissions. The vegetation dataset accounts for the effect of non-photosynthetic vegetation (or brown vegetation) by updating vegetation fraction, the vegetation height, and the vegetation roughness following the method outlined in [Huang and Foroutan (2022, HF22)](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2021JD035243). The overall impact is to reduce windblown dust emissions. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Wind-Blown-Dust-Emissions.md))

* CMAQv6.0alpha fixes a bug related to the estimation of windblown dust emissions within CMAQ when using the NLCD40 land-use classification scheme. To estimate the emissions of windblown dust, grid cell land-use information along with meteorological conditions are needed. In this case, when using WRF with NLCD40 land-use, two categories of NLCD40 (“shrub/scrub” and “dwarf scrub”) were being mapped to the wrong internal categories (“barren or sparsely vegetated” instead of “shrubland”). These internal categories are used in the windblown dust module to assign parameter values controlling erodibility, which in this case was overestimated. Depending on the domain, year and approach to specify vegetation fraction in WRF, this bug fix corrects excessive "soil" PM2.5 and total PM2.5 mass concentrations when using NLCD40 land-use. ([Release Note](../Release_Notes/CMAQ-Release-Notes%3A-Emissions-Updates%3A-Wind-Blown-Dust-Emissions.md#correction-for-nlcd40-land-use-mapping-in-windblown-dust-module))

* CMAQv6.0alpha fixes a bug related to the estimation of marine-gas halogen emissions within CMAQ. To estimate the emissions of gaseous halogens in marine environments, the grid cell area covering the spatial extent within the domain is needed. Because CMAQ horizontal domains are defined by projecting a map onto a 2-D plane a map-scale factor must be applied when converting physical areas to projected space, which was not taken into account when estimating halogen emissions within this module. This bug fix impacts halogen emission estimates in grid cells in which map scale factors are not unity. For example, if using a northern polar stereographic map projection, this will lead to increased ozone (less ozone is destroyed by halogens) and decreases sulfate (less is produced via dimethyl sulfide) mostly over lower latitude areas.

* CMAQv6.0alpha introduces the MetEmis module to dynamically calculate meteorology-induced hourly gridded on-road mobile emissions within CMAQ, using simulated meteorology without any computational burden to the CMAQ modeling system. The impact is to improve the spatiotemporal representation of mobile emissions based on the simulated meteorology inputs when compared to the static scenario. For detailed information see Baek et al., 2023. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Emissions-Updates:-Online-Met-Dependent-Emission-(MetEmis)-Module.md#implement-online-met-dependent-emission-module-metemis))

#### Diagnostic Options

* CMAQv6.0alpha introduces ELMO version 2.1 expands the features of ELMO to include gas concentrations, deposition variables, ISAM output, and DDM output. It improves transparency and offers greater flexibility for defining custom aggregates of raw model species (e.g., NOY, NOz, etc.) and assigning them to output files. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#elmo-version-2))

* CMAQv6.0alpha revises model default from `Budget_Diag = .TRUE.` to `Budget_Diag = .FALSE.`. This change turns off the budget diagnostic tool reducing rutime by 10%. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#changes-in-henrys-law-computation-and-budget-tool))
  
#### Structural Improvements
* CMAQv6.0alpha replaces the CONST.EXT file with a Fortran module to define model fundamental physical, chemical, and mathematical constants (e.g., PI, MWAIR, etc.). Additionally, the values of several constants have been updated to be consistent with 2019 NIST and SI standards, and an approximation to the error function ERF has been removed. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Structural-Improvements.md#replace-constext-include-file-with-module-and-update-constant-values))

* CMAQv6.0alpha rewrites the HLCONST module, which computes Henry's Law constants used in CCTM, to use integer tokens instead of strings reducing model runtime by approximately 8%. ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Diagnostic-Options.md#changes-in-henrys-law-computation-and-budget-tool))

#### On-line coupling of CMAQ with meteorological models
* CMAQv6.0alpha introduces the unified coupler to couple WRF-CMAQ and MPAS-CMAQ in a consistent "one-code" framework. Previously, the WRF-CMAQ implementation was built using [I/O API buffered](https://www.cmascenter.org/ioapi/documentation/all_versions/html/BUFFERED.html#buf) files to transfer data from the two models during runtime. Users should note to implement such a system, a number of infrastructure changes were made. See the [WRF-CMAQ release note](../Release_Notes/CMAQ-Release-Notes:-WRF-CMAQ-Coupled-Model.md#wrf-cmaq-coupled-model).


#### Post-processors

* CMAQv6.0alpha introduces expanded functionality of the CALC_TMETRIC tool. These updates improve efficiency when processing large data sets and provide the ability to produce additional metrics of interest to users (i.e., min/max over a selected period, etc.) ([Release Note](../Release_Notes/CMAQ-Release-Notes:-Postprocessors.md#expand-functionality-of-calc_tmetric)). 

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
  
* CMAQv6.0alpha updates two of the three photolysis diagnostic files (CCTM_PHOTDIAG1 and CCTM_PHOTDIAG3). The CCTM_PHOTDIAG1 file remains largely unchanged, with the only changes being in the diagnostic variables: AOD_W550_ANGST (Aerosol Optical Depth at 550 nm based on an Angstrom Interpolation) and AAOD_W550_ANGST (Aerosol Absorption Optical Depth at 550 nm based on an Angstrom Interpolation). The updates now enable calculation of AOD_550 values when the sun is below the horizon. The CCTM_PHOTDIAG3 file changed in three different ways. (1) The variable representing total extinction, the sum of absorption and scattering at various wavelengths from gas, aerosols and clouds, is no longer reported; this variable is now replaced by the cloud extinction; the total extinction can be computed manually as the sum of gas, aerosol and cloud extinction. (2) The photolysis diagnostic variable names "EXT_AERO_W" are changed to "AERO_EXT_W", making the naming convention consistent with how the gas is reported. (3)  The photolysis diagnostics for AOD_550 and aerosol asymmetry and extinction are now available at all simulation hours, independent of the position of the sun.  ([Release Note #1](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Photolysis.md#remove-uninitialized-variable-and-correct-a-diagnostic-in-cctms-inline-module-for-photolysis-frequencies),  [Release Note #2](../Release_Notes/CMAQ-Release-Notes:-Photolysis.md#remove-uninitialized-variable-and-correct-a-diagnostic-in-cctms-inline-module-for-photolysis-frequencies))
  
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

