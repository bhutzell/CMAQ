# Frequently Asked Questions for Using the CMAQv6.0 BETA Version

## Table of Contents:
* [Do I need to update from v5.5 to v6.0beta?](#why_update_v55_v60b)
* [What do I need to do to update from v5.5 to v6.0beta?](#update_v55_v60b)
  * [What differences should I expect in the required model input files?](#diff_v55_v60b_input_files)
  * [What differences should I expect in my model output files?](#diff_v55_v60b_ouput_files)
* [What differences should I expect in my model results with v6.0beta compared to v5.5?](#diff_v55_v60b_model_results)
* [Are there new benchmark data and documentation updates?](#data_and_docs)
* [Community contributions](#community_contributions)
* [How to cite CMAQ](#how_to_cite)
* [Additional FAQ](#additional_faq)
* [Technical support for CMAQ](#tech_support)

<a id=why_update_v55_v60b></a>
## Do I need to update from v5.5 to v6.0beta?
CMAQv6.0beta includes many scientific enhancements and new features that will benefit certain applications. See the v6.0beta Release Notes for a description of each change.   

#### Instrumented Models

#### On-line coupling of CMAQ with meteorological models
* 


#### Chemistry
* CMAQv6.0beta introduces CRACMM version 3. CRACMM3 includes several updates to CRACMM2. These updates are primarily intended to improve the representation of gas-phase and aerosol chemistry in marine environments. If you are interested in learning more, please see the [CRACMM3 release note](../Release_Notes/CMAQ-Release-Notes:-Chemistry:-Community-Regional-Atmospheric-Chemistry-Multiphase-Mechanism-(CRACMM).md#updated-mechanism-cracmm3).
  
#### Vertical Diffusion & Air Surface Exchange
* CMAQv6.0beta changes the behaviour of the runtime minimum eddy diffusivity (Kz) option called KZMIN. This option, first introduced in CMAQv4.5, is a parametrization to allow the mixing in the planetary boundary layer (PBL) to respond to the land-use characteristics. If the runtime environmental variable KZMIN is set to 'True/Yes', the land-use based paramterizated minium eddy diffusivity will now be applied through the PBL, where as previously it was limited to 500 meters above ground. If KZMIN is set to 'False/No', a constant value of 0.01 m<sup>2</sup>/s is applied everywhere at all times. This change primarily impacts nighttime concentrations, specifically in grid cells where the PBL is lower than 500 meter. In those grid cells, primary emitted species concentrations will increase, where as ozone mixing ratios will decrease due to increased NOx titration. 

#### Emissions
* CMAQv6.0beta fixes a bug related to the estimation of marine-gas halogen emissions within CMAQ. To estimate the emissions of gaseous halogens in marine environments, the grid cell area covering the spatial extent within the domain is needed. Because CMAQ horizontal domains are defined by projecting a map onto a 2-D plane a map-scale factor must be applied when converting physical areas to projected space, which was not taken into account when estimating halogen emissions within this module. This bug fix impacts halogen emission estimates in grid cells in which map scale factors are not unity. For example, if using a northern polar stereographic map projection, this will lead to an increase ozone (less ozone is destroyed by halogens) and decreases sulfate (less is produced via dimethyl sulfide) mostly over lower latitude areas.

* CMAQv6.0beta fixes a bug related to the estimation of windblown dust emissions within CMAQ when using the NLCD40 land-use. To estimate the emissions of windblown dust, grid cell land-use information along with meteorological conditions are needed. In this case, when using WRF with NLCD40 land-use, two cateogries of NLCD40 (“shrub/scrub” and “dwarf scrub”) were being mapped to the wrong internal categories (“barren or sparsely vegetated” instead of “shrubland”). These internal categories are used in teh windblown dust module to assign parameter values controlling erodibility, which in this case was overestimated. Depending on the domain, year and approach to specify vegetation fraction in WRF, this bug fix will likely correct excessinve "soil" PM2.5 and total PM2.5 mass concentrations when using the NLCD40 land-use.

* CMAQv6.0beta introduces the MetEmis module to dynamically calculate meteorology-induced hourly gridded on-road mobile emissions within CMAQ, using simulated meteorology without any computational burden to the CMAQ modeling system. The impact is to better spatiotemporal represent mobile emissions based on the simulated meteorology inputs when compared to the static scenario. For detailed information see Baek et al., 2023. 

#### Process Analysis & Sulfur Tracking Model (STM) 

#### Structural Improvements

#### Diagnostic Options

#### Pre-processors and Utilities

#### Post-processors

#### Python Tools
<a id=update_v55_v60b></a>
## What do I need to do to update from v5.5 to v6.0beta?

<a id=diff_v55_v60b_input_files></a>
### What differences should I expect in the required model input files?
* If you have already successfully migrated to v5.5, you will not need any additional input files to run with the analogous options in v6.0 beta.  CRACMM2 emissions inputs can be used to run CMAQv6.0 beta with the new CRACMM3 mechanism without any modifications. 
* For CRACMM3M (new marine version), an additional emission, CH3I, is needed. This will occur automatically for in-line biogenic emissions, but offline emissions will need to have CH3I explicit.
* CRACMM1 emissions inputs can be mapped to CRACMM2 species with minor adjustments following this guidance: https://usepa.github.io/CRACMM/emissions/README.html

<a id=diff_v55_v60b_ouput_files></a>
### What differences should I expect in my model output files?

<a id=diff_v55_v60b_model_results></a>
## What differences should I expect in my model results with v6.0beta compared to v5.5?

#### Natural Emissions

### Ozone


* **Summary**:
  
### PM2.5

* **Summary**:
  
### Deposition
  
<a id=data_and_docs></a>
## Are there new benchmark data and documentation updates?


|**CMAQ Version**|**Data Type (Size)**|**Domain**|**Simulation Dates**|**Data Access**|**Tutorial**| 
|:----:|:----:|:--------------:|:----:|:--------:|:----:|


<a id=community_contributions></a>
## Community Contributions
The CMAQ team would like to thank our user community for contributing to model updates in CMAQv6.0beta by identifying issues, performing tests, and/or proposing code changes. The following v6.0beta updates include a community contribution*.

\*We attempted to be comprehensive in this list but if we have missed a contribution from you or a colleague, please forgive our oversight and contact us at [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov) so that we can correct our mistake. Thank you for helping us improve the CMAQ modeling system!   

<a id=how_to_cite></a>
## How to Cite CMAQ
Please see our 'How to Cite CMAQ' page if you are interested in referencing one of our released model versions, scientific algorithms, or model output in your own publication: https://www.epa.gov/cmaq/how-cite-cmaq

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
