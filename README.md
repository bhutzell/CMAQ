CMAQv6.0 ALPHA 
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an open-source development project of the U.S. EPA that consists of a suite of programs for conducting air quality model simulations. CMAQ combines emerging knowledge in atmospheric science and air quality modeling with advances in computational techniques in an open-source framework to deliver scientifically sound estimates of ozone, particulates and toxics in the air we breathe, as well as deposition of pollutants such as acids and nutrients to our land and water.

CMAQ is supported by the CMAS Center: http://www.cmascenter.org 

## CMAQ version 6.0 Alpha Overview:

The science updates and new features in the alpha version (v6.0a1) are documented in the [CMAQv6.0alpha Release Notes](DOCS/Release_Notes/README.md) and summarized in the **[Release FAQ](DOCS/Release_FAQ/CMAQv6.0-FAQ.md)**.

The official release of CMAQv6.0 is targeted for fall 2026, pending the availability of necessary resources. This pre-release version has been preliminarily tested on multiple spatial domains – however aspects of the system remain uncharacterized. Users assume the risk of unforeseen or undocumented impacts of code improvements that have been incorporated since the CMAQv5.5 public release.

This CMAQv6.0 alpha release allows early adopters:

* a preview of science and feature updates developed for the CMAQv6.0 release.
* an opportunity to help the the broader user community by testing, troubleshooting, and debugging this version.


## New features in CMAQ version 6.0 include:
* **CRACMM3: new state-of-the-science chemical mechanisms**
  * Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 3 adds chlorine chemistry, heterogeneous sulfur chemistry, and particle nitrate (pNO3) photolysis and improves conservation of carbon across reactions. In addition, it updates reactions for several systems including semivolatile organic compounds. 
 * These updates and other features improve predictions of hazardous air pollutants, PM2.5, and ozone and more explicitly tie precursor sources to resulting pollutant concentrations downwind which should lead to improved sensitivity and source attribution.

* **Major advancements in windblow dust estimates**
  * Dramatic improvement of windblown dust emissions for NLCD40 land-use specification address high bias in dust estimates from earlier CMAQ versions.
  * New satellite-based global vegetation dataset accounts for the effect of previously underestimated brown vegetation and further improves dust estimates for many regions in the US and Northern Hemisphere.
  * These improvements apply to both the U.S. and hemispheric scale simulations.

* **Improvements to source apportionment tools, CMAQ-ISAM and CMAQ-DDM3D** 
  * Tagged source apportionment modeling via CMAQ-ISAM is now compatible with the most up-to-date chemistry CRACMM2, CRACMM3, CRACMM3M, and CRACMM3HAPS.
  * Sensitivity-based source apportionment via DDM3D is now more robust after instabilities from heterogenous chemistry have been resolved. In addition, CMAQ-DDM3D now supports using the STAGE dry deposition module.

* **New customization options and simplified user experience**
  * The Explicit and Lumped air quality Model Output module (ELMO) version 2 offers:
    * expanded features for gas and deposition species. ELMOv1 focused on support for aerosol species.
    * full flexibility for defining aggregates (e.g., VOC, NOY, NOz, etc.) and assigning them to output files.
    * new chemical and meteorological diagnostic variables available for output. Tutorials are provided to support users in adding custom variables themselves.
    * automatic logging of the composition of aggregate output variables like PM2.5, fine-mode organic aerosol (PMF_OA), total Nitrogen deposition, etc.
    * new support for source apportionment tools, CMAQ-ISAM and CMAQ-DDM3D. For example, source-resolved PM2.5 and NOx may now be output directly.  Users no longer have to prescribe manually how to sum source-resolved species together.
  * Consolidated list of chemical mechanisms, highlighting the completion of CRACMM development milestones. See https://www.epa.gov/cmaq/cracmm for more details.
  * Two dry deposition modules, STAGE and M3DRY, are now both built in model executables and may be selected at run-time.
  * Direct user specification of inorganic aerosol mass transfer is now possible! Previously, CMAQ only supported hybrid mass transfer (dynamic for coarse, equilibrium for fine). Now users can assess trade-offs by forcing all modes to dynamic or equilibrium and compare to the default approach.

* **Improved methods for land-surface impacts on deposition, emissions, and dilution** 
  *	The resistance to dry deposition of volatile carbon-containing compounds has been increased consistent with their vapor-pressures. This increases VOC and CO concentrations across model applications.
  * Boundary-layer mixing dynamics in stable conditions have been made consistent with upstream meteorological models.
  * Enhanced options for online coupling of CMAQ chemistry to meteorological models. First public release of a unified coupler to support WRF-CMAQ and MPAS-CMAQ in a harmonized framework.
 

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each version stored as a branch on the main USEPA/CMAQ repository. The most recently released official version of the model will always be on the branch called 'main'. 
To clone code from the CMAQv6.0 alpha version issue the following command from within a working directory on your server:

**Alpha 1 Version**
```
git clone -b 6.0a1 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```


## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release Notes, Release FAQ, Getting Started reference page, User's Guide, and short tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **PYTOOLS:** Python pre- and postprocessing tools
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## CMAQv6.0 Alpha Documentation
The User's Guide chapters, tutorials, and appendices related to ELMOv2.1 and DESID have been updated for CMAQv6.0 alpha.  All other User's Guide content in this repository was last updated for the CMAQv5.5 release. Additional documentation updates will be included in the CMAQv6.0 release. Information on the updates in CMAQv6.0 alpha is included in the **[CMAQ Release Notes](DOCS/Release_Notes/README.md).**


## CMAQ Test Case  
A full set of inputs for 2022 are provided for the 12US1 domain, including emissions compatible with both the CRACMM2 and CRACMM3 chemical mechanisms. Input files can be used for running CMAQv5.5 (with CRACMM2) or CMAQv6.0 alpha (with CRACMM2 or CRACMM3). 
* [CMAQ Data](DOCS/CMAQ_Data.md)


## Other Online Resources 
* [Resources for Running CMAQ on Amazon Web Services](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#cmaq-on-the-cloud)
* [Software Programs for Preparing CMAQ Inputs](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#prepare_cmaq_inputs)
* [Software Programs for Evaluating and Visualizing CMAQ Outputs](https://www.epa.gov/cmaq/cmaq-resourcesutilities-model-users#evaluate_visualize_cmaq)
* [2000 - 2023 air quality observation data from the CMAS Center Data Warehouse](https://drive.google.com/drive/u/1/folders/1QUlUXnHXvXz9qwePi5APzzHkiH5GWACw) - These files are formatted to be compatible with the [Atmospheric Model Evaluation Tool](https://www.epa.gov/cmaq/atmospheric-model-evaluation-tool).
  
## User Support
* [Frequent CMAQ Questions](https://www.epa.gov/cmaq/frequent-cmaq-questions) are available on our website.
* [Debugging tips](https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Tutorials/CMAQ_UG_tutorial_debug.md) are included with the CMAQ tutorials. 
* [The CMAS User Forum](https://forum.cmascenter.org/) is available for users and developers to discuss issues related to using the CMAQ system.
 [**Please read and follow these steps**](https://forum.cmascenter.org/t/please-read-before-posting/1321) prior to submitting new questions to the User Forum.

## EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

* [Open source license](license.md)

