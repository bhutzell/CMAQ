CMAQv6.0 ALPHA 
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an open-source development project of the U.S. EPA that consists of a suite of programs for conducting air quality model simulations. CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor computing techniques in an open-source framework to deliver scientifically sound estimates of ozone, particulates and toxics in the air we breathe, as well as deposition of pollutants such as acids and nutrients to our land and water.


## CMAQ version 6.0 Alpha Overview:

The science updates and new features in the alpha version (v6.0a1) are documented in the [CMAQv6.0alpha Release Notes](DOCS/Release_Notes/README.md) and summarized in the **[Release FAQ](DOCS/Release_FAQ/CMAQv6.0-FAQ.md)**.

While initially planned for fall 2026, the status and timing of any final release of CMAQv6.0 is to be determined. This CMAQv6.0 alpha release allows community members:

* a preview of science and feature updates planned for the CMAQv6.0 release.
* the ability to take advantage of improvements for preliminary studies of their own interest.
* help other interested community members by testing, troubleshooting, and debugging the research version before any potential future final release.

## New features in CMAQ version 6.0 include:
* Gas, Aqueous, & Aerosol Chemistry 
  * Community Regional Atmospheric Chemistry Multiphase Mechanism (CRACMM) version 3 including particle nitrate (pNO3) as a heterogenous reaction 
  * CRACMM3M, with inclusion of halogen chemistry to improve the representation of gas-phase and aerosol chemistry in marine environments.
  * CRACMM3HAPS which includes additional gas chemistry for Hazardous Air Pollutants.
  * Henry’s law rate constant updates 
  * Photolysis rate constant updates 
* Transport Processes & Land Surface Exchange 
  * Changes to runtime minimum eddy diffusivity option (KZMIN) 
* Emissions
  * Corrections to estimation of marine-gas halogen emissions 
  * Corrections to estimation of windblown dust emissions for NLCD40 land-use specification 
  * Release of MetEmis Module to dynamically calculate meteorology-induced on-road mobile emissions within CMAQ (**community contribution based on [Baek et al., 2023](https://doi.org/10.5194/gmd-16-4659-2023))
  * New lightning options 
* Instrumented Models (CMAQ-ISAM & CMAQ-DDM3D)
  * Introducing CMAQ-ISAM compatibility with CRACMM2, CRACMM3, & CRACMM3M 
  * Additional development to improve robustness for all chemical mechanisms 
  * Fixes to DDM3D instabilities relating to propagating sensitivity fields through heterogenous chemistry due to inconsistent unit conversions 
* Diagnostic Model Output
  * Upgrade from ELMOv1.0 to ELMOv2.1 – added new capabilities for model output 
  * Updates to photolysis diagnostic outputs (CCTM_PHOTDIAG1 & CCTM_PHOTDIAG3 ) 
* Coupled model system
  * First public release of unified coupler to couple WRF-CMAQ and MPAS-CMAQ in a consistent “one-code” framework 
* Structural Improvements 
  * Moved CONST.EXT (defines define model fundamental physical, chemical, and mathematical constants) to Fortran module and updated constants to latest 2019 NIST and SI standards 
  * Rewrote HLCONST to use integer tokens instead of strings reducing model runtime by approximately 8% 

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


## CMAQ Test Cases 
Test case input and output data for the CMAQv5.5 release are available from the CMAS Data Warehouse. Step-by-step benchmark tutorials using the test case data are provided in the GitHub repo. These Test Case datasets may be adapted to work with the CMAQv6.0 alpha versions.
* [CMAQ Test Case Data](DOCS/Test_Case_Data.md)


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
