CMAQv6.0 BETA 1
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an open-source development project of the U.S. EPA that consists of a suite of programs for conducting air quality model simulations. CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone, particulates and toxics in the air we breathe, as well as deposition of pollutants such as acids, nutrients, and additional substances to our land and water.


## CMAQ version 6.0 Beta Overview:
While initially planned for fall 2026, the status and timing of any final release of CMAQv6.0 is to be determined. This CMAQv6.0 beta release includes two research versions of the CMAQ system (beta 1 and beta 2) that allow community members:

* a preview of science and feature updates planned for the CMAQv6.0 release.
* the ability to take advantage of improvements for preliminary studies of their own interest.
* help other interested community members by testing, troubleshooting, and debugging the research version before any potential future final release.

### New features in CMAQ version 6.0 include:
* Item 1 
* Item 2
* ...

These updates and other new features in the beta versions are documented in the **[CMAQv6.0beta Release Notes](DOCS/Release_Notes/README.md).**

## Differences in CMAQv6.0 beta 1 and beta 2:
The beta 1 version (v6.0b1; **this branch**) includes the science and feature updates described above.   

The beta 2 version (v6.0b2) is identical to version 1 but removes the dependency of the CMAQ Chemical Transport Model (CCTM) on the I/O API library.  This update makes the code for the offline CMAQ model more consistent with the two coupled versions, WRF-CMAQ and MPAS-CMAQ, allowing for substantially easier developement and maintenance across all three versions.   To implement this update input/output functions and other utilities such as calendar functions that previously relied on the I/O API library (developed and maintained by [Carlie Coats](https://github.com/cjcoats)) have been added to the CMAQ source code under CCTM/src/mio and CCTM/src/misc. The beta 2 version also moves functions related to log warnings and messages from the RUNTIME_VARS module into the LOGDEV_MOD module (both under CCTM/src/util/util/).  

**Next Steps** The I/O updates in v6.0b2 were designed to facilitate creating CMAQ output files with variables of mixed dimensions.  When implemented in a future version of CMAQ, this new feature will allow users to produce many fewer output files per simulation, e.g., 2D and 3D gridded variables can be written to a single file.

## Getting the CMAQ Repository
This CMAQ Git archive is organized with each version stored as a branch on the main USEPA/CMAQ repository. The most recently released official version of the model will always be on the branch called 'main'. To clone code from the CMAQv6.0 beta versions issue the following command from within a working directory on your server:

**Beta 1 Version**
```
git clone -b 6.0b1 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```
**Beta 2 Version**
```
git clone -b 6.0b2 https://github.com/USEPA/CMAQ.git CMAQ_REPO
```

## CMAQ Repository Guide
Source code and scripts are organized as follows:
* **CCTM (CMAQ Chemical Transport Model):** code and scripts for running the 3D-CTM at the heart of CMAQ.
* **DOCS:** Release Notes, Release FAQ, Getting Started reference page, User's Guide, and short tutorials.
* **PREP:** Data preprocessing tools for important input files like initial and boundary conditions, meteorology, etc.
* **POST:** Data postprocessing tools for aggregating and evaluating CMAQ output products (e.g. Combine, Site-Compare, etc)
* **PYTOOLS:** Python pre- and postprocessing tools
* **UTIL:** Utilities for generating code and using CMAQ (e.g. chemical mechanism generation)

## CMAQv6.0 Beta Documentation
The Tutorials and User's Guide included in this repository were last updated for the CMAQv5.5 release. CMAQv6.0 updates will be included in the final official release. Information on the CMAQv6.0 beta versions are included in the **[CMAQ Release Notes](DOCS/Release_Notes/README.md).**


## CMAQ Test Cases 
Test case input and output data for the CMAQv5.5 release are available from the CMAS Data Warehouse. Step-by-step benchmark tutorials using the test case data are provided in the GitHub repo. These Test Case datasets may be adapted to work with the CMAQv6.0 beta versions.
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
