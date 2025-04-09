CMAQv6.0 BETA 1
==========

US EPA Community Multiscale Air Quality Model (CMAQ) Website: https://www.epa.gov/cmaq

CMAQ is an active open-source development project of the U.S. EPA's Office of Research and Development that consists of a suite of programs for conducting air quality model simulations. CMAQ is supported by the CMAS Center: http://www.cmascenter.org

CMAQ combines current knowledge in atmospheric science and air quality modeling with multi-processor computing techniques in an open-source framework to deliver fast, technically sound estimates of ozone, particulates, toxics, and acid deposition.


## CMAQ version 6.0 Beta Overview
The CMAQ6.0 beta release includes two research versions of the CMAQ system (beta 1 and beta 2) that allow community members:

* a preview of science and feature updates planned for the final CMAQv6.0 release.
* a role in helping to test, troubleshoot, and debug the development code before the final release.
* the ability to take advantage of improvements for preliminary studies of their own interest.

### New features in CMAQ version 6.0 include:
* Item 1 
* Item 2
* ...

These updates and other new features in the beta versions are documeted in the **[CMAQv6.0beta Release Notes](DOCS/Release_Notes/README.md).**

While initially planned for fall 2026, the status and timing of any final release of CMAQv6.0 is unknown at this point.

## Differences in CMAQv6.0 beta 1 and beta 2
The beta 1 version (v6.0b1; **this branch**) includes the science and feature updates described above.   

The beta 2 version (v6.0b2) removes the dependency of the CMAQ Chemical Transport Model (CCTM) on the I/O API library.  This update makes the code for the offline CMAQ model consistent with the two coupled versions, WRF-CMAQ and MPAS-CMAQ, allowing for easier developement and maintenance across all three versions.


## Getting the CMAQ Repository
This CMAQ Git archive is organized with each version stored as a branch on the main USEPA/CMAQ repository. The most recently released official version of the the model will always be on the branch called 'main'. To clone code from the CMAQv6.0 beta versions issue the following command from within
a working directory on your server:

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
