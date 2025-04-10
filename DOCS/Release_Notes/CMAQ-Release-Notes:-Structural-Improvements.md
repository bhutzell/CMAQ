# Structural Improvements

### MIO: New functions for input/output commands and other utilities
[Chris Nolte](mailto:nolte.chris@epa.gov) and [David Wong](mailto:wong.david-c@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Restructure   
**Release Version/Date**:  CMAQv6.0 *beta 2 only*

**Description**:  
The MIO update removes the dependency of the CMAQ Chemical Transport Model (CCTM) on the I/O API library. This update makes the code for the offline CMAQ model consistent with the two coupled versions, WRF-CMAQ and MPAS-CMAQ, allowing for substantially easier developement and maintenance across all three versions. To implement this update input/output functions and other utilities such as calendar functions that previously relied on the I/O API library (developed and maintained by [Carlie Coats](https://github.com/cjcoats)) have been added to the CMAQ source code under CCTM/src/mio. In addition, this update moves functions related to log warnings and messages from the RUNTIMEVARS module into the logdev_mod module (both under CCTM/src/util/util/).  

(*talk about how current implantation relies on input file to specify the output files and variables to be written to each output file (this interacts with ELMO). this file is generated in the code from user settings.*)  
(*talk about replacement_util module?*)

The following environment variables are required in the CCTM run script to utilize the new MIO module. Note these updates are included in the sample runscripts of v60b2 under CCTM/scripts.
1. remove the option -v from output file environment variables, e.g., ```setenv CTM_CONC_1      $OUTDIR/CCTM_CONC_${CTM_APPL}.nc ```
2. ```setenv CTM_MIO_FILE Y``` *(turn on generation of MIO_ASCII file; code will crash if this is not set to Y)*  
3. ```setenv MISC_FILE_INFO ${path}$``` *(directory where MIO file will be written)*  
4.  ```
    setenv mio_file_info $OUTDIR/mio_file_input_${CTM_APPL}.txt  
    setenv CTM_MIO_INPUT "INIT_CONC_1"
    ```
    *(for now, need at least one input file)*  <- I think this will require further explanation
5. ```setenv ncd_64bit_offset Y``` *(needed when using netcdf4)*    

**Significance and Impact**:   
[other positive things about mio's purpose ...] CMAQ output files are unchanged by this update.  Although CCTM no longer requires installation of the I/O API library several PREP and POST tools within the CMAQ repository retain this dependency (e.g., ICON, BCON, COMBINE).  \

There are several options in the CMAQ system that have not yet been implemented with MIO and so will not work with this version of CMAQv6.0 beta.  These include:
1. VERTEXT option (<- this option is not actually listed in Appendix A!)
2. Generating MCIP-like outputs when running the WRF-CMAQ coupled model
3. [Windowing capability](../Users_Guide/CMAQ_UG_ch04_model_inputs.md#431-windowing-capability) (i.e, subsetting inputs when the domain of the input files is larger than the simulation domain)

**References**:   
Portions Copyright ©1992-2002 MCNC and Carlie J. Coats, Jr., 2003-2013 by Baron Advanced Meteorological Systems, © 2005-2013, 2017- Carlie J. Coats, Jr., and , and © 2014- UNC Institute for the Environment. Please see the disclaimers contained in the (I/O API Copyright Notice file)[https://cjcoats.github.io/ioapi/NOTICES.html].


|Merge Commit | Internal record|
|:------:|:-------:|


### Replace CONST.EXT include file with module and update constant values  
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Restructure   
**Release Version/Date**: CMAQv6.0 beta 1 and beta 2

**Description**:   
In this PR, the code is restructured to define and use a CONST module in lieu of the CONST.EXT include file. 
The module includes fundamental physical, chemical, and mathematical constants used in CMAQ as well as certain commonly used statement functions, particularly `ESATL` for calculating the saturation vapor pressure of water as a function of temperature.  The values of Avogadro's number, the Boltzmann constant, and the universal gas constant are updated to be consistent with the latest (2019) NIST and SI standards. The single and double precision versions of these constants are also made consistent with each other.
Additionally, the Meng and Seinfeld (1994) approximation to the error function ERF has been removed. ERF and its complement ERFC are intrinsic Fortran functions since the 2008 standard. 

**Significance and Impact**:   
Very minor change in model results. Easier code maintenance and better consistency.  

**References**:   
NIST, The International System of Units (SI). Newell, D.B. and Tiesinga, E., eds. NIST Special Publication 330, 2019.   doi: 10.6028/nist.sp.330-2019  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1137](https://github.com/USEPA/CMAQ_Dev/commit/e7ed66e185b1b93af8515428053465564ae6857c) | [PR#1137](https://github.com/USEPA/CMAQ_Dev/pull/1137)  | 
|[Merge for PR#1138](https://github.com/USEPA/CMAQ_Dev/commit/96449cd6f20eccf61699cee038317b6ffaed467a) | [PR#1138](https://github.com/USEPA/CMAQ_Dev/pull/1138)  |   

### GNU build flag update to enable compilation with GNU versions 10+
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**:  v5.5  

**Description**:  Starting GNU version 10+, GNU no longer allows rank mismatches between the callee and the calling function. The exact verbiage from the GNU change logs:"Mismatches between actual and dummy argument lists in a single file are now rejected with an error. Use the new option -fallow-argument-mismatch to turn these errors into warnings; this option is implied with -std=legacy. -Wargument-mismatch has been removed.” (https://gcc.gnu.org/gcc-10/changes.html)

“GCC 10 now rejects argument mismatches occurring in the same source file. Those are not permitted by the Fortran standard and in general have the potential to generate invalid code. However, the Fortran standard does permit passing an array element or a scalar string (of default character kind or of c_char kind) as actual argument to an array dummy argument. (For the exact wording, see the Fortran standard on argument association; in particular, Fortran 2018, Sect. 15.5.2.4, Para. 4.)

Depending on their nature, argument mismatches have the potential to cause the generation of invalid code and, hence, should be investigated. The most common reason that code fails due to newly enforced check is the following: instead of using an array element as actual argument, a scalar is used; one solution is to replace the scalar by a size-one array. (This should be passed as a whole as there is no point in passing it as array element.) Additionally, check that the code indeed only accesses this single element. — Other mismatches occur more rarely but usually indicate more serious bugs where a wrong result is likely (at least for some target-platform and optimization combination).”

The non-FORTRAN explanation boils down to the ability to pass 1-D arrays, 2-D arrays, 3-D arrays, etc., into a subroutine or function and have the called routine set up so that that it "does the right thing". This is in fact a common occurrence, where the callee "single-indexes" multi-dimensional arrays.

**References**:  n/a
|Merge Commit | Internal record|
|:------:|:-------:|
| [Merge for PR#1154](https://github.com/USEPA/CMAQ_Dev/commit/c31983b72a3049d708138da3f57227875333eb39) |  [PR#1154](https://github.com/USEPA/CMAQ_Dev/pull/1154) |

### Emissions Diagnostics and Log Output
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Diagnostic and Log Updates  
**Release Version**:  v5.5 
 
**Description**:  
Several issues with emissions diagnostics were identified by internal developers and external users. These have been resolved. Issues include:

- Process analysis errors when PA_BLEV > 1 and emissions are restricted to layer 1 only.  This issue was first identified on the CMAS User Forum: https://forum.cmascenter.org/t/really-large-ipr-emis-results-for-upper-layers/
- Inconsistent time vector for B3GTS. On the CONUS domain, it was observed to equal 25 or 26 hours on random days. It should be 24 hours.
- Timing on lightning diagnostic files starting at 00000 instead of 10000.
- Formatting of DESID scale factors in log file has always been F6.3. Users have complained for some time. This is updated to ES9.2.
- The EMVAR molecular weight table defined in desid_vars.F is now assigned with individual operational lines instead of one continuous parameter statement in the module specification section. This update will avoid Fortran continuation line limit issues in the future if the number of emission species continues to expand.
- Adding space for environment variables like the symbolic date labels to be printed completely in the log files

**Significance and Impact**: These updates improve consistency among diagnostic output files and improve readability of the log files. 
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1077](https://github.com/USEPA/CMAQ/commit/1eef012a93faf0f7f9b523fede916fb5cd890fef) | [PR#1077](https://github.com/USEPA/CMAQ_Dev/pull/1077)  |  

## Add precision to timing metrics in logfiles
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Improvement (Minor log formatting change)  
**Release Version/Date**: v5.5  

**Description**: This PR adds three decimal places of precision to the process-level timing metrics in the ascii logfile.

**Significance and Impact**: At high computational efficiency, the default precision provided for the timing metrics in the logfile was yielding 0.0 for some processes. When aggregated, this underestimates the time taken by these processes.  
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#961](https://github.com/USEPA/CMAQ/commit/cf37d49e144b3aed1380c6a74f404063f5e047bf) | [PR#961](https://github.com/USEPA/CMAQ_Dev/pull/961)  | 

## Improvement of Logfile output and error reporting
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix and Log File Improvements  
**Release Version**: CMAQv5.4  

**Description**: 
- Propagated SHA ID from git repository to configuration file and execution ID to support versioning and matching code state to results.
- Propagated (mostly documentation) improvements to v5.4 branch from existing v5.3 release branch. 
- Added M3EXIT output to Main logfile to improve discoverability.
  
**Significance and Impact**: No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#639](https://github.com/USEPA/CMAQ/commit/3dc2bb6e3d3041bbbf0729643cc38bb2c52b2e11) | [PR#639](https://github.com/USEPA/CMAQ_Dev/pull/639)  | 
|[Merge for PR#637](https://github.com/USEPA/CMAQ/commit/6bf6a3c367cb5fae088396c879e1c9609766a5dd) | [PR#637](https://github.com/USEPA/CMAQ_Dev/pull/637)  | 
|[Merge for PR#769](https://github.com/USEPA/CMAQ/commit/c5bce3ef77dc54b29bf66046d07f766afc2d9f61) | [PR#769](https://github.com/USEPA/CMAQ_Dev/pull/769)  | 
