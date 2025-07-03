# Structural Improvements

### Reorganize Aero Module  
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Restructure  
**Release Version/Date**:  CMAQv6.0  

**Description**:   
This update restructures the code in the Aerosol module.    The routines in aero_subs file has been split up, grouping the routines in the orignal file into more intuitive places, making the code more digestible.

This update also renames SOA_DEFN to ORG_DEFN because it's been a long time since SOA_DEFN only treated SOA. It treats all organics. The name is updated both in the module and everywhere the module is used.

**Significance and Impact**:   
No impact on results.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1291](https://github.com/USEPA/CMAQ/commit/684b45ca253c04a854278ca929bb25968583fe3e) | [PR#1291](https://github.com/USEPA/CMAQ_Dev/pull/1291)  |   

### Enable parallel I/O for Lightning and ELMO files
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Fix/added feature  
**Release Version/Date**: CMAQv6.0

**Description**:   
When using a parallel file system (e.g., Lustre), a code block is needed to ensure that the file is open on all processors. Otherwise, the model crashes.
See this thread on the user forum. @dwongepa initially submitted PR #1350 to address this, but that was built on top of v5.5 code and it was easier to port the changes to this new PR.

**Significance and Impact**:   
Was not tested given no access to a parallel file system, however, not anticipated to change results

**References**:   
[CMAS Forum Post](https://forum.cmascenter.org/t/unable-to-write-to-aelmo-even-though-new-aelmo-file-successfully-created/5762/20)

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1352](https://github.com/USEPA/CMAQ/commit/68bb51c3e00840e5b7f93347d67c4f9c6d33eb1f) | [PR#1352](https://github.com/USEPA/CMAQ_Dev/pull/1352)  |   


### Improvements to compiling with GCC 
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Compilation  
**Release Version/Date**: CMAQv6.0

**Description**:   
Compiling the CCTM with gcc has always generated a daunting number of WARNING messages. There are so many that we have ignored them. This PR resolves many of those warnings, with the hope that developers or integrators might notice and address new ones that are created.

The types of warnings that have been addressed are:
1. no longer need "-std=legacy" workaround with GCC compiler.
2. unnecessary commas preceding I/O lists in WRITE and READ statements
3. unnecessary SAVE attributes in module variables (module variables are always saved, that's the point of putting them in a module) tab characters have been removed
4. The IOAPI function SETENVVAR returns a logical rather than an integer. Code has been modified to use the logical rather than do an implicit conversion and ignoring the result.
5. Parentheses are added when multiplying by a negative quantity, i.e., x * (-y)
6. Having a shared do loop continuation statement is apparently a deleted feature. I removed one instance of this in the photolysis module, but there are many instances of this construct in ISORROPIA that I left alone.
7. MEGAN include files have many species names longer than the declared length of 16. I deleted extra spaces to make them fit where possible, but left unchanged those where the string itself is longer than 16.

**Significance and Impact**:   
Beginning with version 10, the GCC compiler enforced stricter checking of the data types and ranks of arguments. Several CMAQ code files in the PARIO and STENEX modules had compilation errors involving their MPI routines. A workaround was added to the CCTM build so that the -std=legacy flag was included if the GCC compiler was being used; this turned these compilation errors into warnings. Upon investigation, it appears that the previous practice of using INCLUDE 'mpif.h' is now considered obsolete. Instead, one should use the MPI module. This allows the compiler to "see" the  proper interface blocks for all MPI routines, which can be used for several different data types and ranks (i.e., scalars and arrays). Otherwise the user needs to write their own interface  blocks, which is tricky and error-prone, or use the "legacy" flag. With the minor mods in this PR, the -std=legacy flag is no longer necessary, and no warnings are generated from these routines.
Additionally, I removed the -DSUBST_MPI=$(BASE_INC)/mpif.h from bldmake. That extra indirection was never a good idea, and we mostly stopped using it several years ago. (per #1)        

Generally, warnings should be addressed when possible. You never know when GCC might become even more militant and convert warnings to errors. (per #2-7)
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1357](https://github.com/USEPA/CMAQ/commit/bd7939748b87668227ebf0371c7e864a48182331) | [PR#1357](https://github.com/USEPA/CMAQ_Dev/pull/1357)  | 
|[Merge for PR#1358](https://github.com/USEPA/CMAQ/commit/ce939e79d4cda7961a5813578ee7de784a00e0b9) | [PR#1358](https://github.com/USEPA/CMAQ_Dev/pull/1358)  |  

### Replace CONST.EXT include file with module and update constant values  
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Restructure   
**Release Version/Date**: CMAQv6.0

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
|[Merge for PR#1137](https://github.com/USEPA/CMAQ/commit/e7ed66e185b1b93af8515428053465564ae6857c) | [PR#1137](https://github.com/USEPA/CMAQ_Dev/pull/1137)  | 
|[Merge for PR#1138](https://github.com/USEPA/CMAQ/commit/96449cd6f20eccf61699cee038317b6ffaed467a) | [PR#1138](https://github.com/USEPA/CMAQ_Dev/pull/1138)  |   


### Cap log_message at 1000 lines  
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency     
**Type of update**: Bug Fix 
**Release Version/Date**:  CMAQv6.0 

**Description**:   
When excessively long character strings are sent to log_message, it has the potential to reach an infinite loop. This update establishes a cap on the log_message at 1000 lines.

**Significance and Impact**:   
No impact on results.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1267](https://github.com/USEPA/CMAQ/commit/684b45ca253c04a854278ca929bb25968583fe3e) | [PR#1267](https://github.com/USEPA/CMAQ_Dev/pull/1267)  |   

### Correct desid_module.F for serial version of CCTM
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  
**Type of update**: Bug Fix  
**Release Version**:  CMAQv6.0  

**Description**:
The update removes a CCTM compile error from the desid_module.F file when the build script compiles a serial version. The error comes from lines getting a grid cell's column and row number in the simulation domain. The method only works for parallel version because it uses a PARIO routine. The fix inserts a ifdef-else block to consider parallel and non-parallel cases.

**Significance and Impact**: The update allows compiling a serial version of CCTM which can be useful in diagnosing CCTM bugs or errors.

|Merge Commit | Pull Request |
|:------:|:-------:|
| Merge for PR#1331 |  [PR#1331](https://github.com/USEPA/CMAQ_Dev/pull/1331) |


### Simplify RETRIEVE_OCEAN_DATA
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**:  Simplify code
**Release Version/Date**: CMAQv6.0

**Description**:   
Simplified the logic in CCTM/src/cio/centralized_io_module.F for subroutine retrieve_ocean_data.


**Significance and Impact**:   
No impact on model results.


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1365](https://github.com/USEPA/CMAQ/commit/f13dddf27a1920ddacd3c7a8cf918735184776df) | [PR#1365](https://github.com/USEPA/CMAQ/pull/1365)  |   


### GNU build flag update to enable compilation with GNU versions 10+
[Fahim Sidi](mailto:sidi.fahim@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix  
**Release Version/Date**:  CMAQv5.5   

**Description**:  Starting GNU version 10+, GNU no longer allows rank mismatches between the callee and the calling function. The exact verbiage from the GNU change logs:"Mismatches between actual and dummy argument lists in a single file are now rejected with an error. Use the new option -fallow-argument-mismatch to turn these errors into warnings; this option is implied with -std=legacy. -Wargument-mismatch has been removed.” (https://gcc.gnu.org/gcc-10/changes.html)

“GCC 10 now rejects argument mismatches occurring in the same source file. Those are not permitted by the Fortran standard and in general have the potential to generate invalid code. However, the Fortran standard does permit passing an array element or a scalar string (of default character kind or of c_char kind) as actual argument to an array dummy argument. (For the exact wording, see the Fortran standard on argument association; in particular, Fortran 2018, Sect. 15.5.2.4, Para. 4.)

Depending on their nature, argument mismatches have the potential to cause the generation of invalid code and, hence, should be investigated. The most common reason that code fails due to newly enforced check is the following: instead of using an array element as actual argument, a scalar is used; one solution is to replace the scalar by a size-one array. (This should be passed as a whole as there is no point in passing it as array element.) Additionally, check that the code indeed only accesses this single element. — Other mismatches occur more rarely but usually indicate more serious bugs where a wrong result is likely (at least for some target-platform and optimization combination).”

The non-FORTRAN explanation boils down to the ability to pass 1-D arrays, 2-D arrays, 3-D arrays, etc., into a subroutine or function and have the called routine set up so that that it "does the right thing". This is in fact a common occurrence, where the callee "single-indexes" multi-dimensional arrays.

|Merge Commit | Internal record|
|:------:|:-------:|
| [Merge for PR#1154](https://github.com/USEPA/CMAQ/commit/c31983b72a3049d708138da3f57227875333eb39) |  [PR#1154](https://github.com/USEPA/CMAQ_Dev/pull/1154) |

### Emissions Diagnostics and Log Output
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Diagnostic and Log Updates  
**Release Version**:  CMAQv5.5 
 
**Description**:   
Several issues with emissions diagnostics were identified by internal developers and external users. These have been resolved. Issues include:

- Process analysis errors when PA_BLEV > 1 and emissions are restricted to layer 1 only.  This issue was first identified on the CMAS User Forum: https://forum.cmascenter.org/t/really-large-ipr-emis-results-for-upper-layers/
- Inconsistent time vector for B3GTS. On the CONUS domain, it was observed to equal 25 or 26 hours on random days. It should be 24 hours.
- Timing on lightning diagnostic files starting at 00000 instead of 10000.
- Formatting of DESID scale factors in log file has always been F6.3. Users have complained for some time. This is updated to ES9.2.
- The EMVAR molecular weight table defined in desid_vars.F is now assigned with individual operational lines instead of one continuous parameter statement in the module specification section. This update will avoid Fortran continuation line limit issues in the future if the number of emission species continues to expand.
- Adding space for environment variables like the symbolic date labels to be printed completely in the log files

**Significance and Impact**:  
These updates improve consistency among diagnostic output files and improve readability of the log files. 
 
|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1077](https://github.com/USEPA/CMAQ/commit/1eef012a93faf0f7f9b523fede916fb5cd890fef) | [PR#1077](https://github.com/USEPA/CMAQ_Dev/pull/1077)  |  

## Add precision to timing metrics in logfiles 
[Ben Murphy](mailto:murphy.ben@epa.gov), U.S. Environmental Protection Agency   
**Type of update**: Improvement (Minor log formatting change)   
**Release Version/Date**: CMAQv5.5    

**Description**:   
This PR adds three decimal places of precision to the process-level timing metrics in the ascii logfile.

**Significance and Impact**:   
At high computational efficiency, the default precision provided for the timing metrics in the logfile was yielding 0.0 for some processes. When aggregated, this underestimates the time taken by these processes.  
 
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
  
**Significance and Impact**:   
No impact on results for the cases tested.  

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#639](https://github.com/USEPA/CMAQ/commit/3dc2bb6e3d3041bbbf0729643cc38bb2c52b2e11) | [PR#639](https://github.com/USEPA/CMAQ_Dev/pull/639)  | 
|[Merge for PR#637](https://github.com/USEPA/CMAQ/commit/6bf6a3c367cb5fae088396c879e1c9609766a5dd) | [PR#637](https://github.com/USEPA/CMAQ_Dev/pull/637)  | 
|[Merge for PR#769](https://github.com/USEPA/CMAQ/commit/c5bce3ef77dc54b29bf66046d07f766afc2d9f61) | [PR#769](https://github.com/USEPA/CMAQ_Dev/pull/769)  | 
