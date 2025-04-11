# SAPRC Chemical Mechanism

### Fix bug preventing CMAQ from running using SAPRC mechanisms
[Chris Nolte](mailto:nolte.chris@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix   
**Release Version/Date**:  CMAQv6.0 beta 1 and beta 2  

**Description**:   
The GC namelist for the SAPRC mechanisms specify that CLNO2 should be written to the DRYDEP outputs. However, this species has no deposition velocity surrogate, meaning that it does not undergo dry deposition. This inconsistency is fixed in CMAQv6.0 so that CLNO2 does undergo dry deposition.  

**Significance and Impact**:   
Allows the model to run. There is a very slight change to model results relative to a case where CLNO2 does not undergo dry deposition.    

**Internal PRs**: [PR#1197](https://github.com/USEPA/CMAQ_Dev/pull/1197)  

### Removal of saprc07tic_ae6i_aq and saprc07tic_ae6i_aqkmti mechanisms

[Golam Sarwar](sarwar.golam.email@epa.gov), U.S. Environmental Protection Agency 

**Type of update**: Model Clean-up

**Release Version/Date**: CMAQv5.4

**Description**:  The saprc07tic_ae6i_aq and saprc07tic_ae6i_aqkmti mechanisms are no longer maintained and have been removed from the CMAQ code repository beginning with version 5.4. These mechanisms can still be accessed and run through previous CMAQ versions.

