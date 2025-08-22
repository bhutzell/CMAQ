# Transport Processes

## Restore Vertical Diffusion and Gravitational Settling Diagnostic Files
**Chris Nolte**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)   
**Type of update**: Bug Fix  
**Release Version/Date**: CMAQv5.5  
**Description**: The environment variable `VDIFF_DIAG_FILE` is supposed to control whether the CCTM outputs the vertical diffusion diagnostic file `CTM_VDIFF_DIAG`, and if gravitational settling is being used, additionally the `CTM_VSED_DIAG` file. However, there was a bug that prevented these diagnostic files from being written even if the user requested these outputs in the run script.   
**Significance and Impact**: No change to model calculation. Allows for outputting vertical diffusion and gravitational settling diagnostic files.    

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1016](https://github.com/USEPA/CMAQ/commit/eddb4a71ce8e46b509d973462696c9fd8cea7ede) | [PR#1016](https://github.com/USEPA/CMAQ_Dev/pull/1016)  |