# Online Met-Dependent Emission (MetEmis) Module

### Implement Online Met-Dependent Emission Module (MetEmis)  
**Ben Murphy**, U.S. Environmental Protection Agency (Please direct questions to [CMAQ_Team@epa.gov](mailto:CMAQ_Team@epa.gov).)    
**Type of update**: Science Update   
**Release Version/Date**:  CMAQv6.0 

**Description**:   
The MetEmis module dynamically calculates meteorology-induced hourly gridded on-road mobile emissions within CMAQ, using simulated meteorology without any computational burden to the CMAQ modeling system.   

**Significance and Impact**:   
Mobile emissions from the on-road and off-network (e.g., vehicle start-up, running exhaust, brake–tire wear, hot soak, and extended idling) are sensitive to temperature and humidity due to various factors, including (1) cold engine starts that enhance emissions at lower ambient temperatures due to incomplete fuel combustion, (2) evaporative losses of volatile organic compounds (VOCs) due to expansion and contraction caused by ambient diurnal temperature variations, (3) enhanced running emissions at higher ambient temperatures, (4) atmospheric moisture suppression of high combustion temperatures that lower nitrogen oxide emissions at higher humidity, and (5) indirect increased emissions from air conditioning at higher ambient temperatures. McDonald et al. (2018) found that NOx emissions from the National Emissions Inventory (NEI) estimated from the U.S. EPA's MOVES are underestimated, leading to a failure regarding the prediction of high ozone days (8 h max ozone >70 ppb; McDonald et al., 2018).  

Overall, the CMAQ-MetEmis coupler allows us to dynamically simulate on-road vehicle emissions from the MOtor Vehicle Emission Simulator (MOVES) on-road emission model for CMAQ, with a better spatiotemporal representation based on the simulated meteorology inputs when compared to the static scenario. The domain total of daily volatile organic compound (VOC) emissions from the inline scenario shows that the largest impacts are from the local meteorology, which is approximately 10 % lower than the ones from the static scenario. In particular, the major difference in the VOC estimates was shown over the California region. These local meteorology impacts on the on-road vehicle emissions via CMAQ-MetEmis revealed an improvement in the hourly NO2, daily maximum ozone, and daily average PM2.5 patterns, with a higher agreement and correlation with daily ground observations.  

To use the MetEmis feature, a compatible emission file must be produced from SMOKE. This file must have emission rates tabulated as a function of ambient temperature, which is then interpolated by CMAQ. In the Run script example below, the file 'emis_mole_onroad.ncf' is an example of this file with MetEmis compability. See the SMOKE User Guide for instructions on how to output such a file.  

Run script configuration example: 

```
       setenv CTM_MET_EMIS Y              #> turns on met-dependent emissions
       setenv CTM_MET_EMIS_DIAG Y    #> Met-Dependent Emissions Diagnostic
       setenv CTM_MET_EMIS_HUM Y    #> turns on NOx humidity corrections
       #> Met-Dependent Emissions Table
       if ( $CTM_MET_EMIS == 'Y' ) then
           setenv METEMIS_TBL emis_mole_onroad.ncf
       endif

      setenv CTM_METEMIS_DIAG "$OUTDIR/CCTM_METEMIS_${CTM_APPL}.nc -v"   #> Met-Dependent Emissions Diagnostic
      set OUT_FILES = (${FLOOR_FILE} ${S_CGRID} ${CTM_CONC_1} ${A_CONC_1} ${MEDIA_CONC}         \
             ${CTM_DRY_DEP_1} $CTM_DEPV_DIAG $B3GTS_S $MEGAN_SOILOUT $BEIS_SOILOUT $BDSNPOUT \
             $CTM_WET_DEP_1 $CTM_WET_DEP_2 $CTM_ELMO_1 $CTM_AELMO_1             \
             $CTM_RJ_1 $CTM_RJ_2 $CTM_RJ_3 $CTM_SSEMIS_1 $CTM_DUST_EMIS_1 $CTM_IPR_1 $CTM_IPR_2 \
             $CTM_IPR_3 $CTM_BUDGET $CTM_IRR_1 $CTM_IRR_2 $CTM_IRR_3 $CTM_DRY_DEP_MOS           \
             $CTM_DEPV_MOS $CTM_VDIFF_DIAG $CTM_VSED_DIAG $CTM_LTNGDIAG_1 $CTM_LTNGDIAG_2       \
             **$CTM_METEMIS_DIAG** $CTM_VEXT_1 )

```

**References**:    
Baek, B. H., Coats, C., Ma, S., Wang, C.-T., Li, Y., Xing, J., Tong, D., Kim, S., and Woo, J.-H.: Dynamic Meteorology-induced Emissions Coupler (MetEmis) 
development in the Community Multiscale Air Quality (CMAQ): CMAQ-MetEmis, Geosci. Model Dev., 16, 4659–4676, https://doi.org/10.5194/gmd-16-4659-2023, 2023.  


|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#1094](https://github.com/USEPA/CMAQ/commit/27a2791509106ef4729637c30a56574ea2e4426b) | [PR#1094](https://github.com/USEPA/CMAQ_Dev/pull/1094)  |
