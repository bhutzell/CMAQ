
### Correct loss of reactive tracer styrene from ozone reaction
[William T. Hutzell](mailto:hutzell.billl@epa.gov), U.S. Environmental Protection Agency    
**Type of update**: Bug Fix
**Release Version/Date**:  CMAQv6.0 
**Description**:  The CCTM reactive tracers module (CCTM/src/reactive_tracers) includes a tracer for styrene. The reviewing DEGRADE_PARAMETERS.F file determined that the loss process for a ozone reaction is wrong. Its rate constant is approximately four orders of magnitude too high and is actually the rate constant for a reaction between styrene and nitrate. The error causes predicted concentrations of styrene to be low over four orders of magnitude. 

The fix replaces bad rate constant with the Le Person et al. (2007) value obtain from the [NIST Chemical Kinetics Database](https://kinetics.nist.gov/kinetics/index.jsp).  For OH, NO<sub>3</sub> and Cl reactions between styrene, the rate constants were obtain from the same database and are Joeson et al. (2014), Atkinson (1991), and Shi (1997), respectively. 

**Significance and Impact**: The error causes a severe underestimate of exposure and human health risks from styrene emissions when using CMAQ for its air toxics assessments such as in [EPA AirToxScreen](https://www.epa.gov/AirToxScreen).

For the STYRENE model species as in the cb6r5hap_ae7_aq mechanism, predicted concentrations are low by several orders of magnitude. The below plot illustrate the error by show the mean styrene concentration after one 24 hour simulation over the 12NE3 2018 Benchmark Domain.

#####   Uncorrected Model.   
![Jul_01_2018_DegError_vs_DegError_STYRENE_Jul_01_2018_Layer1_TilePlot](https://github.com/user-attachments/assets/8d44a904-535d-41db-b599-47d61aca91a0)
#####   Corrected Module.   
![Jul_01_2018_DegFix_vs_DegFix_STYRENE_Jul_01_2018_Layer1_TilePlot](https://github.com/user-attachments/assets/b949ec25-a962-4218-bb32-588acfc1f806)

The error and its removal do not affect predictions of other model species.

**References**:   
1. Joeson Cho, Masoud Roueintan, and Zhuangjie Li, Kinetic and Dynamic Investigations of OH Reaction with Styrene, The Journal of Physical Chemistry A 2014 118 (40), 9460-9470, DOI: 10.1021/jp501380j
2. A. Le Person, G. Eyglunent, V. Daële, A. Mellouki, Y. Mu, The near UV absorption cross-sections and the rate coefficients for the ozonolysis of a series of styrene-like compounds, Journal of Photochemistry and Photobiology A: Chemistry, Volume 195, Issue 1,2008, Pages 54-63, ISSN 1010-6030, https://doi.org/10.1016/j.jphotochem.2007.09.006
3. Atkinson, Roger, Kinetics and Mechanisms of the Gas-Phase Reactions of the NO3 Radical with Organic Compounds               Journal of Physical and Chemical Reference Data 20, 459 (1991); https://doi.org/10.1063/1.555887
4. Shi, J. and Bernhard, M.J. (1997), Kinetic studies of Cl-atom reactions with selected aromatic compounds using the photochemical reactor-FTIR spectroscopy technique. Int. J. Chem. Kinet., 29: 349-358. https://doi.org/10.1002/(SICI)1097-4601(1997)29:5%3C349::AID-KIN5%3E3.0.CO;2-U

**Internal PRs**: [PR#1135](https://github.com/USEPA/CMAQ_Dev/pull/1135)  


### Correction to Species Tables for cb6r5hap_ae7_aq
**Type of update**: Documentation update  
**Release Version/Date**: CMAQ version 5.5   
**Description** [See release note under Carbon Bond 6 Mechanism](./CMAQ-Release-Notes:-Chemistry:-Carbon-Bond-6-Mechanism-(CB6).md#correction-to-molecular-weight-of-hgiigas-in-species-tables)   


### Multi-Pollutant version of the cb6r5hap_ae7_aq mechanism
[William T. Hutzell](mailto:hutzell.bill@epa.gov), U.S. Environmental Protection Agency  

**Type of update**: Science Update and New Feature  

**Release Version/Date**: CMAQ version 5.4   

**Description**  The Multi-pollutant version of the cbr6 mechanism replaces the cb6mp_ae6_aq mechanism in version 5.3.3. The new mechanism is based on  cb6r5_ae7_aq and is called cb6r5hap_ae7_aq. It includes the same toxic gas and aerosol species as the cb6mp_ae7_aq while adds (1) gas and aerosol reactive tracers for benzo[a]pyrene and (2) nonreactive tracers for PAHs lumped based on their Toxic Equivalency Factors. 

**Significance and Impact**: Users can selectively model a wide range of Hazardous Air Pollutants based on the r5 version of the carbon bond 6 mechanisms in CMAQ 5.4.

|Merge Commit | Internal record|
|:------:|:-------:|
|[Merge for PR#887](https://github.com/USEPA/CMAQ/commit/2a64783ac1d0f6dc8ad708920f112c6f2035f8bc) | [PR#887](https://github.com/USEPA/CMAQ_Dev/pull/887)  |
|[Merge for PR#731](https://github.com/USEPA/CMAQ/commit/deb5b42cdd0041549a4aa5b8e6d069f2b75c203d) | [PR#731](https://github.com/USEPA/CMAQ_Dev/pull/731)  |
|[Merge for PR#722](https://github.com/USEPA/CMAQ/commit/fefb235c2c3808284a9364b59c2f7d6ef659b5f8) | [PR#722](https://github.com/USEPA/CMAQ_Dev/pull/722)  |
|[Merge for PR#684](https://github.com/USEPA/CMAQ/commit/2c24787922277311d113815f9073296a4623bc77) | [PR#684](https://github.com/USEPA/CMAQ_Dev/pull/684)  |
