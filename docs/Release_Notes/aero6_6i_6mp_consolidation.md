# Consolidation of aero6, aero6i, aero6_mp aero modules

**Author/P.O.C.:**, [Havala Pye](mailto:pye.havala@epa.gov), Computational Exposure Division, U.S. EPA

## Brief Description

In CMAQv5.1, three options existed within the aero module: aero6, aero6i, and aero6_mp. Each option consisted of a directory of code within aero that would be selected when the model was built. Since much of the code was already shared among the modules, the remaining code was merged together so that separate code directories are not needed in the aero module for each aerosol option.
The functionality of all three options (6, 6i, and 6mp) remain using the shared code directory.

## Significance and Impact

No change in model predictions. Reduces the amount of code in the CCTM.


## Affected Files

aero/aero6i and aero/aero6_mp directories eliminated. All options now exist in main aero/aero6 folder.

cloud/acm_ae6/AQ_DATA.F  
cloud/acm_ae6/aq_map.F  
cloud/acm_ae6/aqchem.F  
cloud/acm_ae6/rescld.F  
cloud/acm_ae6_mp/AQ_DATA.F  
cloud/acm_ae6_mp/aqchem.F  


## References

NA

-----
## Internal Records:
#### Relevant Pull Requests:
[PR #84](https://github.com/USEPA/CMAQ_Dev/pull/84)  
[PR #78](https://github.com/USEPA/CMAQ_Dev/pull/78)  
[PR #75](https://github.com/USEPA/CMAQ_Dev/pull/75)  

#### Commit IDs:
e3b710b744edf6fab4f1d5c6ba3cfe18eca8ef47  
37eb67f12a8aae5eda1e86aa9bbbd243b8a6d8a4  
b16d20a28f6e8f0803e61e5edce07903be2c5915  
36138d436d8f51db55b83d7a3f6528a166d39455  
1dfd9e3b10034dde2f43add22aed1d73cb128711  
1f3854dfe2560d033f4bdc43457da3a31d3df39d  
60099f8a4014cd889b6f2af9ca2e97283895dafc  
9b0cfc09a92e8fafca4dfd0cc1fbd0b0a5e90f29  
618e39f570aacb8ed974d9efd9ada623c8b767cb  
9e7ee40c9e398cf99d08ad3073a90fa4bcbfa832  
f386fb9134a159e7a784e9774c38bc92b7c19cc3  
3a2ee84d5ce2e04f0f424296d6695ed78619e47d  
c84c4042a2347a9569ba6f7a4891edb39af0b7c6  
6be3f17fa52708100d03ee65f3e6897838dd2e09  
bb91c5cc99f02c5117d90a86ddbf93fd78c604a9  
7acc41f6e6d96d49ede0d8bd4aaaaaa8c6ef85f0  
86e391e47dc1954bab5e270c0d475967734d8571  


-----
