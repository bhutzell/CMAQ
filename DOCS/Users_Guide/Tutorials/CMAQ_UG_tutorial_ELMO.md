## CMAQ ELMO Tutorial ##
### Adding Keywords and Derived Variables ###
Purpose: This tutorial will step you through the processes of adding ELMO Keywords to the CMAQ Control Namelist and 
adding new derived variables to the ELMO source code.  

------------

### Add ELMO Keywords

#### STEP 1: Make Space for Your New Keyword

Go to the CMAQ Control Namelist ([CMAQ_Control.nml][link_cmaq_ctrl]) under the section &ELMO_INIT and edit the value of N_Keywords.  

```
&ELMO_INIT
  N_Files = 2               
  N_Max_Output_Variables = 400
  N_Keywords = 74                    ! change from 73 to 74
  N_Max_Keyword_Variables = 150
/
```


#### STEP 2: Add a Keyword Name  

Go to the CMAQ Control Namelist ([CMAQ_Control.nml][link_cmaq_ctrl]) under the section "DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS". 
Create a new line and set the value of Keywd_name(x) equal to the name of your new Keyword. The x is an arbitrary 
index indicating the position of your Keyword in the full Keyword list. There is nothing consequential about the order 
of ELMO Keywords, but indices cannot repeat. If you choose a number in the middle of the existing list, you must increment 
the index of every Keyword after it in the list. The easiest choice is just to make x equal to 1 greater than the 
current largest Keyword index. No keyword index should exceed the value of N_Keywords. 

Example:  
```

 !-----------------------------------------------------!
 !----- DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS -----!
 !-----------------------------------------------------!

  Keywd_name(74) = 'NEW_KEYWORD'
```


#### STEP 3: Specify your Keyword's Components

Populate Keywd(x,:) with a list of comma-separated, quoted strings. These strings can be CMAQ species, ELMO derived 
variables, meteorological variables, other Keywords, or any other variable type ELMO knows of. You do not need to 
specify the length of the component list of your new keyword.  

Example:
```

 !-----------------------------------------------------!
 !----- DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS -----!
 !-----------------------------------------------------!

  Keywd_name(74) = 'NEW_KEYWORD'
  Keywd(74,:) = 'SO2','ASO4','PMF_SO4','PMC_SO4','TA','RH'
```


#### STEP 4: Use Your New Keyword

Your Keyword can now be added to one or more of the existing output files specified in ELMO.

Example:  
```
&ELMO_Files

 !
 !-- Set Properties and Contents for each ELMO Output File
 !

  Flabel(1)= 'DEFAULT'
  Tmode(1) = 'aggregate'
  Lay_Bot(1) = 1
  Lay_Top(1) = 1
  File_Vars(1,:) = 'DEFAULT','NEW_KEYWORD'

  Flabel(2)= 'DEFAULT_DEP'
  Tmode(2) = 'aggregate'
  Lay_Bot(2) = 1
  Lay_Top(2) = 1
  File_Vars(2,:) = 'DEFAULT_DEP'  
```

Or you may create a new output file and use the keyword there.  

```
&ELMO_INIT
  N_Files = 3                        ! change from 2 to 3 output files
  N_Max_Output_Variables = 400
  N_Keywords = 74                    
  N_Max_Keyword_Variables = 150
/

&ELMO_Files

 !
 !-- Set Properties and Contents for each ELMO Output File
 !

  Flabel(1)= 'DEFAULT'
  Tmode(1) = 'aggregate'
  Lay_Bot(1) = 1
  Lay_Top(1) = 1
  File_Vars(1,:) = 'DEFAULT'

  Flabel(2)= 'DEFAULT_DEP'
  Tmode(2) = 'aggregate'
  Lay_Bot(2) = 1
  Lay_Top(2) = 1
  File_Vars(2,:) = 'DEFAULT_DEP'

  Flabel(3)= 'NEW_FILE'
  Tmode(3) = 'aggregate'
  Lay_Bot(3) = 1
  Lay_Top(3) = 1
  File_Vars(3,:) = 'NEW_KEYWORD'

```

------------

### Add Derived Variables to ELMO Source Code

#### STEP 1: Add an Index for the New Variable

Go to [ELMO_DATA.F][link_elmo_data]. Beginning around line 70, you will find a list of 100+ integers, prefixed with ID_, which map to 
derived ELMO variables and other diagnostics. Add an index for your new variable to the end of the list. We recommend 
prefixing it with ID_.

Example:

around line 191 of ELMO_DATA.F, add  
```
      INTEGER, PARAMETER :: ID_NEW_VAR =    115
```  
and modify the following:  
```
      INTEGER, PARAMETER :: N_ELMO_LIST = 115     ! (change 114 to 115)
```

#### STEP 2: Add New Variable Attributes to ELMO_LIST  

Below the list of ID_ indices is a table called ELMO_LIST which holds the attributes for every ELMO derived variable. 
Because each variable maps to an ID_, this list is order-independent. Add a row in any location you prefer. This row 
should contain values for:
- The name of the variable - a character string that will be used in File_Vars to add the variable to an output file. 
This name can also be used in the contents of any Keyword to activate it with a group of other variables.  
- The ID_ of the variable.
- The variable type: 
    - ET_DRVD - a derived ELMO variable. These should be mass concentrations or mixing ratios. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_DRVD_DDEP - a derived ELMO variable for dry deposition fluxes. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_DRVD_WDEP - a derived ELMO variable for wet deposition fluxes. These can presumably be 
    refined in source-oriented applications like ISAM and DDM.  
    - ET_AEROPROP - an aerosol property. These variables describe the aerosol size distribution (e.g. number concentration, 
    diameter, density, etc.) or some chemical property (e.g. O:C, pH).  
    - ET_PHOT - an optical property. This could be AOD, NO2 column, etc.
    - ET_MET - a meteorological variable (e.g. temperature, rain data, etc.)
    - ET_CHEM - a chemical reaction property for heterogeneous reactions like gamma uptake coefficient.

Example:

At the end of the list, around line 337  
```
     &ELDTP( 'SOILT_2      ',ID_SOILT_2, ET_MET,  F, 'K   ', 'Layer 2 soil temperature' ),   ! modify the last line by removing the /) and adding a comma

     &ELDTP( 'NEW_VAR      ',ID_NEW_VAR, ET_DRVD, T, 'ppmV', 'New Variable            ' ) /) ! add your new ELMO derived variable
```

#### STEP 3: Declare Array for Propagating Data to New Variable

Declare an allocatable array in the ELMO_DATA module that will store the values for your new variable so they may be accessed by ELMO 
when it compiles output. Use ELMO_AOD_550 as an example. This particular variable is defined with two dimensions. Use three dimensions 
if your variable is dependent on height.  

Example:  

In ELMO_DATA (lines 630-640) we make ELMO_AOD_550 and other variables available to other CMAQ modules.    
```
        REAL, ALLOCATABLE, SAVE, PUBLIC ::                                                                                        
     &                             ELMO_NEW_VAR(:,:,:), ! new variable
     &                             ELMO_AOD_550(:,:),  ! total aerosol optical depth at                                           
                                                       !   550 nm (Angstrom Interpolation)                                        
     &                             ELMO_EXT_550(:,:,:),! aerosol extinction at 550 nm for layer [1/m]                             
     &                             ELMO_NO2COL(:,:),   ! NO2 Column                                                                 
     &                             ELMO_SO2COL(:,:),   ! SO2 Column                                                                 
     &                             ELMO_HCHOCOL(:,:),  ! Formaldehyde Column                                                        
     &                             ELMO_COCOL(:,:),    ! CO Column                                                                  
     &                             ELMO_VOC_NOX(:,:,:) ! VOC or NOx limited ozone formation     
```
 
#### STEP 4: Allocate and Initialize New Variable

Add your variable to the subroutine elmo_init_shared. Again, you may use ELMO_AOD_550 as an example.  

Example:  
After about line 740  (be sure to keep outside of the ifdef statements, but before the endif of the firsttime)  
```
            allocate( elmo_new_var(ncols, nrows, nlays ), stat=ios)
            call checkmem( ios, 'ELMO_NEW_VAR',pname )
            elmo_new_var = 0.0
```


#### STEP 5: Populate New Variable in any CMAQ process  

Use the new array defined and allocated in Steps 3 & 4 in a CMAQ module to store the data you wish to output. The ELMO_DATA module should 
be used in the subroutine you modified if it is not already. We recommend using the 'Use ELMO_DATA, Only:' approach to protect the rest 
of the ELMO_DATA module and only update your own variable.  

Example:  
```
      USE ELMO_DATA, ONLY : ELMO_NEW_VAR
```
Then use ELMO_NEW_VAR to store some data of interest. 
```
      ELMO_NEW_VAR( row,col,lay ) = [local variable](row, col, lay)
```

As an example, in PHOT.F, ELMO_AOD_550 is populated with the aerosol optical depth from the phot module.  
```
      USE ELMO_DATA, ONLY : ELMO_AOD_550

...
      
! Store PM Diagnostic AOD and extinction
      ELMO_AOD_550 = TAU_AERO_550
      ELMO_EXT_550 = AERO_EXT_550
```
 
#### STEP 6: Propagate Data to ELMO Output Arrays

In ELMO_DERIVED_CALC.F, add a case to the select case statement for the variable IDG. Your case should reference the new ID_ index of
your variable. Within the case, set outval equal to the value of your new variable in the current local grid cell (C1,R1,L1), and
make any appropriate modifications. Again, use the approach for ELMO_AOD_550 as a guide.

```
         ! Retrieve AOD at 550 nm
         CASE ( ID_AOD550 )
             IF ( L1 .EQ. 1 ) THEN
                 OUTVAL = ELMO_AOD_550( C1,R1 )
             ELSE
                 OUTVAL = ELMO_BLANK
             END IF
```

Example: (note this variable has data for column, row, and layers, so differs slightly from ELMO_AOD_550 which only has columns and rows).
Add the following around line 391  
```
         ! Retrieve NEW_VAR
         CASE ( ID_NEW_VAR )
         OUTVAL = ELMO_NEW_VAR( C1,R1,L1 )
```

#### STEP 7: Add Variable Name to CMAQ Control File

ELMO is now equipped to output your variable. You may add it to File_Vars in [CMAQ_Control.nml][link_cmaq_ctrl] for any custom output file you like, 
you may add it to the contents of any existing ELMO Keyword, or you may create a new ELMO Keyword and ELMO_NEW_VAR to its contents. For the last 
option, follow the guidance given above in the section titled "Add ELMO Keywords".

<!-- START_OF_COMMENT -->

[link_elmo_data]: ../../../CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: ../../../CCTM/src/driver/CMAQ_Control.F

<!-- END_OF_COMMENT -->

[link_elmo_data]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/CMAQ_Control.F



