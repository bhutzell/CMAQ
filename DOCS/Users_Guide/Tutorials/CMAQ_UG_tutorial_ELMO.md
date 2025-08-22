## CMAQ ELMO Add New Output Variable Tutorial ##
### Add ELMO Keywords and Derived Variables ###
Purpose: This tutorial will step you through the processes of adding ELMO Keywords to the CMAQ Control Namelist and 
adding new derived variables to the ELMO source code.  

------------

### Add ELMO Keywords

#### STEP 1: Decide to add a new keyword and a new ELMO output file that contains the keyword variable.

Go to the CMAQ Control Namelist ([CMAQ_Control.nml][link_cmaq_ctrl]) under the section &ELMO_INIT and edit the N_Files and N_Keywords

```
&ELMO_INIT
  N_Files = 3                        ! change from 2 to 3 output files
  N_Max_Output_Variables = 400
  N_Keywords = 74                    ! change from 73 to 74
  N_Max_Keyword_Variables = 150
/
```


#### STEP 2: Add your new Keyword Name

Go to the CMAQ Control Namelist ([CMAQ_Control.nml][link_cmaq_ctrl]) under the section "DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS". 
Create a new line and set the value of Keywd_name(x) equal to the name of your new Keyword. The x is an arbitrary 
index indicating the position of your Keyword in the full Keyword list. There is nothing consequential about the order 
of ELMO Keywords, but indices cannot repeat. If you choose a number in the middle of the existing list, you must increment 
the index of every Keyword after it in the list. The easiest choice is just to make x equal to 1 greater than the 
current largest Keyword index. 



Example:

```

 !-----------------------------------------------------!
 !----- DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS -----!
 !-----------------------------------------------------!

  Keywd_name(74) = 'NEW_VAR'
```


#### STEP 3: Specify your keyword components


Populate Keywd(x,:) with a list of comma-separated, quoted strings. These strings can be CMAQ species, ELMO derived 
variables, meteorological variables, other Keywords, or any other variable type ELMO knows of.

Example: 

```
 !-----------------------------------------------------!
 !----- DEFINE ELMO KEYWORDS FOR USE IN FILE_VARS -----!
 !-----------------------------------------------------!

  Keywd(74,:) = 'NEW_VAR'  ! Step 2  Specify components of NEW_VAR ??

```


#### STEP 4: Use Your New Keyword

Your Keyword is ready for use in File_Vars to activate variables for your simulation's output files.

Add an additional output ELMO output file and write your Keyword. 

??Is this correct, or do you add your new keyword to another output file?

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
  File_Vars(1,:) = 'DEFAULT'

  Flabel(2)= 'DEFAULT_DEP'
  Tmode(2) = 'aggregate'
  Lay_Bot(2) = 1
  Lay_Top(2) = 1
  File_Vars(2,:) = 'DEFAULT_DEP'

  Flabel(3)= 'NEW_VAR'
  Tmode(3) = 'aggregate'
  Lay_Bot(3) = 1
  Lay_Top(3) = 1
  File_Vars(3,:) = 'NEW_VAR'

------------

```

### Add Derived Variables to ELMO Source Code

#### STEP 1: Add an Index for the New Variable (ELMO_DATA.F and ELMO_DERIVED_CALC.F) and increment the number in the list

Go to [ELMO_DATA.F][link_elmo_data]. Beginning around line 70, you will find a list of 100+ integers, prefixed with ID_, which map to 
derived ELMO variables and other diagnostics. Add an index for your new variable to the end of the list. We recommend 
prefixing it with ID_.

Example:

around line 191 add

```
      INTEGER, PARAMETER :: ID_NEW_VAR =    115
```

and modify the following:

```
      INTEGER, PARAMETER :: N_ELMO_LIST = 115     ! (change 114 to 115)
```

#### STEP 2: Add your ELMO derived variable to the ELMO_LIST

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
     &ELDTP( 'SOILT_2      ',ID_SOILT_2,    ET_MET,      F, 'K     ', 'Layer 2 soil temperature' ),  ! modify the last line by removing the /) and adding a comma

     &ELDTP( 'NEW_VAR          ',ID_NEW_VAR,        ET_DRVD,     T, 'ppmV', ' New Variable') /)          ! add your new ELMO derived variable
```




#### STEP 3: Declare Array for New Variable

Declare an allocatable array in the ELMO_DATA module that will store the values for your new variable so they may be accessed by ELMO. 
Use ELMO_AOD_550 as an example. This particular variable is defined with two dimensions. Use three dimensions if your variable is 
dependent on height.  

Example:

After about line 650 add

```
       REAL, ALLOCATABLE, SAVE :: ELMO_NEW_VAR(:,:,:) ! NEW_VAR accumulated per timestep
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

#### STEP 5: Populate New Variable in CMAQ in the ELMO_DERIVED_CALC.F

Use the new array defined and allocated in Steps 3/4 in a CMAQ module to store the data you wish to output. The ELMO_DATA module should 
be used in the subroutine you modify if it is not already. We recommend using the 'Use ELMO_DATA, Only:' approach to protect the rest 
of the ELMO_DATA module and only update your own variable.  

Example:

```
            USE ELMO_DATA, ONLY : ELMO_NEW_VAR
```

#### STEP 6: Define NEW_VAR in ELMO_DATA.F

In ELMO_DATA (lines 630-645) we define elmo_aod_550 and other variables.

```
PUBLIC ::                                                                                        
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

#### STEP 7: Allocate NEW_VAR in ELMO_DATA.F

In ELMO_DATA (lines 681-683) we allocate and initialize elmo_aod_550.

```
                                   allocate( elmo_aod_550(ncols, nrows ), stat=ios)
                                   call checkmem( ios, 'ELMO_AOD_550',pname )
                                   elmo_aod_550 = 0.0
```

Do the same for a NEW_VAR

``` 
                                   allocate( elmo_new_var(ncols, nrows, ncols ), stat=ios)
                                   call checkmem( ios, 'ELMO_NEW_VAR',pname )
                                   elmo_new_var = 0.0
```



#### STEP 8: Populate NEW_VAR from variable in the phot module.

In PHOT.F populate elmo_aod_550 with the aerosol optical depth from the phot module.

```
! Store PM Diagnostic AOD and extinction
      ELMO_AOD_550 = TAU_AERO_550
      ELMO_EXT_550 = AERO_EXT_550
```

#### STEP 9: Propagate Data to ELMO Output Arrays

In ELMO_DERIVED_CALC.F, add a case to the select case statement for the variable IDG. Your case should reference the new ID_ index of
your variable. Within the case, set outval equal to the value of your new variable in the current local grid cell (C1,R1,L1), and
make any appropriate modifications. Again, use the approach for ELMO_AOD_550 as a guide.

In ELMO_DERIVED_CALC (lines 391-397) we select the value in elmo_aod_550 for the output value on the ELMO file.

```
         ! Retrieve AOD at 550 nm
         CASE ( ID_AOD550 )
             IF ( L1 .EQ. 1 ) THEN
                 OUTVAL = ELMO_AOD_550( C1,R1 )
             ELSE
                 OUTVAL = ELMO_BLANK
             END IF
```

Example: (note this variable has data for column, row, and layers, so differs slightly from ELMO_AOD_550 which only has columns and rows)

add the following around line 391

```
         ! Retrieve NEW_VAR
         CASE ( ID_NEW_VAR )
         OUTVAL = ELMO_NEW_VAR( C1,R1,L1 )
```


#### STEP 10: Add Variable Name to CMAQ Control File

ELMO is now equipped to output your variable. You may add it to File_Vars in [CMAQ_Control.nml][link_cmaq_ctrl] for any custom output file you like, or 
you may add it to the contents of any ELMO Keyword.  

Note: this was done above in the steps followed for ADD ELMO KEYWORDS at the beginning of this tutorial.



<!-- START_OF_COMMENT -->

[link_elmo_data]: ../../../CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: ../../../CCTM/src/driver/CMAQ_Control.F

<!-- END_OF_COMMENT -->

[link_elmo_data]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/ELMO_DATA.F
[link_cmaq_ctrl]: https://github.com/USEPA/CMAQ/blob/main/CCTM/src/driver/CMAQ_Control.F


