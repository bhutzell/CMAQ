#! /bin/csh -f

# ==================== Build Script for BLDMAKE ===================== #
# Usage: bldit_bldmake.csh                                            #
# Requirements: I/O API & netCDF libraries; a Fortran compiler        #
#                                                                     #
# To report problems or request help with this script/program:        #
#             http://www.cmascenter.org                               #
# =================================================================== #

#> Test whether BLDER are set, If not set it.
  if ( ! $?BLDER ) then
     if ( ! $?compiler ) then
#> Set Compiler Identity by User Input: Options -> intel | pgi | gcc
        if ( $#argv == 1 ) then
           setenv compiler $argv[1]
        else if ( $#argv == 2 ) then
           #> Compiler Name and Version have been provided
           setenv compiler $1
           setenv compilerVrsn $2
        else
           echo "usage: $0 <compiler>"
           echo " where <compiler> is intel, pgi or gcc"
           exit(2)
        endif
     endif
     if ( ! $?compilerVrsn ) setenv compilerVrsn " "
     cd ../../..
     source ./config_cmaq.csh $compiler $compilerVrsn
     setenv BLDER ${CMAQ_HOME}/UTIL/bldmake/bldmake_${compilerString}.exe
  endif
  
#> Recompile BLDMAKE from source if requested or if it does not exist
  if ( $?CompileBLDMAKE || ! -f $BLDER ) then

     if (${compiler} == pgi) then
        # this addition flag is for handling backslash in quoted string properly
        set add_flags = -Mbackslash
     else
        set add_flags = 
     endif

     #> Set BLDER to Default Path
     set BLDEXE = "bldmake_${compilerString}.exe"
     set BLDDIR = "$CMAQ_HOME/UTIL/bldmake"
     setenv BLDER "${BLDDIR}/${BLDEXE}"
     
     #> Make bldmake directory if it does not exist
     if ( ! -d $BLDDIR ) mkdir -pv $BLDDIR
  
     #> Compile BLDMAKE source code
     set BLDSRCDIR = "$CMAQ_REPO/UTIL/bldmake/src"
     set flist = (\
          cfg_module\
          bldmake\
          parser\
          utils )
  
     #> Clean Destination BLDMAKE directory
     cd $BLDDIR
     rm *.o *.mod $BLDER
  
  switch ( $compiler ) #> path of Fortan and C compilers; instead of values set in config.cmaq
   case  "intel":
      set FC = "ifort"
      set CC = "icx"
      breaksw
   case "gcc":
      set FC = "gfortran"
      set CC = "gcc"
      breaksw
   case "pgi":
      set FC = "pgf90"
      set CC = "pgcc"
      breaksw
   default:
      echo "Unknown compiler ${compiler}:"
      echo "Unable to create bldmake executable"
      exit()
      breaksw
  endsw

     #> Create Object Files
     cd $BLDSRCDIR
     foreach file ( $flist )
        $FC -c $add_flags $myFFLAGS $file.f -o $BLDDIR/$file.o
     end
  
     #> Compile BLDMAKE
     cd $BLDDIR
     $FC *.o -o $BLDEXE
     if( ! -e $BLDEXE ) then
         echo " "; echo " ***ERROR*** BLDMAKE Compile failed"; echo " "
         exit 1
     endif
     chmod 755 $BLDEXE
     echo " "; echo " Finish building $BLDEXE "
  
  endif
 
 exit
