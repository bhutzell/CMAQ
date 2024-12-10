/*************************************************************************
*  The Community Multiscale Air Quality (CMAQ) system software is in     *
*  continuous development by various groups and is based on information  *
*  from these groups: Federal Government employees, contractors working  *
*  within a United States Government contract, and non-Federal sources   *
*  including research institutions.  These groups give the Government    *
*  permission to use, prepare derivative works of, and distribute copies *
*  of their work in the CMAQ system to the public and to permit others   *
*  to do so.  The United States Environmental Protection Agency          *
*  therefore grants similar permission to use the CMAQ system software,  *
*  but users are requested to provide copies of derivative works or      *
*  products designed to operate in the CMAQ system to the United States  *
*  Government without restrictions as to use by others.  Software        *
*  that is used with the CMAQ system but distributed under the GNU       *
*  General Public License or the GNU Lesser General Public License is    *
*  subject to their copyright restrictions.                              *
*************************************************************************/

/************************************************************************
* Description:                                                          *
* Function to set an shell environmental variable from program          *
*                                                                       *
* Inputs: ENAME  : Shell environmental variable                         *
*         EVALUE : Value that is ENAME is set to                        *
*                                                                       *
*                                                                       *
*                                                                       *
* Revision History:                                                     *
*     2024: Prototype adapted from D.Wong setenvvar F. Sidi USEPA       *
*                                                                       *
************************************************************************/


#include <stdlib.h>
#include <string.h>
#include <stdio.h>


/** Compilence with Fortran Function calls, append and lower case 
       function name with underscore 
**/

#if FLDMN

#ifndef MIO_SETENVVARC 
#define MIO_SETENVVARC  mio_setenvvarc_
#endif

#elif 

#ifndef MIO_SETENVVARC 
#define MIO_SETENVVARC  mio_setenvvarc
#endif

#endif

int MIO_SETENVVARC( const char * ename,  int *strlen1, 
                   const char * evalue, int *strlen2 ) 
{
/** Scratch Variables: **/
      char *putstr;
      int ierr;

/**Allocate character pointer to be total size of string name ename=evalue **/ 
      putstr = malloc(*strlen1+*strlen2+2);
/**Check if allocation to pointer is successful. If not, return with an error **/

      if ( putstr ) /** Successfull allocation, build string "ename=evalue" **/ 
      { 
        memcpy( putstr, ename, *strlen1 );
        putstr[*strlen1] = '=';  /** Put the equal betwen ename and evalue **/
        memcpy(putstr+*strlen1+1, evalue, *strlen2);  
        putstr[*strlen1+*strlen2+1] = '\0';
        ierr = putenv(putstr);
      }
      else { /** malloc error **/ 
        ierr = 1; 
      }
      /** return status to calling procedure **/ 
      return ierr; 
}
