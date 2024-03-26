!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      module get_env_module

! Function: get environment variables

! Revision History:
!        2010 D.Wong: initial implementation
!  2 Feb 2010 D.Wong: provided an optional outputing device option,
!                     absorbed get_envlist function

        implicit none

        integer, parameter :: max_str_len = 10000

        character (max_str_len) :: loc_str

        interface get_env
          module procedure get_env_int,      &
                           get_env_float,    &
                           get_env_double,   &
                           get_env_char,     &
                           get_env_logical
        end interface

        contains

! --------------------------------------------------------------------------------
        subroutine get_env_int (env_value, env_var, default_env_value, logdev)

          integer, intent(out)      :: env_value
          character (*), intent(in) :: env_var
          integer, intent(in)       :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A16,2x,A,2x,i10, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A16,2x,A,2x,i10)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_int

! --------------------------------------------------------------------------------
        subroutine get_env_float (env_value, env_var, default_env_value, logdev)

          real, intent(out)         :: env_value
          character (*), intent(in) :: env_var
          real, intent(in)          :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A16,2x,A,2x,e10.3, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A16,2x,A,2x,e10.3)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_float

! --------------------------------------------------------------------------------
        subroutine get_env_double (env_value, env_var, default_env_value, logdev)

          real (8), intent(out)     :: env_value
          character (*), intent(in) :: env_var
          real (8), intent(in)          :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             read (loc_str, *) env_value
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A16,2x,A,2x,e10.3, 1x, a9)' ), env_var,'|', env_value, '(default)' 
             else if (regular) then
                write( loc_logdev, '(A16,2x,A,2x,e10.3)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_double

! --------------------------------------------------------------------------------
        subroutine get_env_char (env_value, env_var, default_env_value, logdev)

          character (*), intent(out) :: env_value
          character (*), intent(in)  :: env_var
          character (*), intent(in)  :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: loc_logdev, length
          logical :: default, regular
          character (50) :: myfmt

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          regular = .false.
          default = .false.

          if (len(trim(loc_str)) == 0) then
             env_value = default_env_value
             default = .true.
          else
             env_value = loc_str
             regular = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             length = len_trim(env_value)
             if (default) then
                if (length .eq. 0) then
                   write( loc_logdev, '(A16, 2x, A, 13x, a9)') env_var, '|', '(default)'
                else
                   write (myfmt, '(a18, i3.3, a9)') '(A16, 2x, A, 2x, A', length, ', 1x, a9)'
                   write( loc_logdev, myfmt) env_var, '|', env_value, '(default)'
                end if
             else if (regular) then
                write (myfmt, '(a18, i3.3, a1)') '(A16, 2x, A, 2x, A', length, ')'
                write( loc_logdev, myfmt) env_var,'|', env_value
             end if
          end if

        end subroutine get_env_char

! --------------------------------------------------------------------------------
        subroutine get_env_logical (env_value, env_var, default_env_value, logdev)

          logical, intent(out)      :: env_value
          character (*), intent(in) :: env_var
          logical, intent(in)       :: default_env_value
          integer, intent(in), optional :: logdev

          integer :: length
          integer :: loc_logdev
          logical :: default, regular

          call getenv (env_var, loc_str)

          if (present(logdev)) then
             loc_logdev = logdev
          else
             loc_logdev = 6
          end if

          length = len(trim(loc_str))
          regular = .false.
          default = .false.

          if (length <= 0) then
             env_value = default_env_value
             default = .true.
          else if ((length == 1) .and. ((loc_str(1:1) .eq. 'Y') .or.       &
                                        (loc_str(1:1) .eq. 'y') .or.       &
                                        (loc_str(1:1) .eq. 'T') .or.       &
                                        (loc_str(1:1) .eq. 't'))) then
             env_value = .true.
             regular = .true.
          else if ((length == 1) .and. ((loc_str(1:1) .eq. 'N') .or.       &
                                        (loc_str(1:1) .eq. 'n') .or.       &
                                        (loc_str(1:1) .eq. 'F') .or.       &
                                        (loc_str(1:1) .eq. 'f'))) then
             env_value = .false.
             regular = .true.
          else if ((trim(loc_str) == '.TRUE.') .or.                        &
                   (trim(loc_str) == '.true.') .or.                        &
                   (trim(loc_str) == '.True.') .or.                        &
                   (trim(loc_str) == 'TRUE') .or.                          &
                   (trim(loc_str) == 'true') .or.                          &
                   (trim(loc_str) == 'True') .or.                          &
                   (trim(loc_str) == 'YES') .or.                           &
                   (trim(loc_str) == 'yes') .or.                           &
                   (trim(loc_str) == 'Yes')) then
             env_value = .true.
             regular = .true.
          else if ((trim(loc_str) == '.FALSE.') .or.                       &
                   (trim(loc_str) == '.false.') .or.                       &
                   (trim(loc_str) == '.False.') .or.                       &
                   (trim(loc_str) == 'FALSE') .or.                         &
                   (trim(loc_str) == 'false') .or.                         &
                   (trim(loc_str) == 'False') .or.                         &
                   (trim(loc_str) == 'NO') .or.                            &
                   (trim(loc_str) == 'no') .or.                            &
                   (trim(loc_str) == 'No')) then
             env_value = .false.
             regular = .true.
          else
             write (loc_logdev, *) ' Note: Variable ', trim(env_var), ' improperly formatted'
             env_value = default_env_value
             default = .true.
          end if

          if ( loc_logdev .gt. 0 ) then
             if (default) then
                write( loc_logdev, '(A16,2x,A,10x,L, 1x, a9)' ), env_var,'|', env_value, '(default)'
             else if (regular) then
                write( loc_logdev, '(A16,2x,A,10x,L)' ), env_var,'|', env_value
             end if
          end if

        end subroutine get_env_logical

! --------------------------------------------------------------------------------
      end module get_env_module
