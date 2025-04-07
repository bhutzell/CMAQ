!===============================================================================
! Purpose:  Define the mapping between CMAQ species and aerosol information, water
!           soluable, water insoluable, elementary carbon, sea salt, and water,
!           that will affect the radiation calculation
!
! Revised:  11 Aug 2011  Original version.  David Wong
!           21 Oct 2015  Updated water insoluble species list
!           22 Nov 2016  Constructed water soluble and insoluble list dynamically
!                        based on a given chemical mechanism and AE scheme
!           01 Aug 2019  -- renamed ASEACATK to ASEACAT
!                        -- added a H2O species, AORGH2OJ
!           10 Jan 2024  -- Incorporated unified coupler implementation (David Wong)
!===============================================================================

  module twoway_cgrid_aerosol_spc_map_module

    use aero_data

    INTEGER, PARAMETER :: num_twoway_ae_cmaq_spc = 44
    INTEGER, PARAMETER :: num_twoway_ae_cmaq_spc_other = 12
    INTEGER, PARAMETER :: n_feedback_var  = 22

! for feedback

    CHARACTER (LEN = 16), PARAMETER :: feedback_vlist(n_feedback_var) = &
      (/ 'WS_1            ', 'WS_2            ', 'WS_3            ',  &
         'IS_1            ', 'IS_2            ', 'IS_3            ',  &
         'EC_1            ', 'EC_2            ', 'EC_3            ',  &
         'SEASALT_1       ', 'SEASALT_2       ', 'SEASALT_3       ',  &
         'WATER_1         ', 'WATER_2         ', 'WATER_3         ',  &
         'DIAMETERS_1     ', 'DIAMETERS_2     ', 'DIAMETERS_3     ',  &
         'SD_1            ', 'SD_2            ', 'SD_3            ',  &
         'O3              '                                           &
      /)

    integer :: twoway_ae_cmaq_spc_name_index (num_twoway_ae_cmaq_spc)
    integer :: twoway_ae_cmaq_spc_name_other_index (num_twoway_ae_cmaq_spc_other)

    contains

    integer function find_index (vname) result (index)

    implicit none

    character (len = 16), intent(in) :: vname

    logical :: found
    integer :: s, m

    found = .false.
    s = 0
    do while ((s < n_aerospc) .and. (.not. found))
       s = s + 1
       m = 0
       do while ((m < n_mode) .and. (.not. found))
          m = m + 1
          if (aerospc(s)%name(m) == vname) then
             found = .true.
             index = aerospc_map(s, m)
          end if
       end do
    end do

    if (.not. found) then
       print *, ' Error: variable ', trim(vname), ' not found '
       stop
    end if

    end function find_index

  end module twoway_cgrid_aerosol_spc_map_module
