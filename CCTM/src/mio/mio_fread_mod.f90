! Purpose: read a variable from a file

      module mio_fread_module

        use mio_global_data_module, only : mio_file_data, mio_mype_p1,      &
                                           mio_parallelism, mio_logdev,     &
                                           mio_mype, mio_mpas_dmap
        use mio_parameter_module, only : mio_max_filename_len,              &
                                         mio_max_varname_len,               &
                                         mio_max_time_length, mio_serial,   &
                                         mio_pseudo, mio_mpas_format,       &
                                         mio_wrf_format,                    &
                                         mio_ioapi3_format
        use mio_search_module
        use mio_get_data_module

        implicit none

        interface mio_fread
          module procedure mio_fread_0d_real,         &
                           mio_fread_1d_real,         &
                           mio_fread_2d_real,         &
                           mio_fread_2d_real_bdy,     &
                           mio_fread_3d_real,         &
                           mio_fread_3d_real_sub,     &
                           mio_fread_3d_real_lay,     &
                           mio_fread_0d_double,       &
                           mio_fread_1d_double,       &
                           mio_fread_2d_double,       &
                           mio_fread_3d_double,       &
                           mio_fread_3d_double_lay,   &
                           mio_fread_0d_int,          &
                           mio_fread_1d_int,          &
                           mio_fread_2d_int,          &
                           mio_fread_3d_int,          &
                           mio_fread_3d_int_lay,      &
                           mio_fread_char
        end interface

        contains

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_0d_real'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_1d_real'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        else if (mio_file_data(floc)%grid_type .eq. 'm') then  ! MPAS stack group data in IOAPI3 format (serial)
!                           mycount(2) = mio_file_data(floc)%nrows
!                           mycount(2) = size(data)
!                           mystart(4) = t
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_2d_real'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat, pos
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            pos = 1
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            pos = 2
                         end if

                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, pos)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, pos)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, pos)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, pos)
                      end if

!                     if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
!                                             mio_file_data(floc)%var_id(v),   &
!                                             mystart, mycount, data) ) then
!                        lerror = .true.
!                     end if

                      stat = nf90_get_var(mio_file_data(floc)%fileid,              &
                                          mio_file_data(floc)%var_id(v),           &
                                          data,                                    &
                                          start = mystart,                         &
                                          count = mycount)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error in routine mio_get_data_2d_real'
                         write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
                         lerror = .true.
                      end if

                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_real_bdy (fname, vname, caller, data, bdy_size, timestamp)

! this routine can be removed in the future once parallelism is
! determined w.r.t. individual variable rather than entire model

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:)
          character (*), intent(in) :: caller
          integer, intent(in)       :: bdy_size
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_2d_real_bdy'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat, pos
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

! for the time being it is for IOAPI3 type file
                   mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if

                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_real_bdy

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_3d_real'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

!                  if (mio_parallelism .eq. mio_serial) then
                   if ((mio_parallelism .eq. mio_serial) .or. (fname(1:5) == 'STK_G')) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_real_sub (fname, vname, caller, strcol, endcol, &
                                          strrow, endrow, strlay, endlay, data, timestamp)
!       this routine is for CMAQ application only

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:,:)
          character (*), intent(in) :: caller
          integer, intent(in)       :: strcol, endcol, strrow, endrow, strlay, endlay
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_3d_real_sub'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat, pos
          logical :: lerror = .false.
          character (80) :: err_message

          floc = mio_search (fname)

          if (floc < 0) then
             err_message = '        cannot find file ' // trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                err_message = '        cannot find variable ' // trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   err_message = ' Error: requested timestamp ' // trim(timestamp) // ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      mycount(1) = endcol - strcol + 1
                      mycount(2) = endrow - strrow + 1
                      mycount(3) = endlay - strlay + 1
                      mystart(1) = strcol
                      mystart(2) = strrow
                      mystart(3) = strlay

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   else
                      err_message = ' Applied mio_fread_2d_real_sub to the wrong file type'
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) trim(err_message)
             stop
          end if

        end subroutine mio_fread_3d_real_sub

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_real_lay (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (21), parameter :: pname = 'mio_fread_3d_real_lay'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
!                     if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
!                     else if (mio_file_data(floc)%grid_type .eq. 'd') then
!                        mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                        mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
!                        mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
!                     end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_real_lay

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_0d_double'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_1d_double'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_2d_double'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                         end if
                      end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_3d_double'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_double_lay (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (23), parameter :: pname = 'mio_fread_3d_double_lay'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_double_lay

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_0d_int'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_1d_int'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        else if (mio_file_data(floc)%grid_type .eq. 'm') then  ! MPAS stack group data in IOAPI3 format (serial)
!                           mycount(2) = mio_file_data(floc)%nrows
!                           mycount(2) = size(data)
!                           mystart(4) = t
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_2d_int'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                         end if
                      end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_3d_int'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

!                  if (mio_parallelism .eq. mio_serial) then
                   if ((mio_parallelism .eq. mio_serial) .or. (fname(1:5) == 'STK_G')) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_int_lay (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fread_3d_int_lay'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_int_lay

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_char (fname, vname, caller, data, timestamp)

          character (*), intent(in)  :: fname
          character (*), intent(in)  :: vname
          character (*), intent(out) :: data
          character (*), intent(in)  :: caller
          character (*), intent(in), optional :: timestamp

          character (14), parameter :: pname = 'mio_fread_char'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(2) = t

                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_char

      end module mio_fread_module
