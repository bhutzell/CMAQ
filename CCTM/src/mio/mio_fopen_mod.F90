! Purpose: open a file either with read only or read write mode.

      module mio_fopen_module

        implicit none

        contains

        subroutine mio_fopen (fname, mode, num_of_outfiles)

          use mio_parameter_module
          use mio_global_data_module
          use mio_get_env_module
          use mio_process_header_info_module
          use mio_search_module
          use mio_time_util_func_module, only : mio_julian_to_calendar

          implicit none

          include 'mpif.h'

          character (*), intent(in)     :: fname
          integer, intent(in)           :: mode
          integer, intent(in), optional :: num_of_outfiles

          character (mio_max_time_length), parameter :: zero_time = '0000-00-00_00:00:00'

          integer :: stat, fnvars, ndims, n_global_atts, unlimited,   &
                     nsteps, t, i, ins, iwe, year, month, day, hh, mm, ss,      &
                     fmode, time_strlen_dim_loc, floc, l_num_of_outfiles
          character (mio_max_time_length) :: time_str
          character (mio_max_filename_len) :: full_name
          logical :: done, called_once = .false.
          character :: tt

          call mio_get_env (full_name, fname, ' ')

          if ( size(mio_file_data) .eq. 0 ) then
!         if (mio_n_infiles == 0) then
!         if (mio_nfiles .eq. 0) then
             t = -1
          else
             t = mio_search (full_name, mio_file_data(:)%full_filename, mio_nfiles)
          end if

! write (mio_logdev, '(A,A,2x,2i5)' ) '==c== mio_fopen: fname, mode, t = ', trim(fname), mode, t

          if (present(num_of_outfiles)) then
            if (called_once) then
               write(mio_logdev, *) ' Abort in mio_fopen: called twice with outfiles'
               stop
            else
               l_num_of_outfiles = num_of_outfiles
               call mio_expand_file_data (l_num_of_outfiles)
               called_once = .true.
            end if
          else
             l_num_of_outfiles = 0
          end if

          if (t .gt. 0) then
             write (mio_logdev, '(a16, a25)') fname, ' has already been opened '
             return
          end if 


             if (mod(mio_n_infiles, mio_df_add_space) == 0) then
                call mio_expand_file_data (l_num_of_outfiles)
             end if

             mio_n_infiles = mio_n_infiles + 1
             mio_nfiles = mio_nfiles + 1
             mio_cfile = mio_nfiles
             floc = mio_cfile
             mio_file_data(floc)%filename = fname
             mio_file_data(floc)%full_filename = full_name

             mio_file_data(floc)%link = -1
             if (mode .eq. mio_read_only) then
                fmode = nf90_nowrite
             else
                fmode = nf90_write
             end if

             stat = nf90_open (full_name, fmode, mio_file_data(floc)%fileid)

             if (stat == nf90_noerr) then
                if (mio_mype == 0) then
                   write (mio_logdev, '(a, a9, a)') trim(fname), ' opened: ', trim(full_name)
                end if
             else
                write (mio_logdev, *) 'Abort in routine mio_fopen opening file ', trim(fname)
                write (mio_logdev, *) '      due to an error ', trim(nf90_strerror(stat))
                stop
             end if

             stat = nf90_inquire (mio_file_data(floc)%fileid, ndims,    &
                                  fnvars, n_global_atts, unlimited)

             mio_file_data(floc)%ndims         = ndims
             mio_file_data(floc)%fnvars        = fnvars
             mio_file_data(floc)%nvars         = fnvars - 1
             mio_file_data(floc)%n_global_atts = n_global_atts
             mio_file_data(floc)%mode          = mode

             allocate (mio_file_data(floc)%dim_name(ndims),                            &
                       mio_file_data(floc)%dim_value(ndims),                           &
                       mio_file_data(floc)%var_time_dep(fnvars),                       &
                       mio_file_data(floc)%var_name(fnvars),                           &
                       mio_file_data(floc)%lvar_name(fnvars),                          &
                       mio_file_data(floc)%units(fnvars),                              &
                       mio_file_data(floc)%var_type(fnvars),                           &
                       mio_file_data(floc)%var_decomp(fnvars),                         &
                       mio_file_data(floc)%var_grid_type(fnvars),                      &
                       mio_file_data(floc)%var_id(fnvars),                             &
                       mio_file_data(floc)%var_ndims(fnvars),                          &
                       mio_file_data(floc)%var_dimids(ndims, fnvars),                  &
                       mio_file_data(floc)%var_dimsize(ndims, fnvars),                 &
                       mio_file_data(floc)%num_var_att(fnvars),                        &
                       mio_file_data(floc)%var_att_name(mio_max_num_var_att, fnvars),  &
                       mio_file_data(floc)%var_att_len(mio_max_num_var_att, fnvars),   &
                       mio_file_data(floc)%var_att_type(mio_max_num_var_att, fnvars),  &
                       mio_file_data(floc)%int_vatt_val(mio_max_num_var_att, fnvars),  &
                       mio_file_data(floc)%real_vatt_val(mio_max_num_var_att, fnvars), &
                       mio_file_data(floc)%char_vatt_val(mio_max_num_var_att, fnvars), &
                       mio_file_data(floc)%glo_att_name(n_global_atts),                &
                       mio_file_data(floc)%glo_att_type(n_global_atts),                &
                       mio_file_data(floc)%glo_att_len(n_global_atts),                 &
                       mio_file_data(floc)%glo_att_crange(n_global_atts*2),            &
                       mio_file_data(floc)%glo_att_irange(n_global_atts*2),            &
                       mio_file_data(floc)%glo_att_rrange(n_global_atts*2),            &
                       mio_file_data(floc)%glo_att_drange(n_global_atts*2),            &
                       stat=stat)

             if (stat .ne. 0) then
                write (mio_logdev, *) 'Abort in routine mio_fopen due to memory allocation error'
                write (mio_logdev, '(A,i3,2x,A)') 'floc, fname: ', floc, trim(mio_file_data(floc)%filename)
                stop
             end if

             call mio_retrieve_dimension_information (mio_file_data(floc))

             i = mio_search ('nCells',                          &
                             mio_file_data(floc)%dim_name,      &
                             mio_file_data(floc)%ndims)

             if (i .gt. 0) then
                mio_file_data(floc)%file_format = mio_mpas_format
                mio_file_data(floc)%nvars       = fnvars - 1
             end if

             call mio_retrieve_variable_information (mio_file_data(floc))

             call mio_retrieve_global_attribute_information (mio_file_data(floc))

             i = mio_search ('TITLE',                            &
                             mio_file_data(floc)%glo_att_name,   &
                             mio_file_data(floc)%n_global_atts)

             if (i .gt. 0) then                                           !  WRF
                mio_file_data(floc)%file_format  = mio_wrf_format

                ! local WRF file DateStrLen dimension location
                time_strlen_dim_loc = mio_search ('DateStrLen',                        &
                                                  mio_file_data(floc)%dim_name,   &
                                                  mio_file_data(floc)%ndims)

                mio_file_data(floc)%time_strlen_dim_loc = time_strlen_dim_loc

                ins = mio_search ('south_north',                      &
                                  mio_file_data(floc)%dim_name,  &
                                  mio_file_data(floc)%ndims)
                if (ins .gt. 0) then
                   mio_file_data(floc)%gl_nrows = mio_file_data(floc)%dim_value(ins)
                else
                   mio_file_data(floc)%gl_nrows = 1
                end if

                iwe = mio_search ('west_east',                        &
                                  mio_file_data(floc)%dim_name,  &
                                  mio_file_data(floc)%ndims)
                if (iwe .gt. 0) then
                   mio_file_data(floc)%gl_ncols = mio_file_data(floc)%dim_value(iwe)
                else
                   mio_file_data(floc)%gl_ncols = 1
                end if

                i = mio_search ('bottom_top',                        &
                                mio_file_data(floc)%dim_name,   &
                                mio_file_data(floc)%ndims)
                if (i .gt. 0) then
                   mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
                else
                   mio_file_data(floc)%nlays = 1
                end if

                mio_file_data(floc)%time_dim_loc = mio_search ('Time',                         &
                                                               mio_file_data(floc)%dim_name,   &
                                                               mio_file_data(floc)%ndims)

                mio_file_data(floc)%layer_dim_loc = mio_search ('bottom_top',                   &
                                                                mio_file_data(floc)%dim_name,   &
                                                                mio_file_data(floc)%ndims)

                mio_file_data(floc)%var_decomp = .false.
                do i = 1, fnvars
                   if ((mio_search (ins, mio_file_data(floc)%var_dimids(:,i),     &
                                    mio_file_data(floc)%var_ndims(i)) > 0) .and.  &
                       (mio_search (iwe, mio_file_data(floc)%var_dimids(:,i),     &
                                    mio_file_data(floc)%var_ndims(i)) > 0)) then
                      mio_file_data(floc)%var_decomp(i) = .true.
                   end if
                end do

             else

                i = mio_search ('IOAPI_VERSION',                    &
                                mio_file_data(floc)%glo_att_name,   &
                                mio_file_data(floc)%n_global_atts)

                if (i .gt. 0) then                                                 !  IOAPI_3
                   mio_file_data(floc)%file_format  = mio_ioapi3_format

                   if (mio_bndy_var_ind == -1) then  ! a non-boundary file
                      mio_file_data(floc)%nbndy_cells = 0
                      mio_file_data(floc)%gl_ncols    = mio_file_data(floc)%dim_value(6)
                      mio_file_data(floc)%gl_nrows    = mio_file_data(floc)%dim_value(5)
                   else
                      mio_file_data(floc)%nbndy_cells = mio_file_data(floc)%dim_value(5)
                      i = mio_search ('NCOLS',                            &
                                      mio_file_data(floc)%glo_att_name,   &
                                      mio_file_data(floc)%n_global_atts)
                      t = mio_file_data(floc)%glo_att_irange(2*i)
                      mio_file_data(floc)%gl_ncols    = mio_file_data(floc)%glo_att_ival(t)

                      i = mio_search ('NROWS',                            &
                                      mio_file_data(floc)%glo_att_name,   &
                                      mio_file_data(floc)%n_global_atts)
                      t = mio_file_data(floc)%glo_att_irange(2*i)
                      mio_file_data(floc)%gl_nrows    = mio_file_data(floc)%glo_att_ival(t)

                      i = mio_search ('NTHIK',                            &
                                      mio_file_data(floc)%glo_att_name,   &
                                      mio_file_data(floc)%n_global_atts)
                      t = mio_file_data(floc)%glo_att_irange(2*i)
                      mio_file_data(floc)%bndy_thickness = mio_file_data(floc)%glo_att_ival(t)
                   end if

                   mio_file_data(floc)%time_dim_loc = mio_search ('TSTEP',                        &
                                                                  mio_file_data(floc)%dim_name,   &
                                                                  mio_file_data(floc)%ndims)
 
                   i = mio_search ('LAY',                                                         &
                                   mio_file_data(floc)%dim_name,                                  &
                                   mio_file_data(floc)%ndims)
                   mio_file_data(floc)%layer_dim_loc = i

                   mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
 
                   mio_file_data(floc)%var_decomp = .false.
                   do i = 1, fnvars
                      if ((mio_search (5, mio_file_data(floc)%var_dimids(:,i),       &
                                       mio_file_data(floc)%var_ndims(i)) > 0) .and.  &
                          (mio_search (6, mio_file_data(floc)%var_dimids(:,i),       &
                                       mio_file_data(floc)%var_ndims(i)) > 0)) then
                         mio_file_data(floc)%var_decomp(i) = .true.
                      end if
                   end do

                else if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then

                   ! local MPAS file StrLen dimension location
                   time_strlen_dim_loc = mio_search ('StrLen',                          &
                                                     mio_file_data(floc)%dim_name, &
                                                     mio_file_data(floc)%ndims)

                   mio_file_data(floc)%time_strlen_dim_loc = time_strlen_dim_loc

                   i = mio_search ('nCells',                     &
                                   mio_file_data(floc)%dim_name, &
                                   mio_file_data(floc)%ndims)

                   mio_file_data(floc)%gl_ncols = mio_file_data(floc)%dim_value(i)
                   mio_file_data(floc)%gl_nrows = 1

                   if (mio_parallelism .eq. mio_serial) then
                      mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
!                  else
!                     need implementation
                   end if
                   mio_file_data(floc)%nrows = 1

                   i = mio_search ('nVertLevels',                     &
                                   mio_file_data(floc)%dim_name,      &
                                   mio_file_data(floc)%ndims)
                   if (i .gt. 0) then
                      mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
                   else
                      i = mio_search ('nVertLevelsP1',                &
                                      mio_file_data(floc)%dim_name,   &
                                      mio_file_data(floc)%ndims)
                      if (i .gt. 0) then
                         mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i) - 1
                      else
                         ! # of layers information does not exist, set to 1 by default'
                         mio_file_data(floc)%nlays = 1
                      end if
                   end if

                   mio_file_data(floc)%time_dim_loc = mio_search ('Time',                         &
                                                                  mio_file_data(floc)%dim_name,   &
                                                                  mio_file_data(floc)%ndims)

                   mio_file_data(floc)%layer_dim_loc = mio_search ('nVertLevels',                  &
                                                                   mio_file_data(floc)%dim_name,   &
                                                                   mio_file_data(floc)%ndims)

                   ins = mio_search ('nCells',                      &
                                     mio_file_data(floc)%dim_name,  &
                                     mio_file_data(floc)%ndims)

                   mio_file_data(floc)%var_decomp = .false.
                   do i = 1, fnvars
                      if (mio_search (ins, mio_file_data(floc)%var_dimids(:,i),       &
                                       mio_file_data(floc)%var_ndims(i)) > 0) then
                         mio_file_data(floc)%var_decomp(i) = .true.
                      end if
                   end do
                end if
             end if

!          if (unlimited .ge. 1) then
!             nsteps = mio_file_data(floc)%dim_value(mio_file_data(floc)%time_dim_loc)
!             mio_file_data(floc)%unlimited = unlimited
!          else
!             nsteps = 0
!          end if

             if (mio_file_data(floc)%time_dim_loc .gt. 0) then
                nsteps = mio_file_data(floc)%dim_value(mio_file_data(floc)%time_dim_loc)
!               if (mio_file_data(floc)%mode .eq. mio_read_write) then
!                  allocate (mio_file_data(floc)%timestamp(nsteps+mio_preset_num_tsteps), stat=stat)
!               else
                   allocate (mio_file_data(floc)%timestamp(nsteps), stat=stat)
!               end if
             else
                allocate (mio_file_data(floc)%timestamp(1), stat=stat)
                mio_file_data(floc)%timestamp(1) = zero_time
                nsteps = 1
             end if

             mio_file_data(floc)%nsteps = nsteps

             ! setup domain decomposition mapping
             ! the last dimension indicates cross or dot grid
             allocate (mio_file_data(floc)%ncols_pe(mio_nprocs, 2),     &
                       mio_file_data(floc)%nrows_pe(mio_nprocs, 2),     &
                       mio_file_data(floc)%colde_pe(2, mio_nprocs, 2),  &
                       mio_file_data(floc)%rowde_pe(2, mio_nprocs, 2),  &
                       stat=stat)
 
             mio_file_data(floc)%colde_pe = 0
             mio_file_data(floc)%rowde_pe = 0

             mio_file_data(floc)%ncols_pe = mio_domain_ncols_pe
             mio_file_data(floc)%nrows_pe = mio_domain_nrows_pe
             mio_file_data(floc)%colde_pe = mio_domain_colde_pe
             mio_file_data(floc)%rowde_pe = mio_domain_rowde_pe

             if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then         ! this is for IOAPI data 
                if (mio_parallelism .eq. mio_serial) then
                   mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
                   mio_file_data(floc)%nrows = mio_file_data(floc)%gl_nrows
                else
                   if (mio_file_data(floc)%ndims == 5) then                           ! boundary file 
                      mio_file_data(floc)%grid_type = 'b'
                      mio_file_data(floc)%ncols = -1
                      mio_file_data(floc)%nrows = -1
                   else

                      mio_file_data(floc)%grid_type = '-'
                      call mio_subhfile (fname)

                      if (mio_domain_nrows .eq. 1) then   ! for MPAS stack group data in IOAPI3 format
                         mio_file_data(floc)%grid_type = 'm'
                         mio_file_data(floc)%ncols = mio_domain_ncols_pe(mio_mype_p1,1)
                         mio_file_data(floc)%nrows = mio_file_data(floc)%dim_value(5)
                      end if

                   end if
                end if

                allocate (mio_file_data(floc)%tflag(2,nsteps), stat=stat)
                do t = 1, nsteps
                   stat = nf90_get_var(mio_file_data(floc)%fileid,                    &
                                       mio_file_data(floc)%var_id(mio_time_var_ind),  &
                                       mio_file_data(floc)%tflag(:,t),                &
                                       start = (/ 1, 1, t /),                         &
                                       count = (/ 2, 1, 1 /))
                   if (stat .ne. nf90_noerr) then
                      write (mio_logdev, *) ' Abort in routine mio_fopen while getting time stamp info '
                      write (mio_logdev, *) ' for IOAPI_3 file due to an error ', trim(nf90_strerror(stat))
                      stop
                   end if
                   call mio_julian_to_calendar (mio_file_data(floc)%tflag(1,t), year, month, day)
                   mm = mio_file_data(floc)%tflag(2,t) / 100

                   hh = mm / 100
                   mm = mod(mm, 100)
                   ss = mod(mio_file_data(floc)%tflag(2,t), 100)

                   write (mio_file_data(floc)%timestamp(t),                      &
                        '(i4.4, a1, i2.2, a1, i2.2, 1a, i2.2, 2(a1, i2.2))')     &
                        year, '-', month, '-', day, '_', hh, ':', mm, ':', ss
                end do
             else if ((mio_file_data(floc)%file_format .eq. mio_wrf_format) .or.   &   ! this is for WRF data 
                      (mio_file_data(floc)%file_format .eq. mio_mpas_format)) then     ! this is for MPAS data 

                if (mio_file_data(floc)%file_format .eq. mio_wrf_format) then          ! this is for WRF data 
                   if (mio_parallelism .eq. mio_serial) then
                      mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
                      mio_file_data(floc)%nrows = mio_file_data(floc)%gl_nrows
                   else
                      mio_file_data(floc)%ncols = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                      mio_file_data(floc)%nrows = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                   end if
                end if

!             allocate (mio_file_data(floc)%timestamp(nsteps), stat=stat)
                do t = 1, nsteps
                   time_str = ' '
                   done = .false.
                   i = 0
                   do while ((.not. done) .and.  (i .lt. mio_file_data(floc)%dim_value(time_strlen_dim_loc)))
                      i = i + 1
                      stat = nf90_get_var(mio_file_data(floc)%fileid,                      &
                                          mio_file_data(floc)%var_id(mio_time_var_ind),    &
                                          tt,                                                   &
                                          start = (/ i, t /),                                   &
                                          count = (/ 1, 1 /) )
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) ' Abort in routine mio_fopen while getting time stamp info '
                         write (mio_logdev, *) ' due to an error ', trim(nf90_strerror(stat))
                         stop
                      end if
                      if ((tt .eq. ' ') .or. (ichar(tt) .eq. 0)) then
                         done = .true.
                      else
                         time_str(i:i) = tt
                      end if
                   end do
                   mio_file_data(floc)%timestamp(t) = time_str
                end do
!             if (nsteps > 1) then
!                mio_file_data(floc)%tstep = 0
!             else
!                mio_file_data(floc)%tstep = 0
!             end if

             end if

!   do i = 1, mio_nfiles
!      write (mio_logdev, '(A,i3,2x,A)' ) '==c==', i, trim(mio_file_data(i)%filename)
!   end do

        end subroutine mio_fopen

! ---------------------------------------------------------------------------------------
        subroutine mio_expand_file_data (num_of_outfiles)

          use mio_global_data_module

          integer, intent(in) :: num_of_outfiles

          integer :: n, stat, df_size

          if (mio_n_infiles == 0) then
             allocate (mio_file_data0(0:mio_df_add_space+num_of_outfiles), stat=stat)
             if (stat .ne. 0) then
                write (mio_logdev, *) 'Abort in routine mio_expand_file_data due to insufficient additional space'
                stop
             end if
             mio_file_data => mio_file_data0
          else
             mio_fd_circular = mod((mio_fd_circular + 1), 2)
             if (mio_fd_circular == 0) then
                df_size = size(mio_file_data1) - 1
!               allocate (mio_file_data0(0:df_size+mio_df_add_space), stat=stat)
                allocate (mio_file_data0(0:df_size+mio_df_add_space+num_of_outfiles), stat=stat)
                do n = 1, mio_nfiles
                   call mio_copy_file_data (mio_file_data1(n), mio_file_data0(n))
                end do
                deallocate (mio_file_data1)
                mio_file_data => mio_file_data0
             else
                df_size = size(mio_file_data0) - 1
!               allocate (mio_file_data1(0:df_size+mio_df_add_space), stat=stat)
                allocate (mio_file_data1(0:df_size+mio_df_add_space+num_of_outfiles), stat=stat)
                do n = 1, mio_nfiles
                   call mio_copy_file_data (mio_file_data0(n), mio_file_data1(n))
                end do
                deallocate (mio_file_data0)
                mio_file_data => mio_file_data1
             end if
          end if

        end subroutine mio_expand_file_data

! ---------------------------------------------------------------------------------------
        subroutine mio_subhfile (fname)

          use mio_global_data_module
          use mio_search_module
          use mio_get_global_attr_module

          character (*), intent(in) :: fname

          real( 8 ), parameter :: half = 0.5d+00
          real( 8 ), parameter :: one  = 1.0d+00
          real( 8 ), parameter :: ten  = 1.0d+01
          real( 8 ), parameter :: onek = 1.0d+03
          real( 8 ), parameter :: tenk = 1.0d+04
          real( 8 ), parameter :: tol  = one / onek
          real( 8 ), parameter :: thou = 1.0d-03
          real( 8 ), parameter :: min_double = 1.0d-08

          integer :: loc_strtcol, loc_endcol, loc_strtrow, loc_endrow, &
                     floc, dotfile, pos, gxoff, gyoff
          real( 8 ), save :: xorig_c, yorig_c
          real( 8 )       :: lxorig3d, lyorig3d,       &
                             lxcent3d, lycent3d,       &
                             lxcell3d, lycell3d,       &
                             reloffx, reloffy,         &
                             xorig_f, yorig_f
          logical, save :: firstime = .true.

#ifndef mpas
          if (firstime) then
             firstime = .false.
             xorig_c = mio_domain_xorig / mio_domain_xcell
             xorig_c = real(idnint(onek * xorig_c), 8) / onek
             yorig_c = mio_domain_yorig / mio_domain_ycell
             yorig_c = real(idnint(onek * yorig_c), 8) / onek
          end if

          floc = mio_search (fname)
          if (floc > 0) then
             call mio_get_global_attr (fname, 'XORIG', lxorig3d)
             call mio_get_global_attr (fname, 'YORIG', lyorig3d)
             call mio_get_global_attr (fname, 'XCENT', lxcent3d)
             call mio_get_global_attr (fname, 'YCENT', lycent3d)
             call mio_get_global_attr (fname, 'XCELL', lxcell3d)
             call mio_get_global_attr (fname, 'YCELL', lycell3d)
          else
             write (mio_logdev, '(a45, a16)')                        &
               ' Abort in mio_subhfile due to file not found ', trim(fname)
             stop
          end if

! check some header data against GRIDDESC

          if ( abs( lxcent3d - mio_domain_xcent ) .gt.                       &
               tol * abs( lxcent3d + mio_domain_xcent + tol ) .or.           &
               abs( lycent3d - mio_domain_ycent ) .gt.                       &
               tol * abs( lycent3d + mio_domain_ycent + tol ) .or.           &
               abs( lxcell3d - mio_domain_xcell ) .gt. tol * lxcell3d .or.   &
               abs( lycell3d - mio_domain_ycell ) .gt. tol * lycell3d ) then

             write (mio_logdev, '(a)') trim(fname) // ' file header inconsistent with GRIDDESC'
             write (mio_logdev,2003 ) mio_domain_xcent, lxcent3d, mio_domain_ycent, lycent3d, &
                                      mio_domain_xcell, lxcell3d, mio_domain_ycell, lycell3d
 2003        format(/ 5x, 'XCENT_B:', f20.12, 2x, 'XCENT3D (file):', f20.12  &
                    / 5x, 'YCENT_B:', f20.12, 2x, 'YCENT3D (file):', f20.12  &
                    / 5x, 'XCELL_B:', f20.12, 2x, 'XCELL3D (file):', f20.12  &
                    / 5x, 'YCELL_B:', f20.12, 2x, 'YCELL3D (file):', f20.12 )
             write (mio_logdev, '(a23)') ' Abort in mio_subhfile '
             stop
          end if

! Convert to grid cell coord and truncate

          xorig_f = lxorig3d / lxcell3d
          xorig_f = real( idnint( onek * xorig_f ), 8 ) / onek

          yorig_f = lyorig3d / lycell3d
          yorig_f = real( idnint( onek * yorig_f ), 8 ) / onek

! check the file against the processor setup (COORD.EXT), and get the offsets

          reloffx = thou * real( idnint( onek * ( xorig_c - xorig_f ) ), 8 )
          reloffx = reloffx - one * real( idnint( reloffx ), 8 )

          mio_file_data(floc)%grid_type = 'c'
          dotfile = 0
          pos     = 1
          if ( abs( reloffx ) .gt. min_double ) then ! it better be a dot file
             if ( abs( reloffx ) .lt. half - min_double .or.       &
                  abs( reloffx ) .gt. half + min_double ) then
                write (mio_logdev, '(a)') trim(fname) // ' Xorig inconsistent with GRIDDESC'
                write (mio_logdev,* ) '    RELOFFX: ', reloffx
                write (mio_logdev,* ) '    XORIG_GD, XORIG_F: ', xorig_c, xorig_f
                write (mio_logdev,* ) ' Abort in mio_subhfile '
                stop
             end if
          end if

          reloffy = thou * real( idnint( onek * ( yorig_c - yorig_f ) ), 8 )
          reloffy = reloffy - one * real( idnint( reloffy ), 8 )

          if ( abs( reloffy ) .gt. min_double ) then ! it better be a dot file
             if ( abs( reloffy ) .lt. half - min_double .or.       &
                  abs( reloffy ) .gt. half + min_double ) then
                write (mio_logdev, '(a)') trim(fname) // ' Yorig inconsistent with GRIDDESC'
                write (mio_logdev,* ) '    RELOFFY: ', reloffy
                write (mio_logdev,* ) '    YORIG_GD, YORIG_F: ', yorig_c, yorig_f
                write (mio_logdev,* ) ' Abort in mio_subhfile '
                stop
             else
                dotfile = 1
                pos     = 2
                mio_file_data(floc)%grid_type = 'd'
             end if
          end if
          if ( abs( reloffx - reloffy ) .gt. min_double ) then
             write (mio_logdev, '(a)') ' inconsistent X- and Y-resolution (file vs. model) for file ' // trim(fname)
             write (mio_logdev, * ) ' Abort in mio_subhfile '
             stop
          end if

          gxoff = idint( xorig_c - xorig_f )
          loc_strtcol = gxoff + mio_domain_colde_pe(1,mio_mype_p1,1)
          loc_endcol  = loc_strtcol + mio_domain_ncols_pe(mio_mype_p1,1) - 1 + dotfile

          gyoff = idint( yorig_c - yorig_f )
          loc_strtrow = gyoff + mio_domain_rowde_pe(1,mio_mype_p1,1)
          loc_endrow  = loc_strtrow + mio_domain_nrows_pe(mio_mype_p1,1) - 1 + dotfile

#ifndef twoway
          if ( gxoff .lt. 0 .or. gyoff .lt. 0 ) then
             write (mio_logdev, '(a)') trim(fname) // ' does not cover model domain '
             write (mio_logdev, * ) ' Abort in mio_subhfile '
             stop
          end if
#endif

#ifdef twoway
          if ( ( fname .eq. grid_cro_2d ) .or.             &
               ( fname .eq. grid_dot_2d ) ) then
             loc_endcol  = loc_endcol - loc_strtcol + 1
             loc_strtcol = 1
             loc_endrow  = loc_endrow - loc_strtrow + 1
             loc_strtrow = 1
          else if ( ( fname .eq. met_cro_2d ) .or.         &
                    ( fname .eq. met_cro_3d ) .or.         &
                    ( fname .eq. met_dot_3d ) ) then
             loc_endcol  = loc_endcol - loc_strtcol + 2
             loc_strtcol = 2
             loc_endrow  = loc_endrow - loc_strtrow + 2
             loc_strtrow = 2
          end if
#endif

          mio_file_data(floc)%colde_pe(1, mio_mype_p1, pos) = loc_strtcol
          mio_file_data(floc)%colde_pe(2, mio_mype_p1, pos) = loc_endcol
          mio_file_data(floc)%rowde_pe(1, mio_mype_p1, pos) = loc_strtrow
          mio_file_data(floc)%rowde_pe(2, mio_mype_p1, pos) = loc_endrow

          mio_file_data(floc)%ncols = loc_endcol - loc_strtcol + 1
          mio_file_data(floc)%nrows = loc_endrow - loc_strtrow + 1

          mio_file_data(floc)%ncols_pe(mio_mype_p1, pos) = mio_file_data(floc)%ncols
          mio_file_data(floc)%nrows_pe(mio_mype_p1, pos) = mio_file_data(floc)%nrows
#endif

        end subroutine mio_subhfile

      end module mio_fopen_module
