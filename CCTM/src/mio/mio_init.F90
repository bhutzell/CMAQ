! Purpose: Initialize MIO system by reading in how many input and
!          output files, forming a parallel domain decomposition map,
!          and reading in output variable definition if it is available

      subroutine mio_init (npcol, nprow, ncols, nrows, logdev, ptype)

        use mio_parameter_module
        use mio_global_data_module
        use mio_fopen_module
        use mio_get_env_module
        use mio_util_func_module, only : mio_extract_string
        use mio_search_module
        use mio_interpolation_module

        implicit none

#ifdef parallel
        include 'mpif.h'
#endif

        integer, intent(in) :: npcol, nprow, ncols, nrows
        integer, intent(in), optional :: logdev
        character (*), intent(in), optional :: ptype

        character (40)                   :: str, str1, str2
        character (mio_max_filename_len) :: tstr, finfo
        character (mio_max_str_len), allocatable :: ext_str(:)
        character (mio_max_str_len) :: tvname, temp_str
        integer :: i, j, n, stat, num_of_infiles,  num_of_outfiles, &
                   mode, stage
        logical :: eof, mpi_init_called, found_first_line

        integer, external :: mio_setup_logdev

        interface
          subroutine mio_setup_decomp (nprocs, npcol, nprow, ncols, nrows, &
                                       op_type, ncols_pe, nrows_pe,        &
                                       colde_pe, rowde_pe)
            integer, intent(in)  :: nprocs
            integer, intent(in)  :: npcol
            integer, intent(in)  :: nprow
            integer, intent(in)  :: ncols
            integer, intent(in)  :: nrows
            integer, intent(in)  :: op_type
            integer, intent(out) :: ncols_pe(:,:)
            integer, intent(out) :: nrows_pe(:,:)
            integer, intent(out) :: colde_pe(:,:,:)
            integer, intent(out) :: rowde_pe(:,:,:)
          end subroutine mio_setup_decomp
        end interface

#ifdef parallel
        call mpi_initialized (mpi_init_called, stat)
        if (.not. mpi_init_called) then
           call mpi_init (stat)
        end if

        mio_nprocs   = npcol * nprow
#else
        mio_nprocs   = 1
#endif

        if (present(logdev)) then
           mio_logdev = logdev
        else
           mio_logdev = mio_setup_logdev ()
        end if

        mio_npcol    = npcol
        mio_nprow    = nprow

        mio_io_pe_inclusive = .false.

        if (mio_nprocs .eq. 1) then
           mio_parallelism     = mio_serial          ! serial I/O
           mio_mype            = 0
           mio_io_pe_inclusive = .true.
        else
           call mio_setup_rank (mio_mype)
           if (present(ptype)) then
              mio_parallelism = mio_true_parallel    ! true parallel using pnetCDF or netCDF-4
              if (mod(mio_mype, npcol) == 0) then
                 mio_io_pe_inclusive = .true.
              end if
           else
              mio_parallelism = mio_pseudo           ! pseudo parallel
              if (mio_mype == 0) then
                 mio_io_pe_inclusive = .true.
              end if
           end if
        end if
        mio_mype_p1 = mio_mype + 1

        call mio_read_dscgrid

        if (mio_domain_gdtyp > 0) then  ! CMAQ simulation
           mio_domain_ncols = mio_domain_ncols
           mio_domain_nrows = mio_domain_nrows
        else
           mio_domain_ncols = ncols
           mio_domain_nrows = nrows
        end if

        allocate (mio_domain_ncols_pe(mio_nprocs, 2),      &
                  mio_domain_nrows_pe(mio_nprocs, 2),      &
                  mio_domain_colde_pe(2, mio_nprocs, 2),   &
                  mio_domain_rowde_pe(2, mio_nprocs, 2),   &
                  stat=stat)

        call mio_setup_decomp (mio_nprocs, npcol, nprow,                 &
                               mio_domain_ncols, mio_domain_nrows,       &
                               mio_ioapi3_format,                        &
                               mio_domain_ncols_pe, mio_domain_nrows_pe, &
                               mio_domain_colde_pe, mio_domain_rowde_pe)

        mio_n_infiles  = 0
        mio_n_outfiles = 0
        mio_nfiles = 0

      end subroutine mio_init

! ----------------------------------------------------------------------
      subroutine mio_init2 

        use mio_parameter_module
        use mio_global_data_module
        use mio_fopen_module
        use mio_get_env_module
        use mio_util_func_module, only : mio_extract_string
        use mio_search_module
        use mio_interpolation_module

        implicit none

        character (40)                   :: str, str1, str2
        character (mio_max_filename_len) :: tstr, finfo
        character (mio_max_str_len), allocatable :: ext_str(:)
        character (mio_max_str_len) :: tvname, temp_str
        integer :: i, j, n, stat, num_of_infiles,  num_of_outfiles, &
                   mode, stage
        logical :: eof, mpi_init_called, found_first_line

        allocate (ext_str(2), stat=stat)

        ! obtain the environment variable of the file that contains all
        ! information about input and output files
        call mio_get_env (finfo, 'mio_file_info', ' ')

        open (unit = mio_iunit, file = finfo, status = 'old', iostat=stat)
        if (stat .ne. 0) then
           write (mio_logdev, *) ' Abort in routine mio_init while opening mio_file_info file'
           stop
        end if

        ! first line is the line that contains number of input and output files and skips 
        ! all lines with comments
        found_first_line = .false.
        do while (.not. found_first_line)
           read (mio_iunit, '(a40)') temp_str
           str = adjustl(trim(temp_str))
           if (str(1:1) .ne. '#') then 
              found_first_line = .true.

              read (str, *, iostat=stat) str1, num_of_infiles, str2, num_of_outfiles
              if (stat .ne. 0) then
                 write (mio_logdev, *) ' Abort: In routine mio_init, due to incorrect '
                 write (mio_logdev, *) '        number of arguments in the mio_file_info '
                 write (mio_logdev, *) '        file to indicate number of input and output'
                 stop
              end if

           end if
        end do

        mio_cfile  = 0

        i = 0
        do while (i < num_of_infiles)

           read (mio_iunit, '(a)') temp_str
           tstr = adjustl(trim(temp_str))

           if (tstr(1:1) .ne. '#') then
              i = i + 1
              call mio_extract_string (tstr, ext_str, n)

              mode = mio_read_only
              if (n .gt. 1) then
                 if ((ext_str(2) .eq. 'w') .or.    &
                     (ext_str(2) .eq. 'rw')) then
                    mode = mio_read_write
                 end if
              end if

              mio_cfile = mio_cfile + 1

              if (i == 1) then
                 call mio_fopen (ext_str(1), mode, num_of_outfiles)
              else
                 call mio_fopen (ext_str(1), mode)
              end if
           end if
 
        end do

!       call mio_interpolation_init (num_of_infiles)

! reset mio_cfile
        mio_cfile = -1

! reads in all the output file setup in mio_file_info and stores the
! information in mio_outfile_def_info data structure
        eof = .false.
        i = 0
        stage = 0
        do while (.not. eof)

           read (mio_iunit, '(a)', iostat=stat) temp_str

           if (stat .ne. 0) then
              eof = .true.
           else
              tstr = adjustl(trim(temp_str))
              if (tstr(1:1) .ne. '#') then

                 if (stage == 0) then
                    i = i + 1
                    mio_outfile_def_info%flist(i)%new_file_info = ' '
                    ! obtain output file name and possible file open mode
                    call mio_extract_string (tstr, ext_str)

                    mio_outfile_def_info%flist(i)%fname = ext_str(1)
                    mio_outfile_def_info%flist(i)%fmode = ext_str(2)

                    stage = 1

                 else if (stage == 1) then

                    ! obtain number of output variables and possible copied from file name
                    call mio_extract_string (tstr, n, ext_str)

                    mio_outfile_def_info%flist(i)%nvars         = n    ! negative number denotes partial file
                    mio_outfile_def_info%flist(i)%copy_from     = ext_str(1)
                    mio_outfile_def_info%flist(i)%new_file_info = ext_str(2)

                    stage = 0

                    if (n < 0) then
                       n = n * (-1)
                       allocate (mio_outfile_def_info%flist(i)%vlist(n), stat=stat)

                       ! retrieve and store variable name which full information
                       ! will be obtained from a specified input file
                       j = 0
                       do while (j < n)
                          read (mio_iunit, '(a64)') temp_str

                          tvname = adjustl(trim(temp_str))
                          if (tvname(1:1) .ne. '#') then
                             j = j + 1
                             call remove_comment (tvname)
                             mio_outfile_def_info%flist(i)%vlist(j) = trim(tvname)
                          end if
                       end do
                    else if (n > 0) then
                       allocate (mio_outfile_def_info%flist(i)%vlist(n), stat=stat)
                       j = 0
                       do while (j < n)
                          read (mio_iunit, '(a256)') temp_str
                          tvname = adjustl(trim(temp_str))
                          if (tvname(1:1) .ne. '#') then
                             j = j + 1
                             call remove_comment (tvname)
                             mio_outfile_def_info%flist(i)%vlist(j) = trim(tvname)
                          end if
                       end do
                    end if
                 end if
              end if
           end if
        end do
        mio_outfile_def_info%num_of_file_definitions = i

        if ((mio_parallelism .eq. mio_true_parallel) .or.  &
            (mio_parallelism .eq. mio_pseudo)) then
           call mio_set_barrier
        end if

!       mio_n_infiles  = num_of_infiles
        mio_n_outfiles = num_of_outfiles

write (mio_logdev, '(A,4i5)') '==c== mio_init2 mio_n_infiles, mio_n_outfiles, mio_nfiles, size ', &
                                     mio_n_infiles, mio_n_outfiles, mio_nfiles, size(mio_file_data)-1

!        do i = 1, mio_n_outfiles
!           mio_file_data(mio_n_infiles + i)%filename = 'undefined'
!        end do
        do i = 1, mio_nfiles 
           write (mio_logdev, '(A,i3,2x,A)' ) '==c==', i, trim(mio_file_data(i)%filename)
        end do

        deallocate (ext_str)

      end subroutine mio_init2

! ----------------------------------------------------------------------
      subroutine remove_comment (str)

        implicit none

        character (*), intent(inout) :: str

        integer :: str_len, i, loc
        logical :: found_comment

        str_len = len_trim(str)

        i = 0
        found_comment = .false.
        do while ((i < str_len) .and. (.not. found_comment))
           i = i + 1
           if (str(i:i) == '#') then
              loc = i
              found_comment = .true.
           end if
        end do

        if (found_comment) then
           do i = loc, str_len
              str(i:i) = ' '
           end do
        end if

      end subroutine remove_comment

! ----------------------------------------------------------------------
      subroutine mio_read_dscgrid

        use mio_get_env_module
        use mio_global_data_module

        integer, parameter :: funit = 10007

        character (1000) :: lfname
        character (100)  :: grid_name, lgname, cname, line
        logical :: found, eof
        integer :: stage, stat

        call mio_get_env ( grid_name, 'GRID_NAME', ' ')

        if (grid_name == ' ') then
           mio_domain_gdtyp = -1
           mio_domain_alp   = -1.0d0
           mio_domain_bet   = -1.0d0
           mio_domain_gam   = -1.0d0
           mio_domain_xcent = -1.0d0
           mio_domain_ycent = -1.0d0
           mio_domain_xorig = -1.0d0
           mio_domain_yorig = -1.0d0
           mio_domain_xcell = -1.0d0
           mio_domain_ycell = -1.0d0
           mio_domain_ncols = -1
           mio_domain_nrows = -1
           mio_domain_nthik = -1
        else
           call mio_get_env (lfname, 'GRIDDESC', ' ')

           if (lfname == ' ') then
              write (mio_logdev, '(a37)') ' Abort: GRIDDESC file does not exist.'
              stop
           else
              open (unit = funit, file = lfname, status = 'old')
              found = .false.
              eof   = .false.
              stage = 0
              lgname = "'" // trim(grid_name) // "'"
              do while ((.not. eof) .and. (.not. found))
                 read (funit, '(a100)', iostat=stat) line
                 if (stat .ne. 0) then
                    eof = .true.
                 else
                    if (stage == 0) then
                       if (line(1:1) .ne. "'") then
                          read (line, *) mio_domain_gdtyp,                               &
                                         mio_domain_alp, mio_domain_bet, mio_domain_gam, &
                                         mio_domain_xcent, mio_domain_ycent
                          stage = 1
                       end if
                    else
                       if (line == lgname) then
                          read (funit, *) cname,                              &
                                          mio_domain_xorig, mio_domain_yorig, &
                                          mio_domain_xcell, mio_domain_ycell, &
                                          mio_domain_ncols, mio_domain_nrows, &
                                          mio_domain_nthik
                          found = .true.
                       end if
                    end if
                 end if
              end do
              close (funit)
           end if
        end if

      end subroutine mio_read_dscgrid
