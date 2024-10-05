! Purpose: To copy mio_file_data

      subroutine mio_copy_file_data (source, dest)

        use mio_type_def_module
        use mio_global_data_module, only : mio_file_data
        use mio_search_module

        implicit none

        type (mio_file_record), intent(inout) :: source
        type (mio_file_record), intent(out)   :: dest

        integer :: v, size, lndims, lfnvars, lnsteps, &
                   ln_global_atts, stat

        dest%filename            = source%filename
        dest%full_filename       = source%full_filename
        dest%file_format         = source%file_format
        dest%grid_type           = source%grid_type
        dest%link                = source%link
        dest%mode                = source%mode
        dest%fileid              = source%fileid
        dest%unlimited           = source%unlimited
        dest%nsteps              = source%nsteps
        dest%time_strlen_dim_loc = source%time_strlen_dim_loc
        dest%time_dim_loc        = source%time_dim_loc
        dest%layer_dim_loc       = source%layer_dim_loc
        dest%tstep               = source%tstep
        dest%gl_ncols            = source%gl_ncols
        dest%gl_nrows            = source%gl_nrows
        dest%ncols               = source%ncols
        dest%nrows               = source%nrows
        dest%nlays               = source%nlays
        dest%nbndy_cells         = source%nbndy_cells
        dest%bndy_thickness      = source%bndy_thickness
        dest%ndims               = source%ndims
        dest%nvars               = source%nvars
        dest%nbvars              = source%nbvars
        dest%fnvars              = source%fnvars
        dest%n_global_atts       = source%n_global_atts
        dest%glo_att_cval        = source%glo_att_cval
        dest%glo_att_ival        = source%glo_att_ival
        dest%glo_att_rval        = source%glo_att_rval
        dest%glo_att_dval        = source%glo_att_dval
        dest%n_vars              = source%n_vars

        lfnvars                  = source%fnvars
        lndims                   = source%ndims
        lnsteps                  = source%nsteps
        ln_global_atts           = source%n_global_atts

        if (source%mode == mio_ioapi3_format) then
           allocate (dest%tflag(2,lnsteps), stat=stat)
           dest%tflag = source%tflag
        end if

        allocate (dest%dim_name(lndims),                               &
                  dest%dim_value(lndims),                              &
                  dest%var_time_dep(lfnvars),                          &
                  dest%var_name(lfnvars),                              &
                  dest%lvar_name(lfnvars),                             &
                  dest%units(lfnvars),                                 &
                  dest%var_type(lfnvars),                              &
                  dest%var_decomp(lfnvars),                            &
                  dest%var_grid_type(lfnvars),                         &
                  dest%var_id(lfnvars),                                &
!                 stat=stat)
!       if (stat .ne. 0) then
!          write (mio_logdev, *) 'Abort in routine mio_copy_file_data due to memory allocation erro1'
!          stop
!       end if

                  dest%var_ndims(lfnvars),                             &
!       allocate (dest%var_ndims(lfnvars),                             &
                  dest%var_dimids(lndims, lfnvars),                    &
                  dest%var_dimsize(lndims, lfnvars),                   &
                  dest%num_var_att(lfnvars),                           &
                  dest%var_att_name(mio_max_num_var_att, lfnvars),     &
                  dest%var_att_len(mio_max_num_var_att, lfnvars),      &
                  dest%var_att_type(mio_max_num_var_att, lfnvars),     &
!                 stat=stat)
!       if (stat .ne. 0) then
!          write (mio_logdev, *) 'Abort in routine mio_copy_file_data due to memory allocation erro2'
!          stop
!       end if

                  dest%int_vatt_val(mio_max_num_var_att, lfnvars),     &
!       allocate (dest%int_vatt_val(mio_max_num_var_att, lfnvars),     &
                  dest%real_vatt_val(mio_max_num_var_att, lfnvars),    &
                  dest%char_vatt_val(mio_max_num_var_att, lfnvars),    &
                  dest%glo_att_name(ln_global_atts),                   &
                  dest%glo_att_type(ln_global_atts),                   &
                  dest%glo_att_len(ln_global_atts),                    &
                  dest%glo_att_crange(ln_global_atts*2),               &
                  dest%glo_att_irange(ln_global_atts*2),               &
                  dest%glo_att_rrange(ln_global_atts*2),               &
!                 stat=stat)
!       if (stat .ne. 0) then
!          write (mio_logdev, *) 'Abort in routine mio_copy_file_data due to memory allocation erro3'
!          stop
!       end if

                  dest%glo_att_drange(ln_global_atts*2),               &
!       allocate (dest%glo_att_drange(ln_global_atts*2),               &
                  dest%ncols_pe(mio_nprocs, 2),                        &
                  dest%nrows_pe(mio_nprocs, 2),                        &
                  dest%colde_pe(2, mio_nprocs, 2),                     &
                  dest%rowde_pe(2, mio_nprocs, 2),                     &
                  dest%timestamp(lnsteps),                             &
                  stat=stat)

!       if (stat .ne. 0) then
!          write (mio_logdev, *) 'Abort in routine mio_copy_file_data due to memory allocation erro4'
!          stop
!       end if

!       if (stat .ne. 0) then
!          write (mio_logdev, *) 'Abort in routine mio_copy_file_data due to memory allocation error'
!          stop
!       end if

        dest%dim_name       = source%dim_name
        dest%dim_value      = source%dim_value
        dest%var_time_dep   = source%var_time_dep
        dest%var_name       = source%var_name
        dest%lvar_name      = source%lvar_name
        dest%units          = source%units
        dest%var_type       = source%var_type
        dest%var_decomp     = source%var_decomp
        dest%var_grid_type  = source%var_grid_type
        dest%var_id         = source%var_id
        dest%var_ndims      = source%var_ndims
        dest%num_var_att    = source%num_var_att
        dest%glo_att_name   = source%glo_att_name
        dest%glo_att_type   = source%glo_att_type
        dest%glo_att_len    = source%glo_att_len
        dest%glo_att_crange = source%glo_att_crange
        dest%glo_att_irange = source%glo_att_irange
        dest%glo_att_rrange = source%glo_att_rrange
        dest%glo_att_drange = source%glo_att_drange
        dest%ncols_pe       = source%ncols_pe
        dest%nrows_pe       = source%nrows_pe
        dest%colde_pe       = source%colde_pe
        dest%rowde_pe       = source%rowde_pe
        dest%timestamp      = source%timestamp

        do v = 1, lfnvars
           lndims = source%var_ndims(v)
           size   = source%num_var_att(v)

           dest%var_dimids(1:lndims, v)  = source%var_dimids(1:lndims, v)
           dest%var_dimsize(1:lndims, v) = source%var_dimsize(1:lndims, v)

           dest%var_att_name(1:size, v)  = source%var_att_name(1:size, v)
           dest%var_att_len(1:size, v)   = source%var_att_len(1:size, v)
           dest%var_att_type(1:size, v)  = source%var_att_type(1:size, v)
           dest%int_vatt_val(1:size, v)  = source%int_vatt_val(1:size, v)
           dest%real_vatt_val(1:size, v) = source%real_vatt_val(1:size, v)
           dest%char_vatt_val(1:size, v) = source%char_vatt_val(1:size, v)
        end do

! for circular buffer ?? for the time being not sure to keep circular
! buffer or not
!       allocate (dest%cb_tstamp(?:?),                               &
!                 dest%data_index(?:?:?),                            &
!                 dest%head_loc(?),                                  &
!                 dest%tail_loc(?),                                  &
!                 dest%i_data(?),                                    &
!                 dest%r_data(?),                                    &
!                 dest%d_data(?),                                    &
!                 stat=stat)

        if (source%mode == mio_ioapi3_format) then
           deallocate (source%tflag)
        end if

        deallocate (source%dim_name,                     &
                    source%dim_value,                    &
                    source%var_time_dep,                 &
                    source%var_name,                     &
                    source%lvar_name,                    &
                    source%units,                        &
                    source%var_type,                     &
                    source%var_decomp,                   &
                    source%var_grid_type,                &
                    source%var_id,                       &
                    source%var_ndims,                    &
                    source%var_dimids,                   &
                    source%var_dimsize,                  &
                    source%num_var_att,                  &
                    source%var_att_name,                 &
                    source%var_att_len,                  &
                    source%var_att_type,                 &
                    source%int_vatt_val,                 &
                    source%real_vatt_val,                &
                    source%char_vatt_val,                &
                    source%glo_att_name,                 &
                    source%glo_att_type,                 &
                    source%glo_att_len,                  &
                    source%glo_att_crange,               &
                    source%glo_att_irange,               &
                    source%glo_att_rrange,               &
                    source%glo_att_drange,               &
                    source%ncols_pe,                     &
                    source%nrows_pe,                     &
                    source%colde_pe,                     &
                    source%rowde_pe,                     &
                    source%timestamp)

      end subroutine mio_copy_file_data
