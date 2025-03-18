! Purpose: To copy file information such as dimension names, variable
! dimensions, variable attributes and global attributes, partially or
! wholly from source file to destination file

      subroutine mio_duplicate_file (source, dest)

        use mio_type_def_module
        use mio_global_data_module, only : mio_file_data
        use mio_search_module

        implicit none

        type (mio_file_record), intent(in)  :: source
        type (mio_file_record), intent(out) :: dest

        integer :: t, v, loc, reg_var, basic_var,  &
                   n, s, e, s1, e1, s2, e2, nvars
        character (mio_max_vlist_len) :: vlist

        reg_var = 0
        basic_var = 0

        do v = 1, dest%fnvars

           t = mio_search(source%lvar_name(v),                &
                          mio_basic_mpas_vnames_l,            &
                          mio_n_basic_mpas_vars)

! reorder the variable base on pre-defined sequence: regular variable, basic variable, and time step
           if (t .gt. 0) then
              basic_var = basic_var + 1
              loc       = basic_var + source%nvars
           else if ((source%var_name(v) .eq. 'Times') .or.   &
                    (source%var_name(v) .eq. 'TFLAG') .or.   &
                    (source%var_name(v) .eq. 'xtime')) then
              loc = dest%fnvars
           else
              reg_var = reg_var + 1
              loc     = reg_var
           end if

           dest%var_name(loc)        = source%var_name(v)
           dest%lvar_name(loc)       = source%lvar_name(v)
           dest%units(loc)           = source%units(v)
           dest%var_type(loc)        = source%var_type(v)
           dest%var_decomp(loc)      = source%var_decomp(v)
           dest%var_grid_type(loc)   = source%var_grid_type(v)
           dest%var_time_dep(loc)    = source%var_time_dep(v)
           dest%var_ndims(loc)       = source%var_ndims(v)
           dest%var_dimids(:,loc)    = source%var_dimids(:,v)
           dest%var_dimsize(:,loc)   = source%var_dimsize(:,v)
           dest%num_var_att(loc)     = source%num_var_att(v)
           dest%var_att_name(:,loc)  = source%var_att_name(:,v)
           dest%var_att_len(:,loc)   = source%var_att_len(:,v)
           dest%var_att_type(:,loc)  = source%var_att_type(:,v)
           dest%int_vatt_val(:,loc)  = source%int_vatt_val(:,v)
           dest%real_vatt_val(:,loc) = source%real_vatt_val(:,v)
           dest%char_vatt_val(:,loc) = source%char_vatt_val(:,v)

        end do

! transfer global attribute information
        dest%time_dim_loc          = source%time_dim_loc
        dest%layer_dim_loc         = source%layer_dim_loc
        dest%n_global_atts         = source%n_global_atts

        n                          = dest%n_global_atts
        dest%glo_att_len           = source%glo_att_len
        dest%glo_att_name(1:n)     = source%glo_att_name(1:n)
        dest%glo_att_type(1:n)     = source%glo_att_type(1:n)
        dest%glo_att_irange(1:2*n) = source%glo_att_irange(1:n*2)
        dest%glo_att_rrange(1:2*n) = source%glo_att_rrange(1:n*2)
        dest%glo_att_drange(1:2*n) = source%glo_att_drange(1:n*2)
        dest%glo_att_crange        = source%glo_att_crange
        dest%glo_att_ival          = source%glo_att_ival
        dest%glo_att_rval          = source%glo_att_rval
        dest%glo_att_dval          = source%glo_att_dval
        dest%glo_att_cval          = source%glo_att_cval

        if (source%file_format .eq. mio_ioapi3_format) then
           vlist = ' '
           nvars = dest%nvars
           s = 1
           e = 16
           do n = 1, nvars
              write (vlist(s:e), '(a16)') dest%var_name(n)
              s = s + 16
              e = e + 16
           end do

           s2 = 0
           e2 = 0
           do n = 1, source%n_global_atts
              if (dest%glo_att_type(n) .eq. nf90_char) then
                 if (n .eq. 31) then   ! VAR-LIST
                    s2 = e2 + 1
!                   e2 = s2 + nvars * 16
                    e2 = s2 + nvars * 16 - 1
                    dest%glo_att_cval(s2:e2) = vlist(1:nvars*16)
                    dest%glo_att_len(1:n)    = nvars * 16
                 else
                    s1 = source%glo_att_crange((n-1)*2+1)
                    e1 = source%glo_att_crange(2*n)
                    s2 = e2 + 1
                    e2 = s2 + (e1 - s1)
                    dest%glo_att_cval(s2:e2) = source%glo_att_cval(s1:e1)
                 end if
                 dest%glo_att_crange((n-1)*2+1) = s2
                 dest%glo_att_crange(2*n)       = e2
                 dest%glo_att_len(1:n)    = source%glo_att_len(1:n)
              else if (n .eq. 15) then    ! NVARS
                 s1 = source%glo_att_irange((n-1)*2+1)
                 e1 = source%glo_att_irange(2*n)
                 dest%glo_att_ival(s1:e1) = nvars
              end if
           end do

           dest%gl_ncols = dest%dim_value(6)
           dest%gl_nrows = dest%dim_value(5)
           dest%nlays    = dest%dim_value(3)

        else if ((source%file_format .eq. mio_wrf_format) .or.   &
                 (source%file_format .eq. mio_mpas_format)) then
           dest%glo_att_crange(1:2*n) = source%glo_att_crange(1:n*2)
           dest%glo_att_cval          = source%glo_att_cval
           if (source%file_format .eq. mio_mpas_format) then
              n = mio_search ('nCells', dest%dim_name, dest%ndims)
              dest%gl_ncols = dest%dim_value(n)
              n = mio_search ('nVertLevels', dest%dim_name, dest%ndims)
              if (n > 0) then
                 dest%nlays = dest%dim_value(n)
              else
                 dest%nlays = 1
              end if
              dest%gl_nrows = 1
           else
              n = mio_search ('west_east', dest%dim_name, dest%ndims)
              dest%gl_ncols = dest%dim_value(n)
              n = mio_search ('bottom_top', dest%dim_name, dest%ndims)
              if (n < 0) then
                 dest%nlays = 1
              else
                 dest%nlays = dest%dim_value(n)
              end if
              n = mio_search ('south_north', dest%dim_name, dest%ndims)
              dest%gl_nrows = dest%dim_value(n)
           end if
        end if

      end subroutine mio_duplicate_file

! -------------------------------------------------------------------------------------
      subroutine mio_duplicate_partial_file (source, dest, nvars)

        use mio_type_def_module
        use mio_global_data_module, only : mio_file_data,         &
                                           mio_outfile_def_info
        use mio_search_module
        use mio_parameter_module, only: mio_max_vlist_len

        implicit none

!       integer, intent(in) :: source, dest, nvars
        integer, intent(in) :: nvars
        type (mio_file_record), intent(in)  :: source
        type (mio_file_record), intent(out) :: dest

        integer :: v, t, s, e, s1, e1, s2, e2, n, loc, fnum
        character (mio_max_str_len) :: t_vname
        character (mio_max_vlist_len) :: vlist
        logical :: found_mpas_basic_variable, found

        found = .false.
        fnum = 0
        do while ((.not. found) .and. (fnum < mio_outfile_def_info%num_of_file_definitions))
           fnum = fnum + 1
           if (dest%filename .eq. mio_outfile_def_info%flist(fnum)%fname) then
              found = .true.
           end if
        end do

        do v = 1, nvars

           t_vname = mio_outfile_def_info%flist(fnum)%vlist(v)

           t = mio_search(t_vname, source%var_name, source%fnvars)

! place the variable base on pre-defined sequence: regular variable, basic variable, and time step
           dest%var_name(v)        = source%var_name(t)
           dest%lvar_name(v)       = source%lvar_name(t)
           dest%units(v)           = source%units(t)
           dest%var_type(v)        = source%var_type(t)
           dest%var_decomp(v)      = source%var_decomp(t)
           dest%var_grid_type(v)   = source%var_grid_type(t)
           dest%var_time_dep(v)    = source%var_time_dep(t)
           dest%var_ndims(v)       = source%var_ndims(t)
           dest%var_dimids(:,v)    = source%var_dimids(:,t)
           dest%var_dimsize(:,v)   = source%var_dimsize(:,t)
           dest%num_var_att(v)     = source%num_var_att(t)
           dest%var_att_name(:,v)  = source%var_att_name(:,t)
           dest%var_att_len(:,v)   = source%var_att_len(:,t)
           dest%var_att_type(:,v)  = source%var_att_type(:,t)
           dest%int_vatt_val(:,v)  = source%int_vatt_val(:,t)
           dest%real_vatt_val(:,v) = source%real_vatt_val(:,t)
           dest%char_vatt_val(:,v) = source%char_vatt_val(:,t)

        end do

        found_mpas_basic_variable = .true.
        v = 0
        do while ((found_mpas_basic_variable) .and. (v .lt. source%nbvars))
           v = v + 1

           t = mio_search(mio_basic_mpas_vnames_l(v),       &
                          source%lvar_name,  &
                          source%fnvars)

           if (t .gt. 0) then
              loc = nvars + v
              dest%var_name(loc)        = source%var_name(t)
              dest%lvar_name(loc)       = source%lvar_name(t)
              dest%units(loc)           = source%units(t)
              dest%units(loc)           = source%units(t)
              dest%var_type(loc)        = source%var_type(t)
              dest%var_decomp(loc)      = source%var_decomp(t)
              dest%var_grid_type(loc)   = source%var_grid_type(t)
              dest%var_time_dep(loc)    = source%var_time_dep(t)
              dest%var_ndims(loc)       = source%var_ndims(t)
              dest%var_dimids(:,loc)    = source%var_dimids(:,t)
              dest%var_dimsize(:,loc)   = source%var_dimsize(:,t)
              dest%num_var_att(loc)     = source%num_var_att(t)
              dest%var_att_name(:,loc)  = source%var_att_name(:,t)
              dest%var_att_len(:,loc)   = source%var_att_len(:,t)
              dest%var_att_type(:,loc)  = source%var_att_type(:,t)
              dest%int_vatt_val(:,loc)  = source%int_vatt_val(:,t)
              dest%real_vatt_val(:,loc) = source%real_vatt_val(:,t)
              dest%char_vatt_val(:,loc) = source%char_vatt_val(:,t)
           else
              found_mpas_basic_variable = .false.
           end if

        end do

        if (source%file_format .eq. mio_ioapi3_format) then
           t_vname = 'TFLAG'
        else if (source%file_format .eq. mio_wrf_format) then
           t_vname = 'Times'
        else if (source%file_format .eq. mio_mpas_format) then
           t_vname = 'xtime'
        end if

        t = mio_search(t_vname,                         &
                       source%var_name,  &
                       source%fnvars)

        if (found_mpas_basic_variable) then
           loc = nvars + source%nbvars + 1
        else
           loc = nvars + 1
        end if

        dest%var_name(loc)        = source%var_name(t)
        dest%lvar_name(loc)       = source%lvar_name(t)
        dest%units(loc)           = source%units(t)
        dest%var_type(loc)        = source%var_type(t)
        dest%var_decomp(loc)      = source%var_decomp(t)
        dest%var_grid_type(loc)   = source%var_grid_type(t)
        dest%var_time_dep(loc)    = source%var_time_dep(t)
        dest%var_ndims(loc)       = source%var_ndims(t)
        dest%var_dimids(:,loc)    = source%var_dimids(:,t)
        dest%var_dimsize(:,loc)   = source%var_dimsize(:,t)
        dest%num_var_att(loc)     = source%num_var_att(t)
        dest%var_att_name(:,loc)  = source%var_att_name(:,t)
        dest%var_att_len(:,loc)   = source%var_att_len(:,t)
        dest%var_att_type(:,loc)  = source%var_att_type(:,t)
        dest%int_vatt_val(:,loc)  = source%int_vatt_val(:,t)
        dest%real_vatt_val(:,loc) = source%real_vatt_val(:,t)
        dest%char_vatt_val(:,loc) = source%char_vatt_val(:,t)

! transfer global attribute information
        dest%time_dim_loc          = source%time_dim_loc
        dest%layer_dim_loc         = source%layer_dim_loc
        dest%n_global_atts         = source%n_global_atts

        n                                         = dest%n_global_atts
        dest%glo_att_len           = source%glo_att_len
        dest%glo_att_name(1:n)     = source%glo_att_name(1:n)
        dest%glo_att_type(1:n)     = source%glo_att_type(1:n)
        dest%glo_att_irange(1:2*n) = source%glo_att_irange(1:n*2)
        dest%glo_att_rrange(1:2*n) = source%glo_att_rrange(1:n*2)
        dest%glo_att_drange(1:2*n) = source%glo_att_drange(1:n*2)
        dest%glo_att_crange        = source%glo_att_crange
        dest%glo_att_ival          = source%glo_att_ival
        dest%glo_att_rval          = source%glo_att_rval
        dest%glo_att_dval          = source%glo_att_dval
        dest%glo_att_cval          = source%glo_att_cval

        if (source%file_format .eq. mio_ioapi3_format) then
           vlist = ' '
           s = 1
           e = 16
           do n = 1, dest%nvars
              write (vlist(s:e), '(a16)') dest%var_name(n)
              s = s + 16
              e = e + 16
           end do

           s2 = 0
           e2 = 0
           do n = 1, source%n_global_atts
              if (dest%glo_att_type(n) .eq. nf90_char) then
                 if (n .eq. 31) then   ! VAR-LIST
                    s2 = e2 + 1
!                   e2 = s2 + dest%nvars * 16
                    e2 = s2 + dest%nvars * 16 - 1
                    dest%glo_att_cval(s2:e2) = vlist(1:dest%nvars*16)
                    dest%glo_att_len(1:n)    = dest%nvars * 16
                 else
                    s1 = source%glo_att_crange((n-1)*2+1)
                    e1 = source%glo_att_crange(2*n)
                    s2 = e2 + 1
                    e2 = s2 + (e1 - s1)
                    dest%glo_att_cval(s2:e2) = source%glo_att_cval(s1:e1)
                 end if
                 dest%glo_att_crange((n-1)*2+1) = s2
                 dest%glo_att_crange(2*n)       = e2
                 dest%glo_att_len(1:n)    = source%glo_att_len(1:n)
              else if (n .eq. 15) then    ! NVARS
                 s1 = source%glo_att_irange((n-1)*2+1)
                 e1 = source%glo_att_irange(2*n)
                 dest%glo_att_ival(s1:e1) = dest%nvars
              end if
           end do

           dest%gl_ncols = dest%dim_value(6)
           dest%gl_nrows = dest%dim_value(5)
           dest%nlays    = dest%dim_value(3)

        else if ((source%file_format .eq. mio_wrf_format) .or.   &
                 (source%file_format .eq. mio_mpas_format)) then
           dest%glo_att_crange(1:2*n) = source%glo_att_crange(1:n*2)
           dest%glo_att_cval          = source%glo_att_cval
           if (source%file_format .eq. mio_mpas_format) then
              n = mio_search ('nCells', dest%dim_name, dest%ndims)
              dest%gl_ncols = dest%dim_value(n)
              n = mio_search ('nVertLevels', dest%dim_name, dest%ndims)
              dest%nlays = dest%dim_value(n)
              dest%gl_nrows = 1
           else
              n = mio_search ('west_east', dest%dim_name, dest%ndims)
              dest%gl_ncols = dest%dim_value(n)
              n = mio_search ('bottom_top', dest%dim_name, dest%ndims)
              if (n < 0) then
                 dest%nlays = 1
              else
                 dest%nlays = dest%dim_value(n)
              end if
              n = mio_search ('south_north', dest%dim_name, dest%ndims)
              dest%gl_nrows = dest%dim_value(n)
           end if
        end if

      end subroutine mio_duplicate_partial_file
