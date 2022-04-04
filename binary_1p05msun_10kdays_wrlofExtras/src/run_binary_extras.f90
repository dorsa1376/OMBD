! ***********************************************************************
!
!   Copyright (C) 2012-2019  Bill Paxton & The MESA Team
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! *********************************************************************** 
      module run_binary_extras 

      use star_lib
      use star_def
      use const_def
      use const_def
      use chem_def
      use num_lib
      use binary_def
      use math_lib
      
      implicit none
      
      contains
      
      subroutine extras_binary_controls(binary_id, ierr)
         integer :: binary_id
         integer, intent(out) :: ierr
         type (binary_info), pointer :: b
         ierr = 0

         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in binary_ptr'
            return
         end if
         !write(*,*) 'hello from extra_binary_controls'
         b% other_binary_wind_transfer => WRLOF_wind_transfer 
         ! Set these function pointers to point to the functions you wish to use in
         ! your run_binary_extras. Any which are not set, default to a null_ version
         ! which does nothing.
         b% how_many_extra_binary_history_header_items => how_many_extra_binary_history_header_items
         b% data_for_extra_binary_history_header_items => data_for_extra_binary_history_header_items
         b% how_many_extra_binary_history_columns => how_many_extra_binary_history_columns
         b% data_for_extra_binary_history_columns => data_for_extra_binary_history_columns

         b% extras_binary_startup=> extras_binary_startup
         b% extras_binary_start_step=> extras_binary_start_step
         b% extras_binary_check_model=> extras_binary_check_model
         b% extras_binary_finish_step => extras_binary_finish_step
         b% extras_binary_after_evolve=> extras_binary_after_evolve

         ! Once you have set the function pointers you want, then uncomment this (or set it in your star_job inlist)
         ! to disable the printed warning message,
          b% warn_binary_extra =.false.
         
      end subroutine extras_binary_controls


      integer function how_many_extra_binary_history_header_items(binary_id)
         use binary_def, only: binary_info
         integer, intent(in) :: binary_id
         how_many_extra_binary_history_header_items = 0
      end function how_many_extra_binary_history_header_items


      subroutine data_for_extra_binary_history_header_items( &
           binary_id, n, names, vals, ierr)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id, n
         character (len=maxlen_binary_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         ierr = 0
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in binary_ptr'
            return
         end if
      end subroutine data_for_extra_binary_history_header_items


      integer function how_many_extra_binary_history_columns(binary_id)
         use binary_def, only: binary_info
         integer, intent(in) :: binary_id
         how_many_extra_binary_history_columns = 0
      end function how_many_extra_binary_history_columns


      subroutine data_for_extra_binary_history_columns(binary_id, n, names, vals, ierr)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer, intent(in) :: n
         character (len=maxlen_binary_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         ierr = 0
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in binary_ptr'
            return
         end if
         
      end subroutine data_for_extra_binary_history_columns
      
      
      integer function extras_binary_startup(binary_id,restart,ierr)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer, intent(out) :: ierr
         logical, intent(in) :: restart
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then ! failure in  binary_ptr
            return
         end if
         
!          b% s1% job% warn_run_star_extras = .false.
          extras_binary_startup = keep_going
      end function  extras_binary_startup
      
      integer function extras_binary_start_step(binary_id,ierr)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer, intent(out) :: ierr

         extras_binary_start_step = keep_going
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then ! failure in  binary_ptr
            return
         end if
      
      end function  extras_binary_start_step
      
      !Return either keep_going, retry or terminate
      integer function extras_binary_check_model(binary_id)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer :: ierr
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then ! failure in  binary_ptr
            return
         end if  
         extras_binary_check_model = keep_going
        
      end function extras_binary_check_model
      
      
      ! returns either keep_going or terminate.
      ! note: cannot request retry; extras_check_model can do that.
      integer function extras_binary_finish_step(binary_id)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer :: ierr
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then ! failure in  binary_ptr
            return
         end if  
         extras_binary_finish_step = keep_going
         
      end function extras_binary_finish_step
      
      subroutine extras_binary_after_evolve(binary_id, ierr)
         type (binary_info), pointer :: b
         integer, intent(in) :: binary_id
         integer, intent(out) :: ierr
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then ! failure in  binary_ptr
            return
         end if      
         
 
      end subroutine extras_binary_after_evolve     

      subroutine WRLOF_wind_transfer(binary_id, s_i, ierr)
        !use binary_wind, only: Bondi_Hoyle_wind_transfer
        integer, intent(in) :: binary_id, s_i
        integer, intent(out) :: ierr
        real(dp) :: r_dust, x, q2, c1, c2, c3, abate_xfer
        type (binary_info), pointer :: b
        !WRLOF implemented by Abate et al 2013
        ierr = 0
        call binary_ptr(binary_id, b, ierr)
        if (ierr /= 0) then
           write(*,*) 'failed in binary_ptr'
           return
        end if

        r_dust = 0.5d0 * b% r(s_i) * (b% s_donor % Teff / 1000d0)**2.5d0
        x = r_dust / b% rl(s_i)

        ! constants from Abate et al eq. 5
        q2 = ( b% m(3-s_i)/b% m(s_i) )**2d0
        c1 = -0.284d0
        c2 = 0.918d0
        c3 = -0.234d0


        abate_xfer  = 25d0 / 9d0 * q2* (c1*x**2 + c2*x + c3)
        abate_xfer = max(abate_xfer, 0d0)
        abate_xfer = min(abate_xfer, 5d-1)

        call Bondi_Hoyle_wind_transfer(b% binary_id, s_i, ierr)
        !if (b% r(s_i) .gt. 10.0d0) then
        !write(*,*) 'dbg', abate_xfer, q2, b% wind_xfer_fraction(s_i)
        !end if
        b% wind_xfer_fraction(s_i) = max(b% wind_xfer_fraction(s_i), &
            abate_xfer)

      end subroutine WRLOF_wind_transfer
        
      ! copied from $MESA_DIR/binary/private/binary_wind.f90
      subroutine Bondi_Hoyle_wind_transfer(binary_id,s_i, ierr)
         integer, intent(in) :: binary_id, s_i ! s_i is index of the wind mass losing star
         integer, intent(out) :: ierr
         ! wind transfer fraction based on Bondi-Hoyle mechanism as described in
         ! Hurley et al. 2002, MNRAS, 329, 897-928
         type(binary_info), pointer :: b
         type (star_info), pointer :: s
         real(dp) :: v_orb, v_wind, b_BH
         real(dp) :: alpha  ! Bondi-Hoyle alpha for that star
         real(dp) :: max_xfer  ! Maximum transfer fraction
         call binary_ptr(binary_id, b, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in binary_ptr'
            return
         end if
                  
         if (s_i == 1) then
            s => b% s1
            alpha = b% wind_BH_alpha_1
            max_xfer = b% max_wind_transfer_fraction_1
         else
            s => b% s2
            alpha = b% wind_BH_alpha_2
            max_xfer = b% max_wind_transfer_fraction_2
         end if
                  
         ! orbital speed Hurley et al 2002 eq. 8
         v_orb = sqrt(standard_cgrav * b% m(s_i) / b% separation) !cm/s
                  
         ! windspeed from Hurley et al 2002 eq. 9
         v_wind = sqrt( 2d0 / 8d0 *  standard_cgrav * &
            b% m(s_i) / b% r(s_i) )
                  
         ! Bondi-Hoyle transfer fraction Hurley et al.  2002 eq. 6
         b% wind_xfer_fraction(s_i) = alpha / pow2(b% separation) /&
                (2d0 * sqrt(1d0 - pow2(b% eccentricity))) *&
                pow2(standard_cgrav * b% m(3-s_i) / pow2(v_wind)) *&
                pow(1d0 + pow2(v_orb/v_wind),-1.5d0)
                  
         ! limit to provided maximum
         b% wind_xfer_fraction(s_i) = min(max_xfer, &
            b% wind_xfer_fraction(s_i))
                  
      end subroutine Bondi_Hoyle_wind_transfer

      
      end module run_binary_extras
