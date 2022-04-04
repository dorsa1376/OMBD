! ***********************************************************************
!
!   Copyright (C) 2010-2019  Bill Paxton & The MESA Team
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
 
      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use math_lib
      
      implicit none
      
      include "test_suite_extras_def.inc"

      
      integer, parameter :: num_ages = 5
      integer :: age_cnt
      real(dp), dimension(num_ages) :: ages, loggs, Teffs
      real(dp), dimension(num_ages) :: target_loggs, target_Teffs
      real(dp) :: prev_age, prev_logg, prev_Teff
      

      contains

      include "test_suite_extras.inc"

      subroutine extras_photo_read(id, iounit, ierr)
         integer, intent(in) :: id, iounit
         integer, intent(out) :: ierr
         ierr = 0
         read(iounit) age_cnt
         read(iounit) loggs
         read(iounit) Teffs
      end subroutine extras_photo_read
      
      
      subroutine extras_photo_write(id, iounit)
         integer, intent(in) :: id, iounit
         write(iounit) age_cnt
         write(iounit) loggs
         write(iounit) Teffs
      end subroutine extras_photo_write


      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         s% other_photo_read => extras_photo_read
         s% other_photo_write => extras_photo_write
         
         s% extras_startup => extras_startup
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  
      end subroutine extras_controls
      
      
      subroutine extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         call test_suite_startup(id, restart, ierr)
         age_cnt = 0
         prev_age = 0
         ages(:) = (/ 1d-3, 1d-2, 1d-1, 1d0, 1d1 /) ! Gyrs
         target_loggs(:) = (/ 3.5156379d0, 3.9779386d0, 4.545764d0, 4.72135d0, 4.797736d0 /)
         target_Teffs(:) = (/ 2.463198d3, 2.41337242d3, 1.5603d3, 7.80458d2, 4.07689d2 /)
      end subroutine extras_startup
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
      end function extras_check_model


      integer function how_many_extra_history_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 0
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(id, n, names, vals, ierr)
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id)
         use star_def, only: star_info
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, n, nz, names, vals, ierr)
         use star_def, only: star_info, maxlen_profile_column_name
         use const_def, only: dp
         integer, intent(in) :: id, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_finish_step(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going
         call check_age(s)
         prev_age = s% star_age
         prev_logg = safe_log10(s% cgrav(1)*s% mstar/(s% r(1)*s% r(1)))
         prev_Teff = s% Teff
      end function extras_finish_step
      
      
      subroutine check_age(s)
         type (star_info), pointer :: s
         real(dp) :: next_age, alfa, logg, Teff
         include 'formats'
         if (age_cnt == num_ages) return
         next_age = ages(age_cnt+1)*1d9 ! years
         if (s% star_age < next_age) return
         age_cnt = age_cnt+1
         alfa = (next_age - prev_age)/(s% star_age - prev_age)
         logg = safe_log10(s% cgrav(1)*s% mstar/(s% r(1)*s% r(1)))
         Teff = s% Teff
         if(age_cnt == 1) then
            loggs(age_cnt) = logg
            Teffs(age_cnt) = Teff
         else
            loggs(age_cnt) = alfa*logg + (1-alfa)*prev_logg
            Teffs(age_cnt) = alfa*Teff + (1-alfa)*prev_Teff
         end if
         write(*,2) 'logg Teff', age_cnt, loggs(age_cnt), Teffs(age_cnt)
         write(*,*)
      end subroutine check_age
      
      
      subroutine extras_after_evolve(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp) :: dlogg, dTeff, tol, dt
         logical :: aok
         integer :: i
         include 'formats'
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         if (s% model_number < 20) return
         if (s% star_age < s% max_age*0.999d0) then
            write(*,1) 'failed to reach final age', s% star_age, s% max_age
            return
         end if
         call check_age(s)
         write(*,'(a6,5a20)') 'i', 'age', 'logg', 'Teff', 'dlogg', 'dTeff'
         aok = .true. 
         tol = 5d-2
         do i=1,age_cnt
            dlogg = (loggs(i) - target_loggs(i)) / target_loggs(i)
            dTeff = (Teffs(i) - target_Teffs(i)) / target_Teffs(i)
            if (abs(dlogg) > tol .or. abs(dTeff/Teffs(i)) > tol) aok = .false.
            write(*,'(i6,8x,5f20.10)') i, ages(i), loggs(i), Teffs(i), dlogg, dTeff
         end do
         write(*,*)
         if (aok) then
            write(*,'(a)') 'all values are within tolerance'
         else
            write(*,'(a)') 'FAILED -- some values too far from target'
         end if
         call test_suite_after_evolve(id, ierr)
      end subroutine extras_after_evolve

      end module run_star_extras
      
