      subroutine water_allocation_output (iwallo)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: iwallo        !             |
      integer :: idmd
      integer :: isrc

      !! loop through and print each demand object
      do idmd = 1, wallo(iwallo)%dmd_obs
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%src_obs
          wallom_out(iwallo)%dmd(idmd)%src(isrc) = wallom_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          wallod_out(iwallo)%dmd(idmd)%src(isrc)
        end do
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%wb_reg%d == "y") then   !!using region water balance print codes for now
          write (2510,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,                &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                        &
              (wallo(iwallo)%src(isrc), wallod_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs)  
           if (pco%csvout == "y") then
             write (2514,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                        &
              (wallo(iwallo)%src(isrc), wallod_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
           end if
        end if
       end if
       
       do isrc = 1, wallo(iwallo)%src_obs
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
       end do

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (demand, withdrawals, and unmet) for each source
          do isrc = 1, wallo(iwallo)%src_obs
            walloy_out(iwallo)%dmd(idmd)%src(isrc) = walloy_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          wallom_out(iwallo)%dmd(idmd)%src(isrc)
          end do
          
          if (pco%wb_reg%m == "y") then
          write (2511,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                      &
              (wallo(iwallo)%src(isrc), wallom_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
          if (pco%csvout == "y") then
            write (2515,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                       &
              (wallo(iwallo)%src(isrc), wallom_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
          end if
        end if
        
       do isrc = 1, wallo(iwallo)%src_obs
          wallom_out(iwallo)%dmd(idmd)%src(isrc) = walloz
       end do

        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%src_obs
          walloa_out(iwallo)%dmd(idmd)%src(isrc) = walloa_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          walloy_out(iwallo)%dmd(idmd)%src(isrc)
        end do
          
        if (pco%wb_reg%y == "y") then 
          write (2512,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                      &
              (wallo(iwallo)%src(isrc), walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
              if (pco%csvout == "y") then
           write (2516,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                      &
              (wallo(iwallo)%src(isrc), walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
          end if
        end if
        
       do isrc = 1, wallo(iwallo)%src_obs
          walloy_out(iwallo)%dmd(idmd)%src(isrc) = walloz
       end do

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%src_obs
          walloa_out(iwallo)%dmd(idmd)%src(isrc) = walloa_out(iwallo)%dmd(idmd)%src(isrc) / time%yrs_prt
        end do
        
        if (pco%wb_reg%a == "y") then
        write (2513,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,                &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                      &
              (wallo(iwallo)%src(isrc), walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
        if (pco%csvout == "y") then
          write (2517,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%dmd_src_obs,                                      &
              (wallo(iwallo)%src(isrc), walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%src_obs) 
        end if
       end if
      end if 
      
      end do    ! do idmd = 1, wallo(iwallo)%dmd_obs
      
      return

100   format (4i6,i8,5x,a,i8,5x,a,2x,i8,5x,3f15.4,5x,a,i8,3f15.4)    
      end subroutine water_allocation_output