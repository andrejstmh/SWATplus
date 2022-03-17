      subroutine wallo_control (iwallo)
      
      use water_allocation_module
      
      implicit none 

      integer, intent (inout) :: iwallo     !water allocation object number
      integer :: idmd                       !water demand object number
      integer :: isrc                       !source object number
      real :: dmd_m3                        !m3     |demand

      do idmd = 1, wallo(iwallo)%dmd_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%src_obs
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do
  
        !! set demand for each object
        call wallo_demand (iwallo, idmd)
 
        !! check if water is available from each source - set withdrawal and unmet
        do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
          dmd_m3 = wallo(iwallo)%dmd(idmd)%src(isrc)%frac * wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand
          if (dmd_m3 > 1.e-6) then
            call wallo_withdraw (iwallo, idmd, isrc, dmd_m3)
          end if
        end do
        
        !! loop through sources again to check if compensation is allowed
        do isrc = 1, wallo(iwallo)%dmd(idmd)%dmd_src_obs
          if (wallo(iwallo)%dmd(idmd)%src(isrc)%comp == "y") then
            dmd_m3 = wallo(iwallo)%dmd(idmd)%unmet_m3
            if (dmd_m3 > 1.e-6) then
              call wallo_withdraw (iwallo, idmd, isrc, dmd_m3)
            end if
          end  if
        end do
      end do  
        
      return
      end subroutine wallo_control