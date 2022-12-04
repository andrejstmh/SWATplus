      subroutine proc_open

      implicit none

      !! write headers in output files
      call output_landscape_init
      call header_channel
      call header_aquifer
      call header_sd_channel
      call header_mgt
      call header_lu_change
      call header_yield
      call header_hyd
      call header_reservoir
      call header_wetland
      call header_snutc
      call header_water_allocation
      
       call header_pest
       call header_path
      !call header_cs

      call header_write
     ! debug output file      
      !open(7743, file="test.txt",recl=800)
           
	  return
      
      end subroutine proc_open