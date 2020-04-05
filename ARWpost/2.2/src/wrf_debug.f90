SUBROUTINE wrf_debug(i, msg)

   implicit none
   integer :: i
   character (len=*) msg
!!   WRITE(6,*) 'WRF_DEBUG:'//msg

END SUBROUTINE wrf_debug


SUBROUTINE wrf_message(msg)

   implicit none
   character (len=*) msg
   WRITE(6,*) 'WRF_MESSAGE:'//msg

END SUBROUTINE wrf_message


SUBROUTINE wrf_error_fatal(msg)

   implicit none
   character (len=*) msg
   WRITE(6,*) 'WRF_ERROR_FATAL:'//msg
   STOP

END SUBROUTINE wrf_error_fatal


SUBROUTINE wrf_error_fatal3(msg)

   implicit none
   character (len=*) msg
   WRITE(6,*) 'WRF_ERROR_FATAL:'//msg
   STOP

END SUBROUTINE wrf_error_fatal3
