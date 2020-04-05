      program testdriver
      dimension array(10,10,10),scratch(10,10,10)
      character*4 visfile
      visfile='VIS.'
      call viswrite(2,array,scratch,10,10,10,visfile
     &             ,45.0,-90.0,1.0,1.0,10000.,1000.
     &             ,1993,7,12,12,30,01)
      call viswrite(2,array,scratch,10,10,10,visfile
     &             ,45.0,-90.0,1.0,1.0,10000.,1000.
     &             ,1993,7,12,15,30,01)
      end
