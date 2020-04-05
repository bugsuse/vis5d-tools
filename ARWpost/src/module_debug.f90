MODULE module_debug

   integer, parameter :: STDOUT=-1, DEBUG=0, INFORM=1, WARN=2, ERROR=3

   CONTAINS

   SUBROUTINE mprintf(assertion, level, fmtstring, i1, i2, i3, f1, f2, f3, s1, s2, s3)

      implicit none

      ! Arguments
      integer, intent(in)                     :: level
      logical, intent(in)                     :: assertion
      character (len=*), intent(in)           :: fmtstring
      integer, intent(in), optional           :: i1, i2, i3
      real, intent(in), optional              :: f1, f2, f3
      character (len=*), intent(in), optional :: s1, s2, s3

      ! Local variables 
      integer                                 :: idxi, idxf, idxs, istart, i, iend, ia
      real                                    :: fa
      character (len=128)                     :: sa

      idxi = 1
      idxf = 1
      idxs = 1
      istart = 1
      iend = len_trim(fmtstring)

      IF (assertion) THEN

         IF (level == DEBUG) THEN
            WRITE(6,'(a)',advance='no') 'DEBUG: '
         ELSE IF (level == INFORM) THEN
            WRITE(6,'(a)',advance='no') 'INFORM: '
         ELSE IF (level == WARN) THEN
            WRITE(6,'(a)',advance='no') 'WARNING: '
         ELSE IF (level == ERROR) THEN
            WRITE(6,'(a)',advance='no') 'ERROR: '
         END IF
      
         i = index(fmtstring(istart:iend),'%')
         DO WHILE (i > 0 .and. i < iend)
            i = i + istart - 1
            WRITE(6,'(a)',advance='no') fmtstring(istart:i-1)
   
            IF (fmtstring(i+1:i+1) == '%') THEN
               WRITE(6,'(a1)',advance='no') '%'
                            
            ELSE IF (fmtstring(i+1:i+1) == 'i') THEN
               IF (idxi == 1 .and. present(i1)) THEN
                  ia = i1
               ELSE IF (idxi == 2 .and. present(i2)) THEN
                  ia = i2
               ELSE IF (idxi == 3 .and. present(i3)) THEN
                  ia = i3
               END IF
   
               IF (ia < 10) THEN
                  WRITE(6,'(i1)',advance='no') ia
               ELSE IF (ia < 100) THEN
                  WRITE(6,'(i2)',advance='no') ia
               ELSE IF (ia < 1000) THEN
                  WRITE(6,'(i3)',advance='no') ia
               ELSE IF (ia < 10000) THEN
                  WRITE(6,'(i4)',advance='no') ia
               ELSE IF (ia < 100000) THEN
                  WRITE(6,'(i5)',advance='no') ia
               ELSE
                  WRITE(6,'(i9)',advance='no') ia
               END IF
               idxi = idxi + 1
   
            ELSE IF (fmtstring(i+1:i+1) == 'f') THEN
               IF (idxf == 1 .and. present(f1)) THEN
                  fa = f1
               ELSE IF (idxf == 2 .and. present(f2)) THEN
                  fa = f2
               ELSE IF (idxf == 3 .and. present(f3)) THEN
                  fa = f3
               END IF
   
               IF (fa < -100000.) THEN
                  WRITE(6,'(f12.4)',advance='no') fa
               ELSE IF (fa < -10000.) THEN
                  WRITE(6,'(f11.4)',advance='no') fa
               ELSE IF (fa < -1000.) THEN
                  WRITE(6,'(f10.4)',advance='no') fa
               ELSE IF (fa < -100.) THEN
                  WRITE(6,'(f9.4)',advance='no') fa
               ELSE IF (fa < -10.) THEN
                  WRITE(6,'(f8.4)',advance='no') fa
               ELSE IF (fa < 0.) THEN
                  WRITE(6,'(f7.4)',advance='no') fa
               ELSE IF (fa < 10.) THEN
                  WRITE(6,'(f6.4)',advance='no') fa
               ELSE IF (fa < 100.) THEN
                  WRITE(6,'(f7.4)',advance='no') fa
               ELSE IF (fa < 1000.) THEN
                  WRITE(6,'(f8.4)',advance='no') fa
               ELSE IF (fa < 10000.) THEN
                  WRITE(6,'(f9.4)',advance='no') fa
               ELSE IF (fa < 100000.) THEN
                  WRITE(6,'(f10.4)',advance='no') fa
               ELSE
                  WRITE(6,'(f15.4)',advance='no') fa
               END IF
               idxf = idxf + 1
   
            ELSE IF (fmtstring(i+1:i+1) == 's') THEN
               IF (idxs == 1 .and. present(s1)) THEN
                  sa = s1
               ELSE IF (idxs == 2 .and. present(s2)) THEN
                  sa = s2
               ELSE IF (idxs == 3 .and. present(s3)) THEN
                  sa = s3
               END IF
   
               WRITE(6,'(a)',advance='no') trim(sa)
               idxs = idxs + 1
   
            END IF
   
            istart = i+2
            i = index(fmtstring(istart:iend),'%')
         END do
   
         WRITE(6,'(a)') fmtstring(istart:iend)

         IF (level == ERROR) stop

      END IF


   END SUBROUTINE mprintf

END MODULE module_debug
