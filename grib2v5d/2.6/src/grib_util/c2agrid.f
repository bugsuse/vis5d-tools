c 
      Subroutine c2agrid (nx, ny, ivartype, uv_in, uv_out, kret)
C
C Antonella Sanna 
C Versione tradotta in FORTRAN77 - va testata! Non sono molto sicura dell'ultimo
c If, ma vista la chiamata, mi sembra giusto.
c
c Method: 
c   <Say how it does it: include references to external documentation> 
c   <If this routine is divided into sections, be brief here,  
c        and put Method comments at the start of each section> 
c 
 
c Include statements: 
c Declarations must be of the form: 
c <type>   <VariableName>      ! Description/ purpose of variable 
 
      parameter (linmax=2)
      REAL UV_IN(NX,NY),UV_OUT(NX,NY)
      real uv_line(linmax)

      kret=0

c      if (nx.gt.linmax.or.ny.gt.linmax)then
c         kret=2
c         return
c      endif

      if (ivartype.eq.1)then
         do j=1,ny
            uv_line(1)=uv_in(1,j)-(uv_in(2,j)-uv_in(1,j))/2.
            do i=2,nx
               uv_line(mod(i-1,2)+1) = (uv_in(i-1,j) + uv_in(i,j)) / 2.
               uv_out(i-1,j) = uv_line(mod(i,2)+1)
            enddo
            uv_out(nx,j) = uv_line(mod(nx-1,2)+1)
         enddo
      elseif(ivartype.eq.2)then
         do i=1,nx
            uv_line(1)=uv_in(i,1) - (uv_in(i,2)-uv_in(i,1))/2.
            do j=2,ny
               uv_line(mod(j-1,2)+1) = (uv_in(i,j-1) + uv_in(i,j)) / 2.
               uv_out(i,j-1) = uv_line(mod(j,2)+1)
            enddo
            uv_out(i,ny) = uv_line(mod(ny-1,2)+1)
         enddo
      else
         kret=1
      endif


      return
      end

