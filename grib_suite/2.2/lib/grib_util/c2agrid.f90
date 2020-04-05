
!omstart c2agrid
!idx Interpolation from u,v points to H points for C grids 
! 
Subroutine c2agrid & 
! 
  (nx, ny, vartype, uv_in, uv_out, kret)
 
! Description: 
!   <Say what this routine does> 
! 
! Method: 
!   <Say how it does it: include references to external documentation> 
!   <If this routine is divided into sections, be brief here,  
!        and put Method comments at the start of each section> 
! 
! Current Code Owner: <Name of person responsible for this code> 
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! <version> <date>   Original code. <Your name> 
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
!
!omend 
! Declarations: 
! Modules used: 
  
!Use, Only : & 
! Imported Type Definitions: 
 
! Imported Parameters: 
 
! Imported Scalar Variables with intent (in): 
 
! Imported Scalar Variables with intent (out): 
 
! Imported Array Variables with intent (in): 
 
! Imported Array Variables with intent (out): 
 
! Imported Routines: 
 
! <Repeat from Use for each module...> 
 
Implicit None 
 
! Include statements: 
! Declarations must be of the form: 
! <type>   <VariableName>      ! Description/ purpose of variable 
 
! Subroutine arguments 
! Scalar arguments with intent(in): 

integer , intent(in)  :: nx,ny,vartype
 
! Array  arguments with intent(in): 

real , intent(in) :: uv_in(nx,ny)
 
! Scalar arguments with intent(inout): 
 
! Array  arguments with intent(inout): 
 
! Scalar arguments with intent(out): 

INTEGER , intent(out) :: kret

! Array  arguments with intent(out): 

real , intent(out) :: uv_out(nx,ny)
 
! Local parameters: 
 
! Local scalars: 
 
! Local arrays:

REAL :: uv(nx,ny) 

!- End of header -------------------------------------------------------------

kret=0

SELECT CASE (vartype)
   CASE (1)
! U point to H point
! West boundary
      uv(1,:) = uv_in(1,:) - (uv_in(2,:) - uv_in(1,:)) / 2.
! Rest of the matrix
      uv(2:nx,:) = (uv_in(1:nx-1,:) + uv_in(2:nx,:)) / 2.

      uv_out=uv

   CASE (2)
! V point to H point
! South boundary
      uv(:,1) = uv_in(:,1) - (uv_in(:,2) - uv_in(:,1)) / 2.
! Rest of the matrix
      uv(:,2:ny) = (uv_in(:,1:ny-1) + uv_in(:,2:ny)) / 2.

      uv_out=uv

   CASE DEFAULT
      kret=1

END SELECT



end Subroutine c2agrid
