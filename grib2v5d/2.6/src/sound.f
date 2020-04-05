
      subroutine sound (alo11,ala11,dxhi,dyhi,tlm0d,tph0d
     +  ,nr,nc,maxnl,lev,lowlev
     +  ,data,ora,tabfth,it,typelev,imissing,ivetvar
     +  ,g,ope,ier)

c      variables are sorted by this
c      ivetvar= (/ipp,izf,itt,iqq,ivw,ivu,ivv/)

      parameter (NUSTAZ=500,MAXFILE=50)
      CHARACTER*3 CODE
      character*1 ope
      CHARACTER*80 NOMEFILE(MAXFILE),AFILE
      DIMENSION RLATSTAZ(NUSTAZ),RLONSTAZ(NUSTAZ),ISTAZ(NUSTAZ)
     +  ,NFI(NUSTAZ)
      DATA IUST,IUOT / 10,50 /
      data code/"LMM"/
      logical wind
      integer data(3),ora(2),tabfth(*),it

      save NOMEFILE,RLATSTAZ,RLONSTAZ,ISTAZ,NFI,numfile,inst

      dimension g(nr,nc,maxnl,*),sg(7)
      integer typelev(*),ivetvar(*)

      ier=0

      if (ope.eq."o")then

C----------------------------------------------------------------------
C	OPEN INPUT FILE 

        OPEN(UNIT=IUST,FILE='SOUNDSTAZ.DAT',STATUS='OLD',
     +    FORM='FORMATTED',err=91)
C----------------------------------------------------------------------

        READ (IUST,*,END=88,err=92)numfile
        IF (numfile.GT.maxfile) THEN
          PRINT*,'Error, too many files in output'
          goto 93
        ENDIF

        do i=1,numfile
          READ (IUST,'(1X,A)',END=88,err=94)nomefile(i)
        end do
      
        INST=0
      
 5      READ (IUST,*,END=88,err=95)nf,IST,RLA,RLO
        INST=INST+1
        nfi(inst)=nf
        ISTAZ(INST)=IST
        RLATSTAZ(INST)=RLA
        RLONSTAZ(INST)=RLO
        GOTO 5

 88     CLOSE (IUST)
        PRINT*,'Read ',INST,' Stations for sounding'


C     OPEN OUTPUT FILE 
        do i=1,numfile
          ilun=1
          do k=1,80
            if (NOMEFILE(I)(k:k).ne.' ')ilun=k
          end do
          WRITE (AFILE,12,err=96) NOMEFILE(I)(1:ilun),code
     +      ,DATA(3),data(2),data(1),ora(1)
 12       FORMAT (A,'SOUND',a3,'.',i4.4,3I2.2)
          OPEN(UNIT=IUOT+I,FILE=AFILE,STATUS='UNKNOWN',FORM='FORMATTED'
     +      ,err=97)
        end do
      

      else if(ope.eq."w")then
        
        
c       print *,"righe=",nr," col=",nc," liv="lev
c       print *,"data=",data," ora=",ora," scad=",tabfth(it)
        
c       interpolation of levels in layers


        do k=1,7
          if (typelev(ivetvar(k)).eq.107
     +      .OR. typelev(ivetvar(k)).eq.109
     +      .OR. typelev(ivetvar(k)).eq.119) then
            do i=lev,2,-1
              do ic=1,nc
                do ir=1,nr
                  g(ir,ic,i,ivetvar(k))=(g(ir,ic,i-1,ivetvar(k))
     +              +g(ir,ic,i,ivetvar(k)))/2.
                end do
              end do
            end do
          else if (typelev(ivetvar(k)).eq.imissing)then
            goto 103
          end if
        end do
        
        do i=1,numfile
          
          nstafile=0
          do j=1,inst
            if(nfi(j).eq.i)nstafile=nstafile+1
          end do
          write (iuot+I,14,err=98)code,DATA,ORA(1)
     +      ,it,nstafile,tabfth(it)*3600
 14       format (a3,6(1x,i4.2),1x,i6)
        end do
        
        imhv=nc
        jmhv=nr
        imjm=imhv*jmhv
        igrid=0
        ija=001   ! 32
        ls=-1

        do i=1,numfile
          do j=1,inst
            if(nfi(j).eq.i)then
              
              write (iuot+NFI(J),11,err=99)istaz(j)
     +          ,RLATSTAZ(j),RLONSTAZ(j)

              imod=1            !initialize interpolation (bilinear)

              do k=1,lev
                do m=1,6 ! not 7 becouse wind is double

                  if (m.eq.6)then
                    wind=.true.
c                    imod=1      ! is not repetition ?
                  else 
                    wind=.false.
                  end if

                  call ngetpoint (rlonstaz(j),rlatstaz(j)
     +              ,g(1,1,k,ivetvar(m))
     +              ,g(1,1,k,ivetvar(m+1))
     +              ,rlsm,imjm,imhv,jmhv
     +              ,alo11,ala11,dxhi,dyhi,igrid
     +              ,ija,tlm0d,tph0d
     +              ,wind,imod,ls,sg(m),sg(m+1)
     +              ,ierr)
                  if (ierr.ne.0) goto 100
                  imod=-1       !prepare repetition
                end do
                
                write (iuot+NFI(J),13,err=101)lev-k+1,sg(1)/100.,sg(2)
     +            ,sg(3),sg(4)*1000.,sg(6),sg(7),sg(5)
                
              end do	
 11           format (i10,2(1x,f9.4))	
 13           format (i2,1x,f6.1,1x,f6.0,1x,f6.1,1x,
     +          f6.3,1x,f6.1,1x,f6.1,1x,f6.3)	
              
              
            end if
          end do
        end do
        
      else if(ope.eq."c")then

        
	do i=1,numfile
          CLOSE (IUOT+I,err=104)
	end do
        
      else

        print*, "error function ope =",ope
        goto 102
      end if

      return

 91   ier=91
      return
 92   ier=92
      return
 93   ier=93
      return
 94   ier=94
      return
 95   ier=95
      return
 96   ier=96
      return
 97   ier=97
      return
 98   ier=98
      return
 99   ier=99
      return
 100  ier=100
      print *,"errore ngetpoint",ierr
      return
 101  ier=101
      return
 102  ier=102
      return
 103  ier=103
      return
 104  ier=104
      return

      end
