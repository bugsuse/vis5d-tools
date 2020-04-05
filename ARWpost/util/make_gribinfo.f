
  character (len=250) :: line
  integer             :: types
  character (len=13)  :: names
  character (len=50)  :: desc
  character (len=14)  :: units
  character (len=7)   :: order
  character (len=6)   :: stag
  


  line_count = 0
  open ( 13, file = "Registry" )
  open ( 15, file = "gribinfo.txt" )

  BIGLOOP : do 

    types = 104
    names = " "
    desc  = " "
    units = " "
    order = " "
    stag  = " "
    read(13,'(a250)',END=999) line
    !! Make sure we only look at state variables in the the Registry
    IF ( line(1:5) /= "state" ) cycle BIGLOOP

      !! Make sure we only get real and integers
      iline = 6
      TYPELOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle TYPELOOP 
        IF ( line(iline:iline+4) == "real" ) THEN
          exit TYPELOOP
        END IF
        IF ( line(iline:iline+7) == "integer" ) THEN
          types = 106
          exit TYPELOOP
        END IF
      END DO TYPELOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 3 should not have a "-" - toss if it does
      C3LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == "-" ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle C3LOOP 
        IF ( line(iline:iline) /= " " ) exit C3LOOP 
      END DO C3LOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 4 
      C4LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == "-" ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle C4LOOP 
        IF ( line(iline:iline) /= " " ) THEN
          IF ( line(iline:iline) == "w" ) cycle BIGLOOP
          !!IF ( line(iline+3:iline+3) == "f" ) cycle BIGLOOP
          istart = iline
          CALL next_blank(iline,line) 
          order = line(istart:iline-1)
          IF ( order(3:3) == "b" ) order = order(1:2)
          IF ( INDEX(order,'v') /= 0 ) cycle BIGLOOP
          exit C4LOOP 
        ENDIF
      END DO C4LOOP

      !! Just read Column 5 for now
      C5LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle C5LOOP 
        IF ( line(iline:iline) /= " " ) exit C5LOOP 
      END DO C5LOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 6 - just read
      C6LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle C6LOOP 
        IF ( line(iline:iline) /= " " ) exit C6LOOP 
      END DO C6LOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 7 should have the staggering
      C7LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == " " ) cycle C7LOOP 
        IF ( line(iline:iline) /= " " ) THEN
          stag = line(iline:iline)
          exit C7LOOP 
        END IF
      END DO C7LOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 8 must have an i or h
      C8LOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
         IF ( line(iline:iline) == "\\" ) THEN
           read(13,'(a250)',END=999) line
           iline = 1
         END IF
        IF ( line(iline:iline) == '"' .OR. line(iline:iline) == '=' ) THEN
          iline = iline - 2 
          cycle BIGLOOP
        END IF
        IF ( line(iline:iline) == "i" .OR. &
             line(iline:iline) == "h" ) exit C8LOOP 
        cycle C8LOOP 
      END DO C8LOOP
      CALL next_blank(iline,line)   !! Find the next blank character

      !! Column 9 should have the name of the field
      NAMELOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == '"' ) THEN   !! found the name
          istart = iline+1
          CALL end_name(iline,line) 
          !! Make sure the name is in CAPS
          DO i= istart,iline-1
            if (line(i:i) .ge. 'a' .and. line(i:i) .le. 'z') then
              itmp = ichar(line(i:i)) - 32
              line(i:i) = achar(itmp)
            endif
            END DO
          names = line(istart:iline-1)
          exit NAMELOOP 
        ENDIF
      END DO NAMELOOP

      !! Column 10 should have the description of the field
      DESCLOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) cycle BIGLOOP
        IF ( line(iline:iline) == '"' ) THEN   !! found the name
          istart = iline+1
          CALL end_name(iline,line) 
          desc = line(istart:iline-1)
          exit DESCLOOP 
        ENDIF
      END DO DESCLOOP

      !! Column 11 should have the units of the field
      UNITLOOP : DO 
        iline = iline + 1
        IF ( iline == 250 ) THEN
          units = " " !OK not to have a unit
          exit UNITLOOP 
        END IF
        IF ( line(iline:iline) == '"' ) THEN   !! found the name
          istart = iline+1
          CALL end_name(iline,line) 
          units = line(istart:iline-1)
          exit UNITLOOP 
        ENDIF
      END DO UNITLOOP

    line_count = line_count + 1
    write (15, '(i3,5x,a7,a6,a13,a50,a14)' ) types,order,stag,names,desc,units

  end do BIGLOOP

999 print*,"   --- End of File ---   "

END


SUBROUTINE next_blank(iline,line)

  character (len=250) :: line

  BLANKLOOP : DO
    iline = iline + 1
    IF ( line(iline:iline) /= " " ) cycle BLANKLOOP
    exit BLANKLOOP
  END DO BLANKLOOP

END SUBROUTINE next_blank

SUBROUTINE end_name(iline,line)

  character (len=250) :: line

  ENDNAME : DO
    iline = iline + 1
    IF ( line(iline:iline) /= '"' ) cycle ENDNAME
    exit ENDNAME
  END DO ENDNAME

END SUBROUTINE end_name
