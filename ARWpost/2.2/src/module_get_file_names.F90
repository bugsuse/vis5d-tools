MODULE module_get_file_names

!  To allow for multiple input files, we use a couple of UNIX
!  commands.  These are activated from either the "system" command or 
!  the "exec" command.  Neither is part of the Fortran standard.

   INTEGER                                       :: number_of_input_files
   CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE :: input_file_names

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef crayx1
   SUBROUTINE system(cmd)
      IMPLICIT NONE
      CHARACTER (LEN=*) , INTENT(IN) :: cmd
      integer                        :: ierr
      CALL pxfsystem(cmd, len(cmd), ierr)
   RETURN
   END SUBROUTINE system
#endif

   SUBROUTINE unix_ls ( root )

      USE module_debug

      IMPLICIT NONE
     
      CHARACTER (LEN=*), INTENT(IN) :: root
      CHARACTER (LEN=132)           :: command
      INTEGER                       :: ierr , loop , loslen , strlen, try


      try = 0

      !  Build a UNIX command, and "ls", of all of the files mnatching the "root*" prefix.

         loslen = LEN ( command )
         CALL all_spaces ( command , loslen ) 
   33    continue
         IF ( try == 0 ) WRITE ( command , FMT='("ls -1 ",A," > .foo")' ) TRIM ( root )
         IF ( try == 1 ) WRITE ( command , FMT='("ls -1 ",A,"* > .foo")' ) TRIM ( root )
         
         !  We stuck all of the matching files in the ".foo" file.  Now we place the 
         !  number of the those file (i.e. how many there are) in ".foo1".  Also, if we
         !  do get inside one of these CPP ifdefs, then we set our access flag to true.

         CALL SYSTEM ( TRIM ( command ) ) 
         CALL SYSTEM ( '( cat .foo | wc -l > .foo1 )' )

         !  Read the number of files.

         OPEN (FILE   = '.foo1'       , &
               UNIT   = 112           , &
               STATUS = 'OLD'         , &
               ACCESS = 'SEQUENTIAL'  , &
               FORM   = 'FORMATTED'     )

         READ ( 112 , * ) number_of_input_files
         CLOSE ( 112 )

         !  If there are zero files, we are toast.

         IF ( number_of_input_files .LE. 0 .AND. try == 0 ) THEN
            !! Let's see if we can get to these files a different way
            try = 1
            GOTO 33
         ELSEIF ( number_of_input_files .LE. 0 .AND. try == 1 ) THEN
            CALL mprintf(.true.,STDOUT, ' Oops, we need at least ONE input file for the program to read.')
            STOP
         END IF

      !  Allocate space for this many files.

      ALLOCATE ( input_file_names(number_of_input_files) , STAT=ierr )

      !  Did the allocate work OK?

      IF ( ierr .NE. 0 ) THEN
         CALL mprintf(.true.,STDOUT, ' tried to allocate %i  input files, (look at ./foo)', i1=number_of_input_files)
         STOP
      END IF

      !  Initialize all of the file names to blank.

      CALL init_module_get_file_names


      !  Open the file that has the list of filenames.

      OPEN (FILE   = '.foo'        , &
            UNIT   = 111           , &
            STATUS = 'OLD'         , &
            ACCESS = 'SEQUENTIAL'  , &
            FORM   = 'FORMATTED'     )

      !  Read all of the file names and store them.

      CALL mprintf(.true.,STDOUT, ' ')
      CALL mprintf(.true.,STDOUT, 'FOUND the following input files:')

      DO loop = 1 , number_of_input_files
         READ ( 111 , FMT='(A)' ) input_file_names(loop)
         CALL mprintf(.true.,STDOUT, ' %s ', s1=TRIM(input_file_names(loop)))
      END DO
      CLOSE ( 112 )
      CALL mprintf(.true.,STDOUT, ' ')

      !   We clean up our own messes.

      CALL SYSTEM ( '/bin/rm -f .foo'  )
      CALL SYSTEM ( '/bin/rm -f .foo1' )


   END SUBROUTINE unix_ls

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE all_spaces ( command , length_of_char ) 

      IMPLICIT NONE

      INTEGER                        :: length_of_char
      CHARACTER (LEN=length_of_char) :: command
      INTEGER                        :: loop

      DO loop = 1 , length_of_char
         command(loop:loop) = ' '
      END DO

   END SUBROUTINE all_spaces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE init_module_get_file_names
   
      IMPLICIT NONE
      input_file_names = '                                                  ' // &
                         '                                                  ' // &
                         '                                '

   END SUBROUTINE init_module_get_file_names

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE module_get_file_names
