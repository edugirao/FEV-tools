!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flag_graph_to_fev                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags,i,j,l                                              !!!
INTEGER,ALLOCATABLE:: flag(:,:),fev(:,:,:)                                   !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: have_file_b,have_file_f                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading .fev file                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
CALL read_init(filename)                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph from the .flg file                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)            !!!
IF(have_file_b)THEN                                                          !!!
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')     !!!
  READ(3) nflags,nf,ne,nv                                                    !!!
  ALLOCATE(flag(nflags,3))                                                   !!!
  READ(3)                                                                    !!!
  READ(3) flag                                                               !!!
  CLOSE(UNIT=3)                                                              !!!
ELSE                                                                         !!!
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)            !!!
  IF(have_file_f)THEN                                                        !!!
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.flg')                        !!!
    READ(3,*) nflags,nf,ne,nv                                                !!!
    ALLOCATE(flag(nflags,3))                                                 !!!
    READ(3,*)                                                                !!!
    DO i=1,nflags                                                            !!!
      READ(3,*) flag(i,:)                                                    !!!
    END DO                                                                   !!!
    CLOSE(UNIT=3)                                                            !!!
  ELSE                                                                       !!!
    STOP 'Error: no .flg or .b.flg file found'                               !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating the fev tensor                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
ALLOCATE(fev(nf,ne,nv))                                                      !!!
fev=0                                                                        !!!
DO i=1,nflags                                                                !!!
  fev(flag(i,1),flag(i,2),flag(i,3))=1                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing the fev tensor                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.fev')                            !!!
WRITE(1,*) nf,ne,nv          ! .fev file format:                             !!!
DO i=1,nf                   ! - 1st line: nf ne nv                           !!!
  DO j=1,ne                 ! - Then one line per each tensor entry (0 or 1) !!!
    DO l=1,nv               ! -> Outer loop --> Faces                        !!!
      WRITE(1,*) fev(i,j,l)  ! -> Middle loop -> Edges                       !!!
    END DO                  ! -> Inner loop --> Vertices                     !!!
  END DO                    !                                                !!!
END DO                      !                                                !!!
CLOSE(UNIT=1)               !                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flag_graph_to_fev                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
