PROGRAM faces2xyz

IMPLICIT NONE
INTEGER:: i,nf,i0
CHARACTER*100:: filename_target
CHARACTER*100,ALLOCATABLE:: filename_list(:)
LOGICAL:: have_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting target filename
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE='inp')
READ(1,*) filename_target
CLOSE(UNIT=1)
! Early exit if xyz already exist
INQUIRE(FILE=TRIM(ADJUSTL(filename_target))//'.xyz',EXIST=have_file)
IF(have_file) STOP
! Getting system's size
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename_target))//'.b.fcs',FORM='UNFORMATTED')
READ(1) nf
CLOSE(UNIT=1)
! Ancestor list
ALLOCATE(filename_list(nf))
filename_list(nf)=filename_target
i0=2
DO i=nf,2,-1
  OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename_list(i)))//'.anc')
  READ(1,*) filename_list(i-1)
  CLOSE(UNIT=1)
  INQUIRE(FILE=TRIM(ADJUSTL(filename_list(i-1)))//'.xyz',EXIST=have_file)
  IF(have_file)THEN
    i0=i
    EXIT
  END IF
END DO
IF(i0.eq.2)THEN
  INQUIRE(FILE=TRIM(ADJUSTL(filename_list(1)))//'.xyz',EXIST=have_file)
  IF(.not.have_file) i0=1
END IF  
DO i=i0,nf
  print*, 'Making ',filename_list(i)
  CALL make_a_xyz(filename_list(i))
END DO


END PROGRAM faces2xyz
