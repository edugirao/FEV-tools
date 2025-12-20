PROGRAM faces_to_flags

IMPLICIT NONE
INTEGER:: nf,ne,nv,nmax,i,j,l
INTEGER,ALLOCATABLE:: fev(:,:,:),nface(:)
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)
LOGICAL,ALLOCATABLE:: uneq_face(:)
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:)
LOGICAL:: have_file_b,have_file_f
CHARACTER*100:: filename
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading .fev file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file
CALL read_init(filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading faces/edges info on the .fcs file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)
IF(have_file_b)THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')
  ! Reading tensor sizes
  READ(3) nf,ne,nv,nmax
  ! Reading faces 1) size,uneq_face, 2) edges, and 3) vertices
  ALLOCATE(nface(nf),uneq_face(nf))
  ALLOCATE(f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),b_in_f(nf,nmax),u_in_f(nf,nmax))
  DO i=1,nf
    READ(3) nface(i),uneq_face(i)
    READ(3) f_in_f(i,1:nface(i))
    READ(3) e_in_f(i,1:nface(i))
    READ(3) v_in_f(i,1:nface(i))
    READ(3) b_in_f(i,1:nface(i))
    READ(3) u_in_f(i,1:nface(i))
  END DO
  CLOSE(UNIT=3)  
ELSE
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)
  IF(have_file_f)THEN
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')
    ! Reading tensor sizes
    READ(3,*) nf,ne,nv,nmax
    ! Reading faces 1) size,uneq_face, 2) edges, and 3) vertices
    ALLOCATE(nface(nf),uneq_face(nf))
    ALLOCATE(f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),b_in_f(nf,nmax),u_in_f(nf,nmax))
    DO i=1,nf
      READ(3,*) nface(i),uneq_face(i)
      READ(3,*) f_in_f(i,1:nface(i))
      READ(3,*) e_in_f(i,1:nface(i))
      READ(3,*) v_in_f(i,1:nface(i))
      READ(3,*) b_in_f(i,1:nface(i))
      READ(3,*) u_in_f(i,1:nface(i))
    END DO
    CLOSE(UNIT=3)
  ELSE
    STOP 'Error: no .fcs or .b.fcs file found'
  END IF
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating the fev tensor
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
ALLOCATE(fev(nf,ne,nv))
fev=0

DO i=1,nf
  DO j=1,nface(i)-1
    fev(i,e_in_f(i,j),v_in_f(i,j))=1
    fev(i,e_in_f(i,j),v_in_f(i,j+1))=1
  END DO
  j=nface(i)
  fev(i,e_in_f(i,j),v_in_f(i,j))=1
  fev(i,e_in_f(i,j),v_in_f(i,1))=1  
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing the fev tensor
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.fev')
WRITE(1,*) nf,ne,nv          ! .fev file format:
DO i=1,nf                   ! - 1st line: nf ne nv
  DO j=1,ne                 ! - Then one line per each tensor entry (0 or 1)
    DO l=1,nv               ! -> Outer loop --> Faces
      WRITE(1,*) fev(i,j,l)  ! -> Middle loop -> Edges
    END DO                  ! -> Inner loop --> Vertices
  END DO                    !
END DO                      !
CLOSE(UNIT=1)               !

DEALLOCATE(fev,nface)
DEALLOCATE(f_in_f,e_in_f,v_in_f,b_in_f,u_in_f,uneq_face)
END PROGRAM faces_to_flags

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
