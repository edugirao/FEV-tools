PROGRAM flags_to_faces

IMPLICIT NONE
INTEGER:: nf,ne,nv,nflags,nmax,i
INTEGER,ALLOCATABLE:: flag(:,:),nface(:)
INTEGER,ALLOCATABLE:: neigh_flag(:,:)
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)
LOGICAL:: have_file_b,have_file_f
LOGICAL,ALLOCATABLE:: uneq_face(:)
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:)
CHARACTER*100:: filename
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading .fev file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file
CALL read_init(filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading flag graph and faces sizes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)
IF(have_file_b)THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')
  READ(3) nflags,nf,ne,nv
  ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),nface(nf))    
  READ(3) nface
  READ(3) flag
  READ(3) neigh_flag
  CLOSE(UNIT=3)          
ELSE
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)
  IF(have_file_f)THEN  
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.flg')
    READ(3,*) nflags,nf,ne,nv
    ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),nface(nf))
    READ(3,*) nface  
    DO i=1,nflags
      READ(3,*) flag(i,:),neigh_flag(i,:)
    END DO  
    CLOSE(UNIT=3)
  ELSE
    STOP 'Error: no .flg or .b.flg file found'
  END IF
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting faces/edges/vertices/bridges in faces
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nmax=MAXVAL(nface)
ALLOCATE(f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),b_in_f(nf,nmax))
CALL fev_in_faces(nflags,flag,neigh_flag,nf,nface,f_in_f,e_in_f,v_in_f,b_in_f,nmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting unequivalent faces and edges
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(uneq_face(nf),u_in_f(nf,nmax))
CALL neq_fe(nflags,flag,nf,ne,nface,nmax,e_in_f,uneq_face,u_in_f,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing faces/edges info on the .fcs file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')
! Writing tensor sizes
WRITE(3,'(4I5)') nf,ne,nv,nmax
! Writing faces 1) size,uneq_f, 2) edges, and 3) vertices
DO i=1,nf
  WRITE(3,'(I0,2X,L1)') nface(i),uneq_face(i)
  WRITE(3,'(100I7)') f_in_f(i,1:nface(i))
  WRITE(3,'(100I7)') e_in_f(i,1:nface(i))
  WRITE(3,'(100I7)') v_in_f(i,1:nface(i))
  WRITE(3,'(100L7)') b_in_f(i,1:nface(i))
  WRITE(3,'(100L7)') u_in_f(i,1:nface(i))
END DO
CLOSE(UNIT=3)
OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')
! Writing tensor sizes
WRITE(3) nf,ne,nv,nmax
! Writing faces 1) size,uneq_f, 2) edges, and 3) vertices
DO i=1,nf
  WRITE(3) nface(i),uneq_face(i)
  WRITE(3) f_in_f(i,1:nface(i))
  WRITE(3) e_in_f(i,1:nface(i))
  WRITE(3) v_in_f(i,1:nface(i))
  WRITE(3) b_in_f(i,1:nface(i))
  WRITE(3) u_in_f(i,1:nface(i))
END DO
CLOSE(UNIT=3)  
DEALLOCATE(flag,neigh_flag,nface)
DEALLOCATE(f_in_f,e_in_f,v_in_f,b_in_f,u_in_f,uneq_face)
END PROGRAM flags_to_faces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
