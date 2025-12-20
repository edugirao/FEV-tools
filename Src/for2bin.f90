PROGRAM format_binary_conversion

IMPLICIT NONE
INTEGER:: nf,ne,nv,nflags,nmax,i
INTEGER,ALLOCATABLE:: flag(:,:),nface(:)
INTEGER,ALLOCATABLE:: neigh_flag(:,:),flag_color(:)
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
! (.fcs) <-> (.b.fcs) conversion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)
IF((have_file_b).AND.(.not.have_file_f))THEN
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
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')
  WRITE(3,*) nf,ne,nv,nmax
  DO i=1,nf
    WRITE(3,*) nface(i),uneq_face(i)
    WRITE(3,*) f_in_f(i,1:nface(i))
    WRITE(3,*) e_in_f(i,1:nface(i))
    WRITE(3,*) v_in_f(i,1:nface(i))
    WRITE(3,*) b_in_f(i,1:nface(i))
    WRITE(3,*) u_in_f(i,1:nface(i))
  END DO
  CLOSE(UNIT=3)
ELSE IF((.not.have_file_b).AND.(have_file_f))THEN
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
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')
  WRITE(3) nf,ne,nv,nmax
  DO i=1,nf
    WRITE(3) nface(i),uneq_face(i)
    WRITE(3) f_in_f(i,1:nface(i))
    WRITE(3) e_in_f(i,1:nface(i))
    WRITE(3) v_in_f(i,1:nface(i))
    WRITE(3) b_in_f(i,1:nface(i))
    WRITE(3) u_in_f(i,1:nface(i))
  END DO
  CLOSE(UNIT=3)    
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! (.flg) <-> (.b.flg) conversion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=have_file_b)
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=have_file_f)
IF((have_file_b).AND.(.not.have_file_f))THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')
  READ(3) nflags,nf,ne,nv
  ALLOCATE(nface(nf),uneq_face(nf))
  ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags))
  READ(3) nface
  READ(3) flag
  READ(3) neigh_flag
  READ(3) flag_color
  CLOSE(UNIT=3)          
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.flg')
  WRITE(3,*) nflags,nf,ne,nv
  WRITE(3,*) nface  
  DO i=1,nflags
    WRITE(3,*) flag(i,:),neigh_flag(i,:),flag_color(i)
  END DO  
  CLOSE(UNIT=3)
ELSE IF((.not.have_file_b).AND.(have_file_f))THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.flg')
  READ(3,*) nflags,nf,ne,nv
  ALLOCATE(nface(nf),uneq_face(nf))
  ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags))  
  READ(3,*) nface  
  DO i=1,nflags
    READ(3,*) flag(i,:),neigh_flag(i,:),flag_color(i)
  END DO  
  CLOSE(UNIT=3)
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')
  WRITE(3) nflags,nf,ne,nv
  WRITE(3) nface
  WRITE(3) flag
  WRITE(3) neigh_flag
  WRITE(3) flag_color
  CLOSE(UNIT=3)          
END IF


END PROGRAM format_binary_conversion

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
