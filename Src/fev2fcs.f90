PROGRAM fev_to_faces_info

IMPLICIT NONE
INTEGER:: nf,ne,nv,nflags,ng,ig,i,nmax
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),fev(:,:,:)
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:)
INTEGER,ALLOCATABLE:: reduc_ev(:,:)
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:),uneq_face(:)
CHARACTER*100:: filename,fileindex
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading .fev file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file
CALL read_init(filename)
! Reading FEV Data
CALL read_fev_size(nf,ne,nv,filename)
ALLOCATE(fev(nf,ne,nv))
CALL read_fev(nf,ne,nv,fev,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating flags
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nflags=6*nv ! We have 6 flags for each vertex
ALLOCATE(flag(nflags,3))
flag=0
CALL build_flags(nflags,flag,nf,ne,nv,fev)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Faces sizes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(nface(nf))
CALL faces_sizes(nf,nflags,flag,nface)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Neighbor flags
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(neigh_flag(nflags,3))
neigh_flag=0
! Set face neighbor flags
CALL neigh_f(nflags,neigh_flag,nv)
! Set edge neighbor flags
CALL neigh_e(nflags,neigh_flag,nv)
! Matrix reduction rule - EV
ALLOCATE(reduc_ev(ne,nv))
CALL matrix_reduction_ev(nf,ne,nv,fev,reduc_ev)
! Set vertex neighbor flags
CALL neigh_v(ne,nv,nflags,flag,neigh_flag,reduc_ev) ! Miss twins' v-neighs 
DEALLOCATE(reduc_ev)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting missing v-neighs by bipartition search
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(flag_color(nflags))
flag_color=0     ! Initially all flags have no color
CALL bipartition(nflags,flag_color,neigh_flag,flag,nf,nface,ng)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output of the flag-graphs (.flg file) and faces/edges info (.fcs)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=38,FILE='fgraphs',FORM='UNFORMATTED')
! Running overall generated flag graphs
DO ig=1,ng
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reading flag graph
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  READ(38) neigh_flag,flag_color
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Defining filenames for output
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  IF(ig.eq.1)THEN
    fileindex=TRIM(ADJUSTL(filename))
  ELSE
    WRITE(fileindex,'(I0)') ig
    fileindex=TRIM(ADJUSTL(filename))//'_'//TRIM(ADJUSTL(fileindex))
  END IF
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Writing flag graph on the .flg file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(fileindex))//'.flg')
  WRITE(3,'(4I5)') nflags,nf,ne,nv
  WRITE(3,'(100I7)') nface
  DO i=1,nflags
    WRITE(3,'(7I7)') flag(i,:),neigh_flag(i,:),flag_color(i)
  END DO  
  CLOSE(UNIT=3)
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(fileindex))//'.b.flg',FORM='UNFORMATTED')
  WRITE(3) nflags,nf,ne,nv
  WRITE(3) nface
  WRITE(3) flag
  WRITE(3) neigh_flag
  WRITE(3) flag_color
  CLOSE(UNIT=3)  
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
  ! Writing faces 1) size, uneq_f, 2) edges, and 3) vertices
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
END DO
CLOSE(UNIT=38,STATUS='DELETE')
END PROGRAM fev_to_faces_info

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
