PROGRAM fev_to_flag_graph_generator

IMPLICIT NONE
INTEGER:: nf,ne,nv,nflags,ng,ig,i
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),fev(:,:,:)
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:)
INTEGER,ALLOCATABLE:: reduc_ev(:,:)
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
END DO
CLOSE(UNIT=38,STATUS='DELETE')
END PROGRAM fev_to_flag_graph_generator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
