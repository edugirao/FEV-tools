!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fev2flg                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for Embedding-Tensor to Flag-Graph conversion.                        !!!
! Converts a .fev file to the .flg and .b.flg file formats.                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_bipt                                                               !!!
USE tools_flag                                                               !!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
USE tools_rule                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags,ng,ig,i,j                                          !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),fev(:,:,:)                          !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:),neigh_flag_j(:,:)        !!!
INTEGER,ALLOCATABLE:: reduc_ev(:,:)                                          !!!
INTEGER,ALLOCATABLE:: neigh_flag_set(:,:,:),flag_color_set(:,:)              !!!
CHARACTER*100:: filename,fileindex                                           !!!
LOGICAL:: same                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading embedding tensor from .fev file (allocations inside)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fev(nf,ne,nv,fev,filename)                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating flags                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nflags=6*nv ! We have 6 flags for each vertex                                !!!
ALLOCATE(flag(nflags,3))                                                     !!!
flag=0                                                                       !!!
CALL build_flags(nflags,flag,nf,ne,nv,fev)                                   !!!
! Faces sizes from flags (allocation inside)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL nfaces_from_flg(nf,nflags,flag,nface)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Neighbor flags                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(neigh_flag(nflags,3))                                               !!!
neigh_flag=0                                                                 !!!
! Set face neighbor flags                                                    !!!
CALL neigh_f(nflags,neigh_flag,nv)                                           !!!
! Set edge neighbor flags                                                    !!!
CALL neigh_e(nflags,neigh_flag,nv)                                           !!!
! Matrix reduction rule - EV                                                 !!!
ALLOCATE(reduc_ev(ne,nv))                                                    !!!
CALL matrix_reduction_ev(nf,ne,nv,fev,reduc_ev)                              !!!
! Set vertex neighbor flags                                                  !!!
CALL neigh_v(ne,nv,nflags,flag,neigh_flag,reduc_ev) ! Miss twins' v-neighs   !!!
DEALLOCATE(reduc_ev)                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting missing v-neighs by bipartition search                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(flag_color(nflags))                                                 !!!
flag_color=0     ! Initially all flags have no color                         !!!
CALL bipartition(nflags,flag_color,neigh_flag,flag,nf,nface,ng,neigh_flag_set,flag_color_set)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Output of the flag-graphs (.flg file) and faces/edges info (.fcs)          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Running overall generated flag graphs                                      !!!
ig=0                                                                         !!!
ALLOCATE(neigh_flag_j(nflags,3))                                             !!!
DO i=1,ng                                                                    !!!
  neigh_flag=neigh_flag_set(i,:,:)                                           !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Checking if repeated flag-graph                                          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  same=.false.                                                               !!!
  DO j=1,i-1                                                                 !!!
    neigh_flag_j=neigh_flag_set(j,:,:)                                       !!!
    CALL check_equiv_fgraphs(nflags,nf,flag,nface,neigh_flag, &              !!!
                           & nflags,nf,flag,nface,neigh_flag_j,same)         !!!
    IF(same) EXIT                                                            !!!
  END DO                                                                     !!!
  IF(same) CYCLE                                                             !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting flag colors                                                      !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  flag_color=flag_color_set(i,:)                                             !!!
  ig=ig+1                                                                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Defining filenames for output                                            !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  IF(ig.eq.1)THEN                                                            !!!
    fileindex=TRIM(ADJUSTL(filename))                                        !!!
  ELSE                                                                       !!!
    WRITE(fileindex,'(I0)') ig                                               !!!
    fileindex=TRIM(ADJUSTL(filename))//'_'//TRIM(ADJUSTL(fileindex))         !!!
  END IF                                                                     !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Writing flag graph on the .flg file                                      !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
  CALL write_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,fileindex) !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fev2flg                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
