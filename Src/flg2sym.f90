!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2sym                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
USE tools_symm                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags,nrotmaps,nmirmaps,nglimaps,ntramaps                !!!
INTEGER:: mirror_count,nmaps,nrot,nmir                                       !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),m2(:,:),m1(:,:)                     !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:)                          !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:)                               !!!
INTEGER,ALLOCATABLE:: rot_i(:),rot_n(:),mir_i(:),mir_m(:)                    !!!
CHARACTER*1,ALLOCATABLE:: rot_e(:),mir_e(:)                                  !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*4:: group                                                          !!!
LOGICAL:: nocross                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph from a .flg or a .b.flg file                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating maps (fev_maps.f90)                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Searching maps from flags                                                  !!!
CALL flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,nmaps,maps,maps_nfixed,m1,m2)
! Writing maps                                                               !!!
CALL write_map(nflags,nmaps,maps,maps_nfixed,filename)                       !!!
! Identifying map types                                                      !!!
CALL map_types(nflags,flag_color,nrotmaps,nmirmaps,nglimaps,ntramaps,nmaps,maps,maps_nfixed)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Extracting symmetries from maps (fev_symm.f90)                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL get_rot(nflags,flag,m1,nmaps,maps,maps_nfixed,nrot,rot_e,rot_i,rot_n)   !!!
CALL write_rot(nrot,rot_e,rot_i,rot_n,filename)                              !!!
CALL get_mir(nflags,flag,m2,nmaps,maps,maps_nfixed,nmir,mir_e,mir_i,mir_m)   !!!
CALL write_mir(nmir,mir_e,mir_i,mir_m,filename)                              !!!
CALL counting_mirrors(nflags,flag,m2,nmaps,maps,maps_nfixed,mirror_count)    !!!
IF(((nrotmaps.eq.1).AND.(nmirmaps.eq.2).AND.(nglimaps.eq.0)).OR. &           !!!
 & ((nrotmaps.eq.2).AND.(nmirmaps.eq.3).AND.(nglimaps.eq.0)))THEN            !!!
  ! Cheking for axis off-mirrors                                             !!!
  CALL axis_off_mirrors(nrot,rot_i,rot_e,nmir,mir_i,mir_e,nocross)           !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying wallpaper group                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL symmetry_group(nrotmaps,nmirmaps,nglimaps,mirror_count,nocross,group)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocations                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(flag,neigh_flag,flag_color,nface,m1,m2)                           !!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.sym')                            !!!
WRITE(1,'(A)') group                                                         !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2sym                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
