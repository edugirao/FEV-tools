!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2pri                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for extract the primitive cell of a structure from its Flag-Graph     !!!
! within a .flg or a .b.flg file format. Writes a primitive cell .fev file.  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_flag                                                               !!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
USE tools_conv                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nflags,ntramaps,ne,nv,nmaps,nmax                                !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),fev(:,:,:),maps(:,:),maps_nfixed(:) !!!
INTEGER,ALLOCATABLE:: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)                    !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:),tmaps(:,:)               !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL,ALLOCATABLE:: uneq_face(:)                                           !!!
LOGICAL,ALLOCATABLE:: b_in_f(:,:),u_in_f(:,:)                                !!!
LOGICAL:: flg,fcs                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Where to read the system's data                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL wheretoread(flg,fcs,filename)                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph or obtaining it from faces-info                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(flg)THEN                                                                  !!!
  ! Reading the flag graph from a .flg or a .b.flg file                      !!!
  WRITE(*,*) 'Reading from a .b.flg or a .flg file.'                         !!!
  CALL read_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)   !!!
ELSE IF(fcs)THEN                                                             !!!
  ! Reading part of fcs info from an .fcs or .b.fcs file (allocations inside)!!!
  WRITE(*,*) 'Reading from a .b.fcs or a .fcs file.'                         !!!
  CALL read_fcs_only_ev_in_f(nf,ne,nv,nmax,nface,e_in_f,v_in_f,filename)     !!!
  ! Create flg from fcs (allocations inside fcs_to_flg)                      !!!
  CALL fcs_to_flg(nf,nmax,nface,e_in_f,v_in_f,nflags,flag,neigh_flag,flag_color)
ELSE                                                                         !!!
  STOP 'No flag-graph or faces-info file found!'                             !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating maps (fev_maps.f90)                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,nmaps,maps,maps_nfixed)!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Get translation maps                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL get_tramaps(nmaps,nflags,maps,maps_nfixed,flag_color,ntramaps,tmaps)    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reduce flags to primitive cell                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL flg_to_pri(filename,nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,ntramaps,tmaps)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing primitive flag graph                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL write_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,TRIM(ADJUSTL(filename))//'primitive')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Recreating maps (fev_maps.f90)                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(maps,maps_nfixed)                                                 !!!
CALL flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,nmaps,maps,maps_nfixed)!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting faces/edges/vertices/bridges in faces                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nmax=MAXVAL(nface)                                                           !!!
CALL fev_in_faces(nflags,flag,neigh_flag,nf,nface,f_in_f,e_in_f,v_in_f,b_in_f,nmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting unequivalent faces and edges                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(uneq_face(nf),u_in_f(nf,nmax))                                      !!!
CALL neq_fe(nflags,flag,nf,ne,nface,nmax,e_in_f,uneq_face,u_in_f,nmaps,maps) !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing faces/edges info on the .fcs file                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
CALL write_fcs(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &         !!!
                     & b_in_f,u_in_f,TRIM(ADJUSTL(filename))//'primitive')   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Building primitive FEV tensor (allocations inside)                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL flg_to_fev(nflags,flag,nf,ne,nv,fev)                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing primitive FEV tensor                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL write_fev(nf,ne,nv,fev,TRIM(ADJUSTL(filename))//'primitive')            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocations                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(flag)                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2pri                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
