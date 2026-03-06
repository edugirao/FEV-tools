!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fev2sum                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool to compute the Sum-Rules from the Embedding-Tensor.                   !!!
! Converts a .fev file to a .sum file with the output of the sum rules.      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_rule                                                               !!!
USE tools_writ                                                               !!!
USE tools_conv                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,nflags,nmax                                               !!!
INTEGER,ALLOCATABLE:: nface(:),fev(:,:,:)                                    !!!
INTEGER,ALLOCATABLE:: e_in_f(:,:),v_in_f(:,:)                                !!!
INTEGER,ALLOCATABLE:: flag(:,:),neigh_flag(:,:),flag_color(:)                !!!
INTEGER,ALLOCATABLE:: srule_fe(:),srule_fv(:),srule_ev(:)                    !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: flg,fcs,fevf                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Where to read the system's data                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
fevf=.false.                                                                 !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fev',EXIST=fevf)                     !!!
CALL wheretoread(flg,fcs,filename)                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the fev tensor or obtaining it from flag graph or faces-info       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(fevf)THEN                                                                 !!!
  ! Reading embedding tensor from .fev file (allocations inside)             !!!
  CALL read_fev(nf,ne,nv,fev,filename)                                       !!!
  ! Faces sizes (allocations inside)                                         !!!
  CALL nfaces_from_fev(nf,ne,nv,fev,nface)                                   !!!  
ELSE IF(flg)THEN                                                             !!!
  ! Reading the flag graph from a .flg or a .b.flg file                      !!!
  CALL read_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)   !!!
  ! Creating the fev tensor                                                  !!!
  CALL flg_to_fev(nflags,flag,nf,ne,nv,fev)                                  !!!
ELSE IF(fcs)THEN                                                             !!!
  ! Reading part of fcs info from an .fcs or .b.fcs file (allocations inside)!!!
  CALL read_fcs_only_ev_in_f(nf,ne,nv,nmax,nface,e_in_f,v_in_f,filename)     !!!
  ! Create fev from fcs (allocations inside fcs_to_fev)                      !!!
  CALL fcs_to_fev(nf,ne,nv,nmax,nface,e_in_f,v_in_f,fev)                     !!!
ELSE                                                                         !!!
  STOP 'No fev-tensor, flag-graph or faces-info file found!'                 !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Computing sum rules                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(srule_fe(nv),srule_fv(ne),srule_ev(nf))                             !!!
CALL sum_rules(nf,ne,nv,fev,srule_fe,srule_fv,srule_ev)                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Writing sum rules                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL write_sum_rules(nf,ne,nv,srule_fe,srule_fv,srule_ev,nface,filename)     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fev2sum                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
