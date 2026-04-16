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
INTEGER:: nf,ne,nv,nflags                                               !!!
INTEGER,ALLOCATABLE:: nface(:),fev(:,:,:)                                    !!!
INTEGER,ALLOCATABLE:: flag(:,:)                !!!
INTEGER,ALLOCATABLE:: srule_fe(:),srule_fv(:),srule_ev(:)                    !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL:: flgf,fevf                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Where to read the system's data                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
fevf=.false.                                                                 !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fev',EXIST=fevf)                     !!!
IF(.not.fevf)THEN
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.flg',EXIST=flgf)                     !!!
  IF(.not.flgf) INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.flg',EXIST=flgf)       !!!
END IF  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the fev tensor or obtaining it from flag graph         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(fevf)THEN                                                                 !!!
  ! Reading embedding tensor from .fev file (allocations inside)             !!!
  CALL read_fev(nf,ne,nv,fev,filename)                                       !!!
  ! Faces sizes (allocations inside)                                         !!!
  CALL nfaces_from_fev(nf,ne,nv,fev,nface)                                   !!!  
ELSE IF(flgf)THEN                                                             !!!
  ! Reading the flag graph from a .flg or a .b.flg file                      !!!
  CALL read_flg(nf,ne,nv,nflags,nface,flag,filename)   !!!
  ! Creating the fev tensor                                                  !!!
  CALL flg_to_fev(nflags,flag,nf,ne,nv,fev)                                  !!!
ELSE                                                                         !!!
  STOP 'No fev-tensor or flag-graph file found!'                 !!!
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
