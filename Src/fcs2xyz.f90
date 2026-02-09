!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fcs2xyz                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_coor                                                               !!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,i0,f0,ie1,ie2,nv1,nf1                                         !!!
INTEGER,ALLOCATABLE:: nface1(:),x_in_f1(:,:),y_in_f1(:,:)                    !!!
CHARACTER*100:: filename_target,filename_p                                   !!!
CHARACTER*100,ALLOCATABLE:: filename_list(:)                                 !!!
REAL(KIND=8),ALLOCATABLE:: r(:,:)                                            !!!
LOGICAL:: have_file                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting target information                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting target filename                                                    !!!
CALL read_init(filename_target,'inp')                                        !!!
! Early exit if xyz already exist                                            !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename_target))//'.xyz',EXIST=have_file)         !!!
IF(have_file) STOP TRIM(ADJUSTL(filename_target))//'.xyz already exists.'    !!!
! Getting target's size                                                      !!!
CALL read_fcs_only_nf(nf,filename_target)                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Ancestor list                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_anc_list(nf,filename_target,filename_list,i0)                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Taking care of graphene if the first in the ancestor list                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(i0.eq.1)THEN                                                              !!!
  filename_list(1)='6'                                                       !!!
  CALL write_graphene                                                        !!!
  CALL make_graphene                                                         !!!
  i0=2                                                                       !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating strucuture(s) from ancestor list                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=i0,nf                                                                   !!!
  print*, 'Making ',filename_list(i)                                         !!!
  ! Getting ancestor info                                                    !!!
  CALL read_anc(filename_list(i),filename_p,f0,ie1,ie2)                      !!!
  ! Optimizing the xyz                                                       !!!
  CALL make_a_xyz(filename_list(i),filename_p,f0,ie1,ie2,nv1,r,nf1,nface1,x_in_f1,y_in_f1)
  !  Writing the xyz                                                         !!!
  CALL write_xyz(nv1,r,filename_list(i))                                     !!!
  !  Writing the nxy                                                         !!!
  CALL write_nxy(nf1,MAXVAL(nface1),nface1,x_in_f1,y_in_f1,filename_list(i)) !!!
  DEALLOCATE(r,x_in_f1,y_in_f1,nface1)                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fcs2xyz                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
