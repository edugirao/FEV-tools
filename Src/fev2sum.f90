!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fev2sum                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_flag                                                               !!!
USE tools_read                                                               !!!
USE tools_rule                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,k,nf,ne,nv,nflags                                              !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),fev(:,:,:)                          !!!
INTEGER,ALLOCATABLE:: srule_fe(:),srule_fv(:),srule_ev(:)                    !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading embedding tensor from .fev file (allocations inside)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fev(nf,ne,nv,fev,filename)                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Computing sum rules                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(srule_fe(nv),srule_fv(ne),srule_ev(nf))                             !!!
CALL sum_rules(nf,ne,nv,fev,srule_fe,srule_fv,srule_ev)                      !!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.sum')                            !!!
WRITE(1,'(A)') '!****************** Sum Rules *******************!'          !!!
WRITE(1,'(A,I0)') 'Euler Characteristic -> ',nf-ne+nv                        !!!
WRITE(1,'(A)') '!------------------------------------------------!'          !!!
WRITE(1,'(A)') 'Face-Edge rule test'                                         !!!
DO k=1,nv                                                                    !!!
  WRITE(1,'(A,I3,A,I2)')  'Vertex',k,'->',srule_fe(k)                        !!!
END DO                                                                       !!!
WRITE(1,'(A)') '!------------------------------------------------!'          !!!
WRITE(1,'(A)') 'Face-Vertex rule test'                                       !!!
DO j=1,ne                                                                    !!!
  WRITE(1,'(A,I3,A,I2)') 'Edge',j,'->',srule_fv(j)                           !!!
END DO                                                                       !!!
WRITE(1,'(A)') '!------------------------------------------------!'          !!!
WRITE(1,'(A)') 'Edge-Vertex rule test'                                       !!!
DO i=1,nf                                                                    !!!
  WRITE(1,'(A,I3,A,I2)')  'Face',i,'->',srule_ev(i)                          !!!
END DO                                                                       !!!
WRITE(1,'(A)') '!************************************************!'          !!!
WRITE(1,'(A)') '! Faces Sizes                                    !'          !!!
WRITE(1,'(A)') '!------------------------------------------------!'          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating flags (fev_flag.f90)                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Building flags                                                             !!!
nflags=6*nv ! We have 6 flags for each vertex                                !!!
ALLOCATE(flag(nflags,3))                                                     !!!
flag=0                                                                       !!!
CALL build_flags(nflags,flag,nf,ne,nv,fev)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Faces sizes                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(nface(nf))                                                          !!!
CALL faces_sizes(nf,nflags,flag,nface)                                       !!!
WRITE(1,'(100I7)') nface                                                     !!!
WRITE(1,'(A)') '!************************************************!'          !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fev2sum                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
