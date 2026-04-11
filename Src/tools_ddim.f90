!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_ddim                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE reordering_fcs(nf,ne,nv,nmax,nface,f_in_f,e_in_f,v_in_f,b_in_f,f2f,e2e,v2v,ie2,inv)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,e00,e33,e44,v55,v66,inv,ie2                                      !!!
INTEGER:: f3,e0,e3,e4,v5,v6,ne,nv,nf,nmax                                    !!!
INTEGER:: e2e(ne),v2v(nv),f2f(nf)                                            !!!
INTEGER:: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),nface(nf)          !!!
INTEGER:: f_in_f2(nf,nmax),e_in_f2(nf,nmax),v_in_f2(nf,nmax),nface2(nf)      !!!
LOGICAL:: b_in_f(nf,nmax)                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting relevant elements (includes eventually reversing F0)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL get_deleted_elements(nf,nmax,nface,f_in_f,e_in_f,v_in_f,b_in_f,f3,e0,e3,e4,v5,v6,ie2,inv)
! Which extracted elements appear first                                      !!!
e00=e0                                                                       !!!
e33=e3                                                                       !!!
e44=e4                                                                       !!!
IF(e00.gt.e33)THEN                                                           !!!
  i=e00 ; e00=e33 ; e33=i                                                    !!!
END IF                                                                       !!!
IF(e33.gt.e44)THEN                                                           !!!
  i=e44 ; e44=e33 ; e33=i                                                    !!!
END IF                                                                       !!!
IF(e00.gt.e33)THEN                                                           !!!
  i=e00 ; e00=e33 ; e33=i                                                    !!!
END IF                                                                       !!!
v55=v5                                                                       !!!
v66=v6                                                                       !!!
IF(v55.gt.v66)THEN                                                           !!!
  i=v55 ; v55=v66 ; v66=i                                                    !!!
END IF                                                                       !!!
! Edge-to-Edge map                                                           !!!
e2e=0                                                                        !!!
DO i=1,ne                                                                    !!!
  ! Not extracted edges                                                      !!!
  IF(i.lt.e00)THEN                                                           !!!
    e2e(i)=i                                                                 !!!
  ELSE IF((i.gt.e00).AND.(i.lt.e33))THEN                                     !!!
    e2e(i)=i-1                                                               !!!
  ELSE IF((i.gt.e33).AND.(i.lt.e44))THEN                                     !!!
    e2e(i)=i-2                                                               !!!
  ELSE IF(i.gt.e44)THEN                                                      !!!
    e2e(i)=i-3                                                               !!!
  END IF                                                                     !!!
  ! Extracted edges                                                          !!!
  IF(i.eq.e0)THEN                                                            !!!
    e2e(i)=ne                                                                !!!
  ELSE IF(i.eq.e3)THEN                                                       !!!
    e2e(i)=ne-2                                                              !!!
  ELSE IF(i.eq.e4)THEN                                                       !!!
    e2e(i)=ne-1                                                              !!!  
  END IF                                                                     !!!  
END DO                                                                       !!!
! Vert-to-Vert map                                                           !!!
v2v=0                                                                        !!!
DO i=1,nv                                                                    !!!
  ! Not extracted verts                                                      !!!
  IF(i.lt.v55)THEN                                                           !!!
    v2v(i)=i                                                                 !!!
  ELSE IF((i.gt.v55).AND.(i.lt.v66))THEN                                     !!!
    v2v(i)=i-1                                                               !!!
  ELSE IF(i.gt.v66)THEN                                                      !!!
    v2v(i)=i-2                                                               !!!
  END IF                                                                     !!!
  ! Extracted verts                                                          !!!
  IF(i.eq.v5)THEN                                                            !!!
    v2v(i)=nv-1                                                              !!!
  ELSE IF(i.eq.v6)THEN                                                       !!!
    v2v(i)=nv                                                                !!!
  END IF                                                                     !!!    
END DO                                                                       !!!
! Face-to-Face map                                                           !!!
f2f=0                                                                        !!!
DO i=1,nf                                                                    !!!
  ! Not extracted faces                                                      !!!
  IF(i.lt.f3)THEN                                                            !!!
    f2f(i)=i                                                                 !!!
  ELSE IF(i.gt.f3)THEN                                                       !!!
    f2f(i)=i-1                                                               !!!
  END IF                                                                     !!!
  ! Extracted face                                                           !!!
  IF(i.eq.f3)THEN                                                            !!!
    f2f(i)=nf                                                                !!!
  END IF                                                                     !!!    
END DO                                                                       !!!
! Temporary arrays                                                           !!!
f_in_f2=0 ; e_in_f2=0 ; v_in_f2=0                                            !!!
DO i=1,nf                                                                    !!!
  nface2(f2f(i))=nface(i)                                                    !!!
  f_in_f2(f2f(i),1:nface(i))=f2f(f_in_f(i,1:nface(i)))                       !!!
  e_in_f2(f2f(i),1:nface(i))=e2e(e_in_f(i,1:nface(i)))                       !!!
  v_in_f2(f2f(i),1:nface(i))=v2v(v_in_f(i,1:nface(i)))                       !!!
END DO                                                                       !!!
! Passing the new order                                                      !!!
nface=nface2                                                                 !!!
f_in_f=f_in_f2                                                               !!!
e_in_f=e_in_f2                                                               !!!
v_in_f=v_in_f2                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE reordering_fcs                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_deleted_elements(nf,nmax,nface,f_in_f,e_in_f,v_in_f,b_in_f,f3,e0,e3,e4,v5,v6,ie2,inv)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nmax,f0,f3,e0,e3,e4,v5,v6,inv,i,ie0,ie2,j                       !!!
INTEGER:: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),nface(nf)          !!!
INTEGER:: f_in_f2(nmax),e_in_f2(nmax),v_in_f2(nmax)                          !!!
LOGICAL:: bridge,b_in_f(nf,nmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! We choose F3 to be a face smaller than a hexagon                           !!!
f3=0                                                                         !!!
! Preferably a triangle
DO i=1,nf                                                                    !!!
  IF(nface(i).eq.3)THEN                                                      !!!
    f3=i                                                                     !!!
    EXIT
  END IF                                                                     !!!
END DO                                                                       !!!
! If not a triangle, look for a square or pentagon
IF(f3.eq.0)THEN
  DO i=1,nf                                                                    !!!
    IF(nface(i).lt.6)THEN                                                      !!!
      f3=i                                                                     !!!
      EXIT
    END IF                                                                     !!!
  END DO                                                                       !!!
END IF
! Not a triangle/square/pentagon? Look for a hexagon with no bridges         !!!
IF(f3.eq.0)THEN
  DO i=1,nf                                                                    !!!
    IF(nface(i).eq.6)THEN   ! Check each hexagon                                                   !!!
      bridge=.false.        ! In principle, no bridges
      DO j=1,6              ! Search for bridges inside the hexagon
        IF(b_in_f(i,j))THEN ! Found a bridge?
          bridge=.true.     ! Update the logical variable
          EXIT              ! and leave. 
        END IF
      END DO
      ! Verifying if no bridge
      IF(.not.bridge)THEN 
        f3=i                                                                     !!!
        EXIT
      END IF  
    END IF                                                                     !!!
  END DO                                                                       !!!
END IF
IF(f3.eq.0) STOP 'F3 not found.'                                             !!!
! We choose E0 to be the one before last edge in F3                          !!!
e0=e_in_f(f3,nface(f3)-1)                                                    !!!
! We choose E3 to be the second before last edge in F3                       !!!
e3=e_in_f(f3,nface(f3)-2)                                                    !!!
! We choose E4 to be last edge in F3                                         !!!
e4=e_in_f(f3,nface(f3))                                                      !!!
! V5 is the last vertex in F3 (it is where E0 finishes)                      !!!
v5=v_in_f(f3,nface(f3))                                                      !!!
! V6 is the one before last vertex in F3 (it is where E0 starts)             !!!
v6=v_in_f(f3,nface(f3)-1)                                                    !!!
! Getting E0 index in F0                                                     !!!
f0=f_in_f(f3,nface(f3)-1) ! F0 is the other face touching E0                 !!!
DO i=1,nface(f0) ! E0 index in F0                                            !!!
  IF(e_in_f(f0,i).eq.e0)THEN                                                 !!!
    ie0=i                                                                    !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Checking need for reversing F0 order                                       !!!
inv=1                                                                        !!!
IF(v_in_f(f0,ie0).eq.v6)THEN                                                 !!!
  inv=-1                                                                     !!!
  CALL reverse_f0(f0,nf,nface,nmax,f_in_f,e_in_f,v_in_f)                     !!!  
  ie0=nface(f0)+1-ie0                                                        !!!
END IF                                                                       !!!
! Checking need for shifting F0 order                                        !!!
ie2=ie0+1                                                                    !!!
IF(ie2.gt.nface(f0)) ie2=ie2-nface(f0)                                       !!!
j=ie2                                                                        !!!
f_in_f2=0 ; e_in_f2=0 ; v_in_f2=0                                            !!!
DO i=1,nface(f0)                                                             !!!
  j=j+1                                                                      !!!
  IF(j.gt.nface(f0)) j=j-nface(f0)                                           !!!
  f_in_f2(i)=f_in_f(f0,j)                                                    !!!
  e_in_f2(i)=e_in_f(f0,j)                                                    !!!
  v_in_f2(i)=v_in_f(f0,j)                                                    !!!
END DO                                                                       !!!
f_in_f(f0,:)=f_in_f2                                                         !!!
e_in_f(f0,:)=e_in_f2                                                         !!!
v_in_f(f0,:)=v_in_f2                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_deleted_elements                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE reverse_f0(f0,nf,nface,nmax,f_in_f,e_in_f,v_in_f)                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: f0,nf,nface(nf),nmax,f,e,v,i                                       !!!
INTEGER:: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reversing face elements                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,nface(f0)/2                                                           !!!
  f=f_in_f(f0,i)                                                             !!!
  f_in_f(f0,i)=f_in_f(f0,nface(f0)+1-i)                                      !!!
  f_in_f(f0,nface(f0)+1-i)=f                                                 !!!
  e=e_in_f(f0,i)                                                             !!!
  e_in_f(f0,i)=e_in_f(f0,nface(f0)+1-i)                                      !!!
  e_in_f(f0,nface(f0)+1-i)=e                                                 !!!
  v=v_in_f(f0,i)                                                             !!!
  v_in_f(f0,i)=v_in_f(f0,nface(f0)+1-i)                                      !!!
  v_in_f(f0,nface(f0)+1-i)=v                                                 !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE reverse_f0                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE reverse_f0xy(f0,nf,nface,nmax,x_in_f,y_in_f)                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: f0,nf,nface(nf),nmax,x,y,i                                         !!!
INTEGER:: x_in_f(nf,nmax),y_in_f(nf,nmax)                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reversing face elements                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,nface(f0)/2                                                           !!!
  x=x_in_f(f0,i)                                                             !!!
  x_in_f(f0,i)=x_in_f(f0,nface(f0)+1-i)                                      !!!
  x_in_f(f0,nface(f0)+1-i)=x                                                 !!!
  y=y_in_f(f0,i)                                                             !!!
  y_in_f(f0,i)=y_in_f(f0,nface(f0)+1-i)                                      !!!
  y_in_f(f0,nface(f0)+1-i)=y                                                 !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE reverse_f0xy                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE make_prev(nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1,&      !!!
                   & nf0,ne0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0,f0,ie1,ie2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf1,ne1,nv1,nmax1,nf0,ne0,nv0,nmax0,i                              !!!
INTEGER:: f0,f1,f2,e1,e2,v3,v4,ie1,ie2,l                                     !!!
INTEGER:: nface1(nf1)                                                        !!!
INTEGER:: f_in_f1(nf1,nmax1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1)           !!!
INTEGER,ALLOCATABLE:: nface0(:),f_in_f0(:,:),e_in_f0(:,:),v_in_f0(:,:)       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting relevant elements                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL get_relevant_elements(nf1,ne1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1, &   !!!
           & f0,f1,f2,e1,e2,v3,v4,ie1,ie2)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Allocating parent data                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nmax0=2*nmax1                                                                !!!  
nf0=nf1-1                                                                    !!!
ne0=ne1-3                                                                    !!!
nv0=nv1-2                                                                    !!!
ALLOCATE(nface0(nf0))                                                        !!!
ALLOCATE(f_in_f0(nf0,nmax0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0))           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating the parent structure faces                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
l=0                                                                          !!!
e_in_f0=0                                                                    !!!
v_in_f0=0                                                                    !!!
DO i=1,nf1-1 ! Going all child's faces, except F3                            !!!
  ! Make F0                                                                  !!!
  IF(i.eq.f0)THEN                                                            !!!
    CALL build_f0(nf0,nmax0,nface0,e_in_f0,v_in_f0, &                        !!!
        & nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,f0,e1,e2,v3,v4,ie1,ie2,l) !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!
  ! Make F1 if the case                                                      !!!
  IF((i.eq.f1).AND.(f1.ne.f0))THEN                                           !!!
    CALL build_f12(nf0,nmax0,nface0,e_in_f0,v_in_f0, &                       !!!
             & nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,f1,e1,e2,v3,v4,l)    !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!
  ! Make F2 if the case                                                      !!!
  IF((i.eq.f2).AND.(f2.ne.f0).AND.(f2.ne.f1))THEN                            !!!
    CALL build_f12(nf0,nmax0,nface0,e_in_f0,v_in_f0, &                       !!!
             & nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,f2,e1,e2,v3,v4,l)    !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!
  ! Faces not involved in dimer deletion                                     !!!
  CALL build_other_faces(nf0,nmax0,nface0,e_in_f0,v_in_f0, &                 !!!
                           & nf1,nmax1,nface1,e_in_f1,v_in_f1,i,f0,f1,f2,l)    !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Refactoring nmax0                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
nmax0=MAXVAL(nface0)                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE make_prev                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_relevant_elements(nf,ne,nmax,nface,f_in_f,e_in_f,v_in_f, &    !!!
           & f0,f1,f2,e1,e2,v3,v4,ie1,ie2)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nmax,f0,f1,f2,f3,e1,e2,v3,v4,e0,i,j,ie0,ie1,ie2,v1,v2        !!!
INTEGER:: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax),nface(nf)          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting relevant face elements                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deleted face                                                               !!!
f3=nf                                                                        !!!
! Identifying F1 and F2                                                      !!!
f1=f_in_f(f3,nface(f3))         ! Face touching E4                           !!!
f2=f_in_f(f3,nface(f3)-2) ! Face touching E3                                 !!!
! Identifying F0                                                             !!!
f0=f_in_f(f3,nface(f3)-1) ! Other face touching E0 (see E0 definition below) !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting relevant edges elements                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
! Choosing E0, E3, and E4 as 1st, 2nd, and last edges in F3, respectively    !!!
e0=ne ! =e_in_f(f3,1) -> We know E0 index in F3 is 1                         !!!
DO i=1,nface(f0) ! Getting E0 index in F0                                    !!!
  IF(e_in_f(f0,i).eq.e0)THEN                                                 !!!
    ie0=i                                                                    !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! E1 is backward relative to E0 in F0                                        !!!
ie1=ie0-1                                                                    !!!
! IF(ie1.eq.0) ie1=nface(f0)                                                 !!!
e1=e_in_f(f0,ie1)                                                            !!!
! E2 is forward relative to E0 in F0                                         !!!
ie2=ie0+1                                                                    !!!
! IF(ie2.eq.nface(f0)+1) ie2=1                                               !!!
e2=e_in_f(f0,ie2)                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting relevant vertex elements                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
v1=v_in_f(f0,ie1)   ! V1 is in E1                                            !!!                                                           
j=ie2+1                                                                      !!!
IF(j.gt.nface(f0)) j=j-nface(f0)                                             !!!
v2=v_in_f(f0,j)   ! V2 is in Ex after E2                                     !!!
v3=v_in_f(f3,nface(f3)-2) ! V3 is the second before last in F3               !!!
v4=v_in_f(f3,1)   ! V4 is the 1st one in F3                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_relevant_elements                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_f0(nf0,nmax0,nface0,e_in_f0,v_in_f0, &
          & nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,f0,e1,e2,v3,v4,ie10,ie20,l)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,e,v,ie1,ie2,l                                                  !!!
INTEGER:: nf0,ne1,nv1,nf1,nmax0,nmax1,f0,f3,v6,v3,e1,e2,e4,v4,v5,ie10,ie20   !!!
INTEGER:: nface0(nf0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0)                  !!!
INTEGER:: nface1(nf1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ie1=ie10                                                                     !!!
ie2=ie20                                                                     !!!
v5=nv1-1                                                                     !!!
v6=nv1                                                                       !!!
e4=ne1-1                                                                     !!!
l=l+1                                                                        !!!
! Copying first F0 part from V2/EX (EX after E2) until V1/E1                 !!!
j=0                                                                          !!!
DO i=1,ie1                                                                   !!!
  v=v_in_f1(f0,i)                                                            !!!
  e=e_in_f1(f0,i)                                                            !!!
  IF((v.eq.v4).AND.(e.eq.e4))THEN                                            !!!
    j=j+1                                                                    !!!
    e_in_f0(l,j)=e1                                                          !!!
    v_in_f0(l,j)=v4                                                          !!!
    CYCLE                                                                    !!!
  ELSE IF(v.eq.v6)THEN                                                       !!!
    CYCLE                                                                    !!!
  ELSE IF(v.eq.v5)THEN                                                       !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!
  j=j+1                                                                      !!!
  e_in_f0(l,j)=e                                                             !!!
  v_in_f0(l,j)=v                                                             !!!
END DO                                                                       !!!
ie10=j                                                                       !!!
! Copying the F3 part (except E0/E4/E3)                                      !!!
f3=nf1                                                                       !!!
DO i=1,nface1(f3)-3 ! We go from V4-EX to EY before V3 around F3             !!!
  j=j+1                                                                      !!!
  e_in_f0(l,j)=e_in_f1(f3,i)                                                 !!!
  v_in_f0(l,j)=v_in_f1(f3,i)                                                 !!!    
END DO                                                                       !!!
! New E2 (V3-to-V2)                                                          !!!
j=j+1                                                                        !!!
e_in_f0(l,j)=e2                                                              !!!
v_in_f0(l,j)=v3                                                              !!!
ie20=j                                                                       !!!
nface0(l)=j                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_f0                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_f12(nf0,nmax0,nface0,e_in_f0,v_in_f0, &
               & nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1,f12,e1,e2,v3,v4,l)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf0,nf1,ne1,nv1,nmax0,nmax1,f12,v3,v4,v5,v6,e1,e2,e3,e4,e,v,l  !!!
INTEGER:: nface0(nf0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0)                  !!!
INTEGER:: nface1(nf1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
l=l+1                                                                        !!!
j=0                                                                          !!!
v6=nv1                                                                       !!!
v5=nv1-1                                                                     !!!
e3=ne1-2                                                                     !!!
e4=ne1-1                                                                     !!!
DO i=1,nface1(f12)                                                           !!!
  e=e_in_f1(f12,i)                                                           !!!
  v=v_in_f1(f12,i)                                                           !!!    
  IF(v.eq.v6) CYCLE                                                          !!!
  IF(v.eq.v5) CYCLE                                                          !!!
  IF((v.eq.v3).AND.(e.eq.e3))THEN                                            !!!
    j=j+1                                                                    !!!
    e_in_f0(l,j)=e2                                                          !!!
    v_in_f0(l,j)=v                                                           !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!
  IF((v.eq.v4).AND.(e.eq.e4))THEN                                            !!!
    j=j+1                                                                    !!!
    e_in_f0(l,j)=e1                                                          !!!
    v_in_f0(l,j)=v                                                           !!!
    CYCLE                                                                    !!!
  END IF                                                                     !!!  
  j=j+1                                                                      !!!
  e_in_f0(l,j)=e                                                             !!!
  v_in_f0(l,j)=v                                                             !!!
END DO                                                                       !!!
nface0(l)=j                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_f12                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_other_faces(nf0,nmax0,nface0,e_in_f0,v_in_f0, &
                           & nf1,nmax1,nface1,e_in_f1,v_in_f1,i,f0,f1,f2,l)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf0,nf1,nmax0,nmax1,f0,f1,f2,f3,l                                !!!
INTEGER:: nface0(nf0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0)                  !!!
INTEGER:: nface1(nf1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copy face child face i to parent face l                                  !!!
l=l+1                                                                      !!!
nface0(l)=nface1(i)       ! uneq_face1,f_in_f1, b_in_f1 & u_in_f1 later    !!!
e_in_f0(l,1:nface0(l))=e_in_f1(i,1:nface0(l))                              !!!
v_in_f0(l,1:nface0(l))=v_in_f1(i,1:nface0(l))                              !!!    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_other_faces                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_other_faces2(nf0,nmax0,nface0,e_in_f0,v_in_f0, &
                           & nf1,nmax1,nface1,e_in_f1,v_in_f1,f0,f1,f2,l)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf0,nf1,nmax0,nmax1,f0,f1,f2,f3,l                                !!!
INTEGER:: nface0(nf0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0)                  !!!
INTEGER:: nface1(nf1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
f3=nf1                                                                       !!!
DO i=1,nf1                                                                   !!!
  IF(i.eq.f1) CYCLE                                                          !!!
  IF(i.eq.f2) CYCLE                                                          !!!
  IF(i.eq.f0) CYCLE                                                          !!!
  IF(i.eq.f3) CYCLE                                                          !!!
  ! Copy face child face i to parent face l                                  !!!
  l=l+1                                                                      !!!
  nface0(l)=nface1(i)       ! uneq_face1,f_in_f1, b_in_f1 & u_in_f1 later    !!!
  e_in_f0(l,1:nface0(l))=e_in_f1(i,1:nface0(l))                              !!!
  v_in_f0(l,1:nface0(l))=v_in_f1(i,1:nface0(l))                              !!!    
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_other_faces2                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_ddim                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
