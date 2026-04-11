!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_coor                                                            !!!
IMPLICIT NONE                                                                !!!
PRIVATE                                                                      !!!
PUBLIC:: add_a_dimer                          !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_real_face(nface,rf,nf,nv,nmax,rp,f,v_in_f,x_in_f,y_in_f)      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,nv,i1,i2,nface,nmax,f                                         !!!
INTEGER:: v_in_f(nf,nmax),x_in_f(nf,nmax),y_in_f(nf,nmax)                    !!!
REAL(KIND=8):: x(2),y(2),rf(nface,2),r1(2),r2(2),rp(nv,2)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading xy structure from .xyz                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Lattice vectors                                                            !!!
x(1)=1.0D0 ; x(2)=0.0D0                                                      !!!
y(1)=0.0D0 ; y(2)=1.0D0                                                      !!!
r2=rp(v_in_f(f,1),:)                                                         !!!
DO i=1,nface                                                                 !!!
  i1=i                                                                       !!!
  i2=i1+1                                                                    !!!
  IF(i1.eq.nface) i2=1                                                       !!!
  r1=r2                                                                      !!!
  r2=r1+rp(v_in_f(f,i2),:)+x_in_f(f,i1)*x+y_in_f(f,i1)*y-rp(v_in_f(f,i1),:)  !!!
  rf(i,:)=r1                                                                 !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_real_face                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE f0_nxy(f0,ie1,ie2,nface0,nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ia,ib,i1,i2,ie1,ie2,nface0,nmax0,e1,e2                         !!!
INTEGER:: delta,l0,nf0,e_in_f0(nf0,nmax0),f0                                 !!!
INTEGER:: x_in_f1(nf0+1,nmax0+3),y_in_f1(nf0+1,nmax0+3),x_in_f0(nf0,nmax0),y_in_f0(nf0,nmax0)
REAL(KIND=8):: rf(nface0,2),r1(2),r2(2),rm(2)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
l0=0                                                                         !!!
ia=ie2+1                                                                     !!!
ib=ie1+nface0-1                                                              !!!
IF(ie1.gt.ie2) ib=ib-nface0
DO i=ia,ib                                                                   !!!
  i1=i                                                                       !!!
  IF(i1.gt.nface0) i1=i1-nface0                                              !!!
  i2=i1+1                                                                    !!!
  IF(i2.gt.nface0) i2=i2-nface0                                              !!!
  r1=rf(i1,:)                                                                !!!
  r2=rf(i2,:)                                                                !!!
  ! Face f0                                                                  !!!
  IF(e_in_f0(f0,i1).eq.e1)THEN                                               !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l0=l0+1                                                                  !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f0,l0)=delta                                                     !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f0,l0)=delta                                                     !!!
    l0=l0+1                                                                  !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f0,l0)=delta                                                     !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f0,l0)=delta                                                     !!!
  ELSE IF(e_in_f0(f0,i1).eq.e2)THEN                                          !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l0=l0+1                                                                  !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f0,l0)=delta                                                     !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f0,l0)=delta                                                     !!!
    l0=l0+1                                                                  !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f0,l0)=delta                                                     !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f0,l0)=delta                                                     !!!
  ELSE                                                                       !!!
    l0=l0+1                                                                  !!!
    x_in_f1(f0,l0)=x_in_f0(f0,i1)                                            !!!
    y_in_f1(f0,l0)=y_in_f0(f0,i1)                                            !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Rest of face f0                                                            !!!
IF(e1.ne.e2)THEN                                                             !!!
  l0=l0+1                                                                    !!!
  r1=rf(ie1,:) ! V1                                                          !!!
  j=ie1+1                                                                    !!!
  IF(j.gt.nface0) j=j-nface0                                                 !!!
  r2=(rf(ie1,:)+rf(j,:))/2.0D0 ! V5                                          !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
  l0=l0+1                                                                    !!!
  r1=r2 ! V5                                                                 !!!
  r2=rf(ie2,:) ! V3                                                          !!!
  j=ie2+1                                                                    !!!
  IF(j.gt.nface0) j=j-nface0                                                 !!!
  r2=(r2+rf(j,:))/2.0D0 ! V6=(V3+V2)/2                                       !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
  l0=l0+1                                                                    !!!
  r1=r2 ! V6                                                                 !!!
  r2=rf(j,:) ! V2                                                            !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
ELSE                                                                         !!!
  l0=l0+1                                                                    !!!
  i1=ie1                                                                     !!!
  IF(i1.gt.nface0) i1=i1-nface0                                              !!!
  i2=i1+1                                                                    !!!
  IF(i2.gt.nface0) i2=i2-nface0                                              !!!
  r1=rf(i1,:) ! V1                                                           !!!
  rm=(rf(i2,:)-r1)/3.0D0 ! (v4-v1)/3                                         !!!
  r2=r1+rm ! V5 (1st appeareance)                                            !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
  l0=l0+1                                                                    !!!
  r1=r2 ! V5 (1st appeareance)                                               !!!
  i1=ie2                                                                     !!!
  IF(i1.gt.nface0) i1=i1-nface0                                              !!!
  r2=rf(i1,:)-rm ! V34-(v4-v1)/3 -> V6 2nd appearance                        !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
  l0=l0+1                                                                    !!!
  r1=r2 ! V6 2nd appearance                                                  !!!
  r2=r1-rm ! V5 2nd appearance -> V34-2(v4-v1)/3                             !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
  l0=l0+1                                                                    !!!
  r1=r2 ! V5 2nd appearance                                                  !!!
  r2=r1-rm ! V12                                                             !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f0,l0)=delta                                                       !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f0,l0)=delta                                                       !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE f0_nxy                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE f3_nxy(f3,f0,ie1,ie2,nface3,nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ia,ib,i1,i2,ie1,ie2,nface3,nmax0,e1,e2                         !!!
INTEGER:: delta,l3,nf0,e_in_f0(nf0,nmax0),f0,f3                              !!!
INTEGER:: x_in_f1(nf0+1,nmax0+3),y_in_f1(nf0+1,nmax0+3)                      !!!
INTEGER:: x_in_f0(nf0,nmax0),y_in_f0(nf0,nmax0)                              !!!
REAL(KIND=8):: rf(nface3,2),r1(2),r2(2),rm(2)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
l3=0                                                                         !!!
ia=ie1+1                                                                     !!!
ib=ie2-1                                                                     !!!
IF(ie1.gt.ie2) ib=ib+nface3

DO i=ia,ib                                                                   !!!
  i1=i                                                                       !!!
  IF(i1.gt.nface3) i1=i1-nface3                                              !!!
  i2=i1+1                                                                    !!!
  IF(i2.gt.nface3) i2=i2-nface3                                              !!!
  r1=rf(i1,:)                                                                !!!
  r2=rf(i2,:)                                                                !!!
  ! Face F3                                                                  !!!
  IF(e_in_f0(f0,i1).eq.e1)THEN                                               !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l3=l3+1                                                                  !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f3,l3)=delta                                                     !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f3,l3)=delta                                                     !!!
    l3=l3+1                                                                  !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f3,l3)=delta                                                     !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f3,l3)=delta                                                     !!!
  ELSE IF(e_in_f0(f0,i1).eq.e2)THEN                                          !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l3=l3+1                                                                  !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f3,l3)=delta                                                     !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f3,l3)=delta                                                     !!!
    l3=l3+1                                                                  !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f3,l3)=delta                                                     !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f3,l3)=delta                                                     !!!
  ELSE                                                                       !!!
    l3=l3+1                                                                  !!!
    x_in_f1(f3,l3)=x_in_f0(f0,i1)                                            !!!
    y_in_f1(f3,l3)=y_in_f0(f0,i1)                                            !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Rest of face f3                                                            !!!
IF(e1.ne.e2)THEN                                                             !!!
  l3=l3+1                                                                    !!!
  r1=rf(ie2,:) ! V3                                                          !!!
  j=ie2+1                                                                    !!!
  IF(j.gt.nface3) j=j-nface3                                                 !!!
  r2=(r1+rf(j,:))/2.0D0 ! V6                                                 !!!
  ! delta=FLOOR(rk(v6,1))-FLOOR(rk(v3,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v6,2))-FLOOR(rk(v3,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
  l3=l3+1                                                                    !!!
  r1=r2 ! V6                                                                 !!!
  j=ie1+1                                                                    !!!
  IF(j.gt.nface3) j=j-nface3                                                 !!!
  r2=(rf(ie1,:)+rf(j,:))/2.0D0 ! V5                                          !!!
  ! delta=FLOOR(rk(v5,1))-FLOOR(rk(v6,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v5,2))-FLOOR(rk(v6,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
  l3=l3+1                                                                    !!!
  r1=r2 ! V5                                                                 !!!
  j=ie1+1                                                                    !!!
  IF(ie1.eq.nface3) j=1                                                      !!!
  r2=rf(j,:) ! V2                                                            !!!
  ! delta=FLOOR(rk(v4,1))-FLOOR(rk(v5,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v4,2))-FLOOR(rk(v5,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
ELSE                                                                         !!!
  l3=l3+1                                                                    !!!
  r1=rf(ie2,:) ! V34                                                         !!!
  i2=ie2+1                                                                   !!!
  IF(i2.gt.nface3) i2=i2-nface3                                              !!!
  rm=(rf(i2,:)-r1)/3.0D0 ! (v1-v4)/3                                         !!!
  r2=r1+rm ! V6 2nd  appearance                                              !!!
  ! delta=FLOOR(rk(v6,1))-FLOOR(rk(v3,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v6,2))-FLOOR(rk(v3,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
  l3=l3+1                                                                    !!!
  r1=r2 ! V6 2nd  appearance                                                 !!!
  r2=rf(ie1,:)-rm ! V5 1st  appearance - V12-(v1-v4)/3                       !!!
  ! delta=FLOOR(rk(v5,1))-FLOOR(rk(v6,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v5,2))-FLOOR(rk(v6,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
  l3=l3+1                                                                    !!!
  r1=r2    ! V5 1st  appearance                                              !!!
  r2=r1-rm ! V6 1st  appearance                                              !!!
  ! delta=FLOOR(rk(v5,1))-FLOOR(rk(v6,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v5,2))-FLOOR(rk(v6,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
  l3=l3+1                                                                    !!!
  r1=r2    ! V6 1st  appearance                                              !!!
  r2=r1-rm ! V43                                                             !!!
  ! delta=FLOOR(rk(v4,1))-FLOOR(rk(v5,1))                                    !!!
  delta=FLOOR(r2(1))-FLOOR(r1(1))                                            !!!
  x_in_f1(f3,l3)=delta                                                       !!!
  ! delta=FLOOR(rk(v4,2))-FLOOR(rk(v5,2))                                    !!!
  delta=FLOOR(r2(2))-FLOOR(r1(2))                                            !!!
  y_in_f1(f3,l3)=delta                                                       !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE f3_nxy                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE f12_nxy(f12,nface12,nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,i1,i2,nface12,nmax0,e1,e2                                        !!!
INTEGER:: delta,l12,nf0,e_in_f0(nf0,nmax0),f12                               !!!
INTEGER:: x_in_f1(nf0+1,nmax0+3),y_in_f1(nf0+1,nmax0+3),x_in_f0(nf0,nmax0),y_in_f0(nf0,nmax0)
REAL(KIND=8):: rf(nface12,2),r1(2),r2(2),rm(2)                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
l12=0                                                                        !!!
DO i=1,nface12                                                               !!!
  i1=i                                                                       !!!
  i2=i1+1                                                                    !!!
  IF(i2.gt.nface12) i2=i2-nface12                                            !!!
  r1=rf(i1,:)                                                                !!!
  r2=rf(i2,:)                                                                !!!
  IF(e_in_f0(f12,i1).eq.e1)THEN                                              !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l12=l12+1                                                                !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f12,l12)=delta                                                   !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f12,l12)=delta                                                   !!!
    l12=l12+1                                                                !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f12,l12)=delta                                                   !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f12,l12)=delta                                                   !!!
  ELSE IF(e_in_f0(f12,i1).eq.e2)THEN                                         !!!
    rm=(r1+r2)*0.5D0                                                         !!!
    l12=l12+1                                                                !!!
    delta=FLOOR(rm(1))-FLOOR(r1(1))                                          !!!
    x_in_f1(f12,l12)=delta                                                   !!!
    delta=FLOOR(rm(2))-FLOOR(r1(2))                                          !!!
    y_in_f1(f12,l12)=delta                                                   !!!
    l12=l12+1                                                                !!!
    delta=FLOOR(r2(1))-FLOOR(rm(1))                                          !!!
    x_in_f1(f12,l12)=delta                                                   !!!
    delta=FLOOR(r2(2))-FLOOR(rm(2))                                          !!!
    y_in_f1(f12,l12)=delta                                                   !!!
  ELSE                                                                       !!!
    l12=l12+1                                                                !!!
    x_in_f1(f12,l12)=x_in_f0(f12,i)                                          !!!
    y_in_f1(f12,l12)=y_in_f0(f12,i)                                          !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE f12_nxy                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE add_a_dimer(f0,ie1,ie2,rp, &                                      !!!
       & nf0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0,x_in_f0,y_in_f0, &     !!!
       & nf1,nv1,nmax1,nface1,x_in_f1,y_in_f1,rk)                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf0,nv0,nf1,nv1,ie1,ie2                                        !!!
INTEGER:: f0,f1,f2,f3,e1,e2,v1,v2,v3,v4,v5,v6,i1,i2                          !!!
INTEGER:: nmax0,nmax1                                                        !!!
INTEGER:: iv5x,iv5y,iv6x,iv6y                                                !!!
INTEGER:: nface0(nf0)                                                        !!!
INTEGER:: f_in_f0(nf0,nmax0),e_in_f0(nf0,nmax0),v_in_f0(nf0,nmax0)           !!!
INTEGER:: x_in_f0(nf0,nmax0),y_in_f0(nf0,nmax0)                              !!!
INTEGER:: nface1(nf1)                                                        !!!
INTEGER,ALLOCATABLE:: x_in_f1(:,:),y_in_f1(:,:)                              !!!
REAL(KIND=8):: rp(nv0,2)                                                     !!!
REAL(KIND=8),ALLOCATABLE:: rk(:,:),rf(:,:)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adding dimer info                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Who's V1?                                                                  !!!
! f0=     ! Who's F0?                                                        !!!
! ie1=    ! ie1 gives E1                                                     !!!
e1=e_in_f0(f0,ie1)                                                           !!!
v1=v_in_f0(f0,ie1)  ! V1 is E1's starting vertex                             !!!
! Who's V4?                                                                  !!!
j=ie1+1 ! Ej is the edge after E1.                                           !!!
IF(ie1.eq.nface0(f0)) j=1 ! Correct loop limit                               !!!
v4=v_in_f0(f0,j)   ! V4 is Ej's starting vertex (V1-V4 make E1)              !!!
! Who's V3?                                                                  !!!
! ie2=               ! ie2 gives E2                                          !!!
e2=e_in_f0(f0,ie2)                                                           !!!
v3=v_in_f0(f0,ie2) ! V3 is E2's starting vertex                              !!!
! Who's V2?                                                                  !!!
j=ie2+1 ! Ej is the edge after E2?                                           !!!
IF(ie2.eq.nface0(f0)) j=1 ! Correct loop limit                               !!!
v2=v_in_f0(f0,j)   ! V2 is Ej's starting vertex (V3-V2 make E1)              !!!
f1=f_in_f0(f0,ie1)     ! Face touching E1                                    !!!
f2=f_in_f0(f0,ie2)     ! Face touching E2                                    !!!
f3=nf0+1                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                     !!!
! Expansion                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                     !!!
v5=nv0+1                                                                     !!!
v6=nv0+2                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating child .nxy data                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(ALLOCATED(x_in_f1)) stop 'aaaaaaa'
IF(ALLOCATED(y_in_f1)) stop 'aaaaaaa'
ALLOCATE(x_in_f1(nf1,nmax1),y_in_f1(nf1,nmax1))                              !!!
x_in_f1=0                                                                    !!!
y_in_f1=0                                                                    !!!
! Copying unchanged parent faces                                             !!!
DO i=1,nf1                                                                   !!!
  IF(i.eq.f0) CYCLE                                                          !!!
  IF(i.eq.f1) CYCLE                                                          !!!
  IF(i.eq.f2) CYCLE                                                          !!!
  IF(i.eq.f3) CYCLE                                                          !!!
  x_in_f1(i,1:nface1(i))=x_in_f0(i,1:nface0(i))                              !!!
  y_in_f1(i,1:nface1(i))=y_in_f0(i,1:nface0(i))                              !!!
END DO                                                                       !!!
! Creating data for new face F1                                              !!!
IF(f1.ne.f0)THEN                                                             !!!
  ! Old F1 face                                                              !!!
  ALLOCATE(rf(nface0(f1),2))                                                 !!!
  CALL get_real_face(nface0(f1),rf,nf0,nv0,nmax0,rp,f1,v_in_f0,x_in_f0,y_in_f0)
  ! Data for new F1 faces                                                    !!!
  CALL f12_nxy(f1,nface0(f1),nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
  DEALLOCATE(rf)                                                             !!!
END IF                                                                       !!!
! Creating data for new face F2                                              !!!
IF((f2.ne.f0).AND.(f2.ne.f1))THEN                                            !!!
  ! Old F2 face                                                              !!!
  ALLOCATE(rf(nface0(f2),2))                                                 !!!
  CALL get_real_face(nface0(f2),rf,nf0,nv0,nmax0,rp,f2,v_in_f0,x_in_f0,y_in_f0)
  ! Data for new F1 faces                                                    !!!
  CALL f12_nxy(f2,nface0(f2),nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
  DEALLOCATE(rf)                                                             !!!
END IF                                                                       !!!
! Old F0 face                                                                !!!
ALLOCATE(rf(nface0(f0),2))                                                   !!!
CALL get_real_face(nface0(f0),rf,nf0,nv0,nmax0,rp,f0,v_in_f0,x_in_f0,y_in_f0)!!!
! Data for new F0 face                                                       !!!
CALL f0_nxy(f0,ie1,ie2,nface0(f0),nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
! Data for new F3 face                                                       !!!
CALL f3_nxy(f3,f0,ie1,ie2,nface0(f0),nmax0,nf0,rf,e1,e2,e_in_f0,x_in_f0,y_in_f0,x_in_f1,y_in_f1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating child structure                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Allocating child structure                                                 !!!
IF(ALLOCATED(rk)) stop 'aaaaaaa'
ALLOCATE(rk(nv1,2))                                                          !!!
! Copying parent structure                                                   !!!
rk(1:nv0,:)=rp                                                               !!!
! Creating new vertices                                                      !!!
IF(e1.ne.e2)THEN                                                             !!!
  i1=ie1                                                                     !!!
  i2=ie1+1                                                                   !!!
  IF(i1.eq.nface0(f0)) i2=1                                                  !!!
  rk(v5,:)=(rf(i1,:)+rf(i2,:))*0.5D0                                         !!!
  i1=ie2                                                                     !!!
  i2=ie2+1                                                                   !!!
  IF(i1.eq.nface0(f0)) i2=1                                                  !!!
  rk(v6,:)=(rf(i1,:)+rf(i2,:))*0.5D0                                         !!!
ELSE                                                                         !!!
  i1=ie1                                                                     !!!
  i2=ie1+1                                                                   !!!
  IF(i1.eq.nface0(f0)) i2=1                                                  !!!
  rk(v5,:)=rf(i1,:)+(rf(i2,:)-rf(i1,:))/3.0D0                                !!!
  rk(v6,:)=rf(i1,:)+2.0D0*(rf(i2,:)-rf(i1,:))/3.0D0                          !!!
END IF                                                                       !!!
! Correct v5/v6 coordinates                                                  !!!
iv5x=FLOOR(rk(v5,1))                                                         !!!
iv5y=FLOOR(rk(v5,2))                                                         !!!
iv6x=FLOOR(rk(v6,1))                                                         !!!
iv6y=FLOOR(rk(v6,2))                                                         !!!
IF(iv5x.ne.0) rk(v5,1)=rk(v5,1)-iv5x*1.0D0                                   !!!
IF(iv5y.ne.0) rk(v5,2)=rk(v5,2)-iv5y*1.0D0                                   !!!
IF(iv6x.ne.0) rk(v6,1)=rk(v6,1)-iv6x*1.0D0                                   !!!
IF(iv6y.ne.0) rk(v6,2)=rk(v6,2)-iv6y*1.0D0                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE add_a_dimer                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_coor                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
