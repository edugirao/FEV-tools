!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE build_flags(nflags,flag,nf,ne,nv,fev) ! b**                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: k,nf,ne,nv,fev(nf,ne,nv)                                           !!!
INTEGER:: nflags,flag(nflags,3)                                              !!!
INTEGER:: e(3),f(3)                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! We define the flag set for each vertex                                     !!!
DO k=1,nv                                                                    !!!
  ! Getting faces and edges from vertex k                                    !!!
  CALL fe_for_v(nf,ne,nv,fev,f,e,k)                                          !!!
  ! Set the vertex for the set                                               !!!
  flag(6*(k-1)+1:6*k,3)=k                                                    !!!
  ! Set the edges for the set                                                !!!
  flag(6*(k-1)+1:6*(k-1)+2,2)=e(1) ! 1st/2nd flags get 1st connected edge    !!!
  flag(6*(k-1)+3:6*(k-1)+4,2)=e(2) ! 3rd/4th flags get 2nd connected edge    !!!
  flag(6*(k-1)+5:6*(k-1)+6,2)=e(3) ! 5th/6th flags get 3rd connected edge    !!!
  ! Set the faces for the set                                                !!!
  flag(6*(k-1)+1,1)=f(3)                                                     !!!
  flag(6*(k-1)+2,1)=f(1)                                                     !!!
  flag(6*(k-1)+3,1)=f(1)                                                     !!!
  flag(6*(k-1)+4,1)=f(2)                                                     !!!
  flag(6*(k-1)+5,1)=f(2)                                                     !!!
  flag(6*(k-1)+6,1)=f(3)                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE build_flags                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fe_for_v(nf,ne,nv,fev,f,e,k)                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,k,l,l1,l2,l3,nf,ne,nv,fev(nf,ne,nv)                            !!!
INTEGER:: reduction_ev,e(3),f(3)                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting edges from vertex k                                                !!!
e=0 ; l=1                                                                    !!!
DO j=1,ne                                                                    !!!
  ! Checking if edge j contains vertex k                                     !!!
  reduction_ev=SUM(fev(:,j,k))                                               !!!
  IF((reduction_ev.eq.1).OR.(reduction_ev.eq.2))THEN                         !!!
    e(l)=j                                                                   !!!
    l=l+1                                                                    !!!
    IF(l.eq.4) EXIT                                                          !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Getting faces from vertex k                                                !!!
f=0 ; l=0                                                                    !!!
DO i=1,nf                                                                    !!!
  DO l1=1,3                                                                  !!!
    l2=l1+1                                                                  !!!
    l3=l1+2                                                                  !!!
    IF(l1.eq.3) l2=1                                                         !!!
    IF(l3.gt.3) l3=l3-3                                                      !!!
    ! Checking if face i contains edges e(l1)/e(l2) at vertex k              !!!
    IF((fev(i,e(l1),k).eq.1).AND.(fev(i,e(l2),k).eq.1))THEN                  !!!
      ! Correction ot the single-bridge-vertex case                          !!!
      IF(f(l1).ne.0)THEN                                                     !!!
        IF((SUM(fev(:,e(l3),:)).eq.2).AND.(fev(i,e(l3),k).eq.0)) f(l1)=i     !!!
        CYCLE                                                                !!!
      END IF                                                                 !!!
      ! Assigning the face                                                   !!!
      f(l1)=i                                                                !!!
      l=l+1                                                                  !!!
      IF(l.eq.4) EXIT                                                        !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(l.eq.4) EXIT                                                            !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fe_for_v                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neigh_f(nflags,neigh_flag,nv)                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: k,nv,nflags,neigh_flag(nflags,3)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is facilitated by the particular way the flags                        !!!
! were constructed and ordered around the vertex.                            !!!
DO k=1,nv                                                                    !!!
  neigh_flag(6*(k-1)+1,1)=6*(k-1)+2                                          !!!
  neigh_flag(6*(k-1)+2,1)=6*(k-1)+1                                          !!!
  neigh_flag(6*(k-1)+3,1)=6*(k-1)+4                                          !!!
  neigh_flag(6*(k-1)+4,1)=6*(k-1)+3                                          !!!
  neigh_flag(6*(k-1)+5,1)=6*(k-1)+6                                          !!!
  neigh_flag(6*(k-1)+6,1)=6*(k-1)+5                                          !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE neigh_f                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neigh_e(nflags,neigh_flag,nv)                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: k,nv,nflags,neigh_flag(nflags,3)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is facilitated by the particular way the flags                        !!!
! were constructed and ordered around the vertex.                            !!!
DO k=1,nv                                                                    !!!
  neigh_flag(6*(k-1)+2,2)=6*(k-1)+3                                          !!!
  neigh_flag(6*(k-1)+3,2)=6*(k-1)+2                                          !!!
  neigh_flag(6*(k-1)+4,2)=6*(k-1)+5                                          !!!
  neigh_flag(6*(k-1)+5,2)=6*(k-1)+4                                          !!!
  neigh_flag(6*(k-1)+6,2)=6*(k-1)+1                                          !!!
  neigh_flag(6*(k-1)+1,2)=6*(k-1)+6                                          !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE neigh_e                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neigh_v(ne,nv,nflags,flag,neigh_flag,reduc_ev)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: k,l,j,kk,i1,i2,ne,nv,nflags,flag(nflags,3)                         !!!
INTEGER:: neigh_flag(nflags,3),reduc_ev(ne,nv)                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO k=1,nv                                                                    !!!
  ! For each vertex, go through its 3 emerging edges                         !!!
  DO l=1,3                                                                   !!!
    ! Let's take the l-th edge emerging from vertex k                        !!!
    j=flag(6*(k-1)+2*(l-1)+1,2)                                              !!!
    ! Don't go if the edge is a bridge                                       !!!
    IF(reduc_ev(j,k).eq.1) CYCLE                                             !!!
    ! Look for the other vertex connected by this edge                       !!!
    DO kk=k+1,nv                                                             !!!
      IF(reduc_ev(j,kk).eq.2)THEN                                            !!!
        DO i1=2*(l-1)+1,2*(l-1)+2                                            !!!
          DO i2=1,6                                                          !!!
            IF((flag(6*(k-1)+i1,1).eq.flag(6*(kk-1)+i2,1)).AND. &            !!!
             & (flag(6*(k-1)+i1,2).eq.flag(6*(kk-1)+i2,2)))THEN              !!!
              neigh_flag(6*(k-1)+i1,3)=6*(kk-1)+i2                           !!!
              neigh_flag(6*(kk-1)+i2,3)=6*(k-1)+i1                           !!!
              EXIT                                                           !!!
            END IF                                                           !!!
          END DO                                                             !!!
        END DO                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE neigh_v                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE first_neighbors(nflags,neigh_flag,m2)                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nflags,m2(nflags,nflags),neigh_flag(nflags,3)                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
m2=0                                                                         !!!
DO i=1,nflags                                                                !!!
  DO j=1,3                                                                   !!!
    l=neigh_flag(i,j)                                                        !!!
    m2(i,l)=1                                                                !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE first_neighbors                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE second_neighbors(nflags,flag,m2,m1,flag_color)                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nflags,flag(nflags,3),m2(nflags,nflags),flag_color(nflags)     !!!
INTEGER:: m1(nflags,nflags)                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
m1=0                                                                         !!!
DO i=1,nflags                                                                !!!
  DO j=i+1,nflags                                                            !!!
    IF(flag(i,1).eq.flag(j,1))THEN                                           !!!
      IF(m2(i,j).ne.0) CYCLE                                                 !!!
      IF(flag_color(i)*flag_color(j).eq.-1) CYCLE                            !!!
      m1(i,j)=1                                                              !!!
      m1(j,i)=1                                                              !!!
      CYCLE                                                                  !!!
    END IF                                                                   !!!
    IF(flag(i,2).eq.flag(j,2))THEN                                           !!!
      IF(m2(i,j).ne.0) CYCLE                                                 !!!
      IF(flag_color(i)*flag_color(j).eq.-1) CYCLE                            !!!
      m1(i,j)=1                                                              !!!
      m1(j,i)=1                                                              !!!
      CYCLE                                                                  !!!
    END IF                                                                   !!!
    IF(flag(i,3).eq.flag(j,3))THEN                                           !!!
      IF(m2(i,j).ne.0) CYCLE                                                 !!!
      IF(flag_color(i)*flag_color(j).eq.-1) CYCLE                            !!!
      m1(i,j)=1                                                              !!!
      m1(j,i)=1                                                              !!!
      CYCLE                                                                  !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE second_neighbors                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fcs2flg(nf,nmax,nface,e_in_f,v_in_f,nflags,flag,neigh_flag,flag_color)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags),nface(nf)   !!!
INTEGER:: e_in_f(nf,nmax),v_in_f(nf,nmax)                                    !!!
INTEGER:: nf,nmax,nflags                                                     !!!
INTEGER:: i,j,l,l0,f                                                         !!!
LOGICAL:: done(nflags),out                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Building flags, e-neighs and v-neighs                                      !!!
l=0                                                                          !!!
flag=0                                                                       !!!
neigh_flag=0                                                                 !!!
DO f=1,nf                                                                    !!!
  l0=l+1                                                                     !!!
  DO j=1,nface(f)-1                                                          !!!
    l=l+1                                                                    !!!
    flag(l,1)=f                                                              !!!
    flag(l,2)=e_in_f(f,j)                                                    !!!
    flag(l,3)=v_in_f(f,j)                                                    !!!
    IF(j.ne.1) neigh_flag(l,2)=l-1                                           !!!
    neigh_flag(l,3)=l+1                                                      !!!
    l=l+1                                                                    !!!
    flag(l,1)=f                                                              !!!
    flag(l,2)=e_in_f(f,j)                                                    !!!
    flag(l,3)=v_in_f(f,j+1)                                                  !!!
    neigh_flag(l,2)=l+1                                                      !!!
    neigh_flag(l,3)=l-1                                                      !!!
  END DO                                                                     !!!
  j=nface(f)                                                                 !!!
  l=l+1                                                                      !!!
  flag(l,1)=f                                                                !!!
  flag(l,2)=e_in_f(f,j)                                                      !!!
  flag(l,3)=v_in_f(f,j)                                                      !!!
  neigh_flag(l,2)=l-1                                                        !!!
  neigh_flag(l,3)=l+1                                                        !!!
  l=l+1                                                                      !!!
  flag(l,1)=f                                                                !!!
  flag(l,2)=e_in_f(f,j)                                                      !!!
  flag(l,3)=v_in_f(f,1)                                                      !!!
  neigh_flag(l,2)=l0                                                         !!!
  neigh_flag(l,3)=l-1                                                        !!!
  neigh_flag(l0,2)=l                                                         !!!
END DO                                                                       !!!
! Building f-neighs                                                          !!!
DO i=1,nflags                                                                !!!
  IF(neigh_flag(i,1).ne.0) CYCLE                                             !!!
  DO j=i+1,nflags                                                            !!!
    IF(flag(i,2).eq.flag(j,2))THEN                                           !!!
      IF(flag(i,3).eq.flag(j,3))THEN                                         !!!
        neigh_flag(i,1)=j                                                    !!!
        neigh_flag(j,1)=i                                                    !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Coloring flags                                                             !!!
flag_color=0                                                                 !!!
flag_color(1)=1                                                              !!!
done=.false.                                                                 !!!
DO j=1,nflags ! at most, does not need to go until nflags                    !!!
  out=.true.                                                                 !!!
  DO i=1,nflags                                                              !!!
    IF(flag_color(i).ne.0)THEN                                               !!!
      IF(.not.done(i))THEN                                                   !!!
        flag_color(neigh_flag(i,:))=-flag_color(i)                           !!!
        done(i)=.true.                                                       !!!
        out=.false.                                                          !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(out) EXIT                                                               !!!
END DO                                                                       !!!
DO i=1,nflags                                                                !!!
  IF(flag_color(i)*flag_color(neigh_flag(i,1)).ne.-1) stop 'aaaa'            !!!
  IF(flag_color(i)*flag_color(neigh_flag(i,2)).ne.-1) stop 'aaaa'            !!!
  IF(flag_color(i)*flag_color(neigh_flag(i,3)).ne.-1) stop 'aaaa'            !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fcs2flg                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
