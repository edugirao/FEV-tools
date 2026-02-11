!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_adim                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
PRIVATE:: sorting,swaptwo                                                    !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE f_in_faces(nflags,flag,neigh_flag,nf,nface,f_in_f,b_in_f,lmax)    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf,nflags,flag(nflags,3),neigh_flag(nflags,3),nface(nf)      !!!
INTEGER:: lmax,f_in_f(nf,lmax),last                                          !!!
LOGICAL:: b_in_f(nf,lmax)                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! f_in_f(i,j) -> neighbor face by i-th face                                  !!!
! e_in_f(i,j) -> j-th edge in i-th face                                      !!!
! v_in_f(i,j) -> j-th vertex in i-th face                                    !!!
! -> e_in_f(i,j) starts in v_in_f(i,j) and finishes in v_in_f(i,j+1)         !!!
! -> They're sorted by appearance around the face the face.                  !!!
! -> Bridges appear twice. Vertices on bridges appear twice.                 !!!
f_in_f=0                                                                     !!!
b_in_f=.false.                                                               !!!
DO i=1,nf                                                                    !!!
  DO j=1,nflags                                                              !!!
    IF(flag(j,1).eq.i)THEN                                                   !!!
      l=1                                                                    !!!
      f_in_f(i,l)=flag(neigh_flag(j,1),1)                                    !!!
      IF(f_in_f(i,l).eq.i) b_in_f(i,l)=.true.                                !!!
      last=j                                                                 !!!
      DO                                                                     !!!
        l=l+1                                                                !!!
        last=neigh_flag(last,3)                                              !!!
        last=neigh_flag(last,2)                                              !!!
        f_in_f(i,l)=flag(neigh_flag(last,1),1)                               !!!
        IF(f_in_f(i,l).eq.i) b_in_f(i,l)=.true.                              !!!
        IF(l.eq.nface(i)) EXIT                                               !!!
      END DO                                                                 !!!
      EXIT                                                                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE f_in_faces                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE create_f12(nf,nface,nmax,e_in_f,v_in_f,fi,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,l,e_in_f12(nmax),v_in_f12(nmax)                                  !!!
INTEGER:: nf,nface,nmax,e_in_f(nf,nmax),v_in_f(nf,nmax)                      !!!
INTEGER:: fi,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
e_in_f12=0                                                                   !!!
v_in_f12=0                                                                   !!!
! Adding one edge/vertex at a time                                           !!!
l=0 ! Starting counter                                                       !!!
DO i=1,nface                                                                 !!!
  IF((e_in_f(fi,i).ne.e1).AND.(e_in_f(fi,i).ne.e2))THEN                      !!!
    ! Old edges                                                              !!!
    l=l+1                                                                    !!!
    e_in_f12(l)=e_in_f(fi,i)                                                 !!!
    v_in_f12(l)=v_in_f(fi,i)                                                 !!!
  ELSE IF(e_in_f(fi,i).eq.e1)THEN                                            !!!
    ! New edges                                                              !!!
    IF(v_in_f(fi,i).eq.v1)THEN                                               !!!
      ! V1-to-V4 orientation                                                 !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e1                                                         !!!
      v_in_f12(l)=v1                                                         !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e4                                                         !!!
      v_in_f12(l)=v5                                                         !!!
    ELSE IF(v_in_f(fi,i).eq.v4)THEN                                          !!!
      ! V4-to-V1 orientation                                                 !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e4                                                         !!!
      v_in_f12(l)=v4                                                         !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e1                                                         !!!
      v_in_f12(l)=v5                                                         !!!
    ELSE                                                                     !!!
      STOP 'problem in correction 12a'                                       !!!
    END IF                                                                   !!!
  ELSE IF(e_in_f(fi,i).eq.e2)THEN                                            !!!
    ! New edges                                                              !!!
    IF(v_in_f(fi,i).eq.v2)THEN                                               !!!
      ! V2-to-V3 orientation                                                 !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e2                                                         !!!
      v_in_f12(l)=v2                                                         !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e3                                                         !!!
      v_in_f12(l)=v6                                                         !!!
    ELSE IF(v_in_f(fi,i).eq.v3)THEN                                          !!!
      ! V3-to-V2 orientation                                                 !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e3                                                         !!!
      v_in_f12(l)=v3                                                         !!!
      l=l+1                                                                  !!!
      e_in_f12(l)=e2                                                         !!!
      v_in_f12(l)=v6                                                         !!!
    ELSE                                                                     !!!
      STOP 'problem in correction 12b'                                       !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
nface=l                                                                      !!!
e_in_f(fi,1:nface)=e_in_f12(1:nface)                                         !!!
e_in_f(fi,nface+1:nmax)=0                                                    !!!
v_in_f(fi,1:nface)=v_in_f12(1:nface)                                         !!!
v_in_f(fi,nface+1:nmax)=0                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE create_f12                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE create_f0(nf,nface,nmax,e_in_f,v_in_f,f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6,ie1,ie2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,e_in_f0(nmax),v_in_f0(nmax),ie1,ie2                          !!!
INTEGER:: nf,nface,nmax,e_in_f(nf,nmax),v_in_f(nf,nmax)                      !!!
INTEGER:: f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Start/end V2                                                               !!!
e_in_f0=0                                                                    !!!
v_in_f0=0                                                                    !!!
l=0                                                                          !!!
DO i=ie2+1,nface+ie1-1                                                       !!!
  j=i                                                                        !!!
  IF(i.gt.nface) j=j-nface                                                   !!!
  IF((e_in_f(f0,j).ne.e1).AND.(e_in_f(f0,j).ne.e2))THEN                      !!!
    ! Old edges                                                              !!!
    l=l+1                                                                    !!!
    e_in_f0(l)=e_in_f(f0,j)                                                  !!!
    v_in_f0(l)=v_in_f(f0,j)                                                  !!!
  ELSE IF(e_in_f(f0,j).eq.e1)THEN                                            !!!
    IF(v_in_f(f0,j).eq.v1) STOP 'problem in correction f0a'                  !!!
    ! Necessarily V4-to-V1 orientation                                       !!!
    l=l+1                                                                    !!!
    e_in_f0(l)=e4                                                            !!!
    v_in_f0(l)=v4                                                            !!!
    l=l+1                                                                    !!!
    e_in_f0(l)=e1                                                            !!!
    v_in_f0(l)=v5                                                            !!!
  ELSE IF(e_in_f(f0,j).eq.e2)THEN                                            !!!
    IF(v_in_f(f0,j).eq.v3) STOP 'problem in correction f0b'                  !!!
    ! Necessarily V2-to-V3 orientation                                       !!!
    l=l+1                                                                    !!!
    e_in_f0(l)=e2                                                            !!!
    v_in_f0(l)=v2                                                            !!!
    l=l+1                                                                    !!!
    e_in_f0(l)=e3                                                            !!!
    v_in_f0(l)=v6                                                            !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Closing the ring with the new edges                                        !!!
l=l+1                                                                        !!!
e_in_f0(l)=e1                                                                !!!
v_in_f0(l)=v1                                                                !!!
l=l+1                                                                        !!!
e_in_f0(l)=e0                                                                !!!
v_in_f0(l)=v5                                                                !!!
IF(e1.ne.e2)THEN                                                             !!!
  l=l+1                                                                      !!!
  e_in_f0(l)=e2                                                              !!!
  v_in_f0(l)=v6                                                              !!!
ELSE                                                                         !!!
  l=l+1                                                                      !!!
  e_in_f0(l)=e3                                                              !!!
  v_in_f0(l)=v6                                                              !!!
  l=l+1                                                                      !!!
  e_in_f0(l)=e1                                                              !!!
  v_in_f0(l)=v5                                                              !!!
END IF                                                                       !!!
nface=l                                                                      !!!
e_in_f(f0,1:nface)=e_in_f0(1:nface)                                          !!!
e_in_f(f0,nface+1:nmax)=0                                                    !!!
v_in_f(f0,1:nface)=v_in_f0(1:nface)                                          !!!
v_in_f(f0,nface+1:nmax)=0                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE create_f0                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE create_f3(nf,nface3,nmax,e_in_f,v_in_f,f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6,ie1,ie2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER:: i,l,e_in_f3(nmax),v_in_f3(nmax),ie1,ie2                            !!!
INTEGER:: nf,nface3,nmax,e_in_f(nf,nmax),v_in_f(nf,nmax)                     !!!
INTEGER:: f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                                !!!
! Start/end in V4                                                            !!!
e_in_f3=0                                                                    !!!
v_in_f3=0                                                                    !!!
l=0                                                                          !!!
DO i=ie1+1,ie2-1                                                             !!!
  IF((e_in_f(f0,i).ne.e1).AND.(e_in_f(f0,i).ne.e2))THEN                      !!!
    ! Old edges                                                              !!!
    l=l+1                                                                    !!!
    e_in_f3(l)=e_in_f(f0,i)                                                  !!!
    v_in_f3(l)=v_in_f(f0,i)                                                  !!!
  ELSE IF(e_in_f(f0,i).eq.e1)THEN                                            !!!
    IF(v_in_f(f0,i).eq.v1) STOP 'problem in correction f3a'                  !!!
    ! Necessarily V4-to-V1 orientation                                       !!!
    l=l+1                                                                    !!!
    e_in_f3(l)=e4                                                            !!!
    v_in_f3(l)=v4                                                            !!!
    l=l+1                                                                    !!!
    e_in_f3(l)=e1                                                            !!!
    v_in_f3(l)=v5                                                            !!!
  ELSE IF(e_in_f(f0,i).eq.e2)THEN                                            !!!
    IF(v_in_f(f0,i).eq.v3) STOP 'problem in correction f3b'                  !!!
    ! Necessarily V2-to-V3 orientation                                       !!!
    l=l+1                                                                    !!!
    e_in_f3(l)=e2                                                            !!!
    v_in_f3(l)=v2                                                            !!!
    l=l+1                                                                    !!!
    e_in_f3(l)=e3                                                            !!!
    v_in_f3(l)=v6                                                            !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Closing the ring with the new edges                                        !!!
IF(e1.ne.e2)THEN                                                             !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e3                                                              !!!
  v_in_f3(l)=v3                                                              !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e0                                                              !!!
  v_in_f3(l)=v6                                                              !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e4                                                              !!!
  v_in_f3(l)=v5                                                              !!!
ELSE                                                                         !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e4                                                              !!!
  v_in_f3(l)=v4                                                              !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e0                                                              !!!
  v_in_f3(l)=v6                                                              !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e3                                                              !!!
  v_in_f3(l)=v5                                                              !!!
  l=l+1                                                                      !!!
  e_in_f3(l)=e4                                                              !!!
  v_in_f3(l)=v6                                                              !!!
END IF                                                                       !!!
nface3=l                                                                     !!!
e_in_f(nf,1:l)=e_in_f3(1:l)                                                  !!!
v_in_f(nf,1:l)=v_in_f3(1:l)                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE create_f3                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_lname(nf,nface,tmpfile)                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nface(nf),f(nf),i                                               !!!
CHARACTER*100:: tmpfile,tmp                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
f=nface                                                                      !!!
CALL sorting(nf,f)                                                           !!!
WRITE(tmpfile,'(I0)') f(1)                                                   !!!
DO i=2,nf                                                                    !!!
  WRITE(tmp,'(I0)') f(i)                                                     !!!
  tmpfile=TRIM(ADJUSTL(tmpfile))//'-'//TRIM(ADJUSTL(tmp))                    !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
END SUBROUTINE get_lname                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE sorting(n,x)                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to sort the elements of a vector in ascending order             !!!
! by using the selection algorithm.                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT none                                                                !!!
INTEGER :: n,i,j,k,x(n)                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Proceeding with selection                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,n                   ! looking for i-th smallest guy                   !!!
  k=i                      ! The i-th guy is the first candidate             !!!
  DO j=i+1,n               ! We look at the guys on its right                !!!
    IF(x(j).le.x(k)) k=j   ! If we find a smaller guy, we...                 !!!
  END DO                   ! ... have a new candidate                        !!!
  CALL swaptwo(x(i),x(k))  ! The end: i-th smallest get spot i               !!!
END DO                     ! We go now for (i+1)-th smallest                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE sorting                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE swaptwo(a,b)                                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine to swap two real numbers                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT none                                                                !!!
INTEGER:: a,b,tmp                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Swapping numbers                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tmp=a ! Saving "a" value in a safe place                                     !!!
a=b   ! Store "b" value in the place of "a"                                  !!!
b=tmp ! Store the original "a" value on the place of "b"                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE swaptwo                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_adim                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
