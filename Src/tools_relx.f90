!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_relx                                                            !!!
IMPLICIT NONE                                                                !!!
PRIVATE                                                                      !!!
PUBLIC:: make_graphene,xyz_optimization                          !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE xyz_optimization(nf,nv,nmax,nface,v_in_f,x_in_f,y_in_f,r,filename)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf,nv,nmax,makesvgpdf                                          !!!
INTEGER:: nn(nv),neigh(nv,3),neigh_x(nv,3),neigh_y(nv,3)                     !!!
INTEGER:: nface(nf),nstepmax                                                 !!!
INTEGER:: v_in_f(nf,nmax),x_in_f(nf,nmax),y_in_f(nf,nmax)                    !!!
CHARACTER*100:: filename                                                     !!!
REAL(KIND=8):: d0,dmax,pi,k,ka,q,dt                                          !!!
REAL(KIND=8):: r(nv,2),f(nv,2)                                               !!!
LOGICAL:: yes                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
pi=DACOS(-1.0D0)                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! nxy to neigh                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL nxy2neigh(nf,nface,nmax,v_in_f,x_in_f,y_in_f,nv,nn,neigh,neigh_x,neigh_y) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reorder neighbors to have cyclic sequence                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL order_neigh(nv,r,neigh,neigh_x,neigh_y)                                 !!!
                                                                             !!!
INQUIRE(FILE='relax.par',EXIST=yes)                                          !!!
IF(yes)THEN                                                                  !!!
  OPEN(UNIT=58,FILE='relax.par')                                             !!!
  READ(58,*) k                                                               !!!
  READ(58,*) ka                                                              !!!
  READ(58,*) q                                                               !!!
  READ(58,*) dt                                                              !!!
  READ(58,*) nstepmax                                                        !!!
  READ(58,*) makesvgpdf                                                      !!!
  CLOSE(UNIT=58)                                                             !!!
ELSE                                                                         !!!
  ! Spring k                                                                 !!!
  k=1.0D0                                                                    !!!
  ! Angular spring k                                                         !!!
  ka=8.0D0                                                                   !!!
  ! Repulsive Coulomb force constant                                         !!!
  q=1.0D0                                                                    !!!
  ! Time increment (force to displacement scale)                             !!!
  dt=0.001                                                                   !!!
  ! Max number of geometry steps                                             !!!
  nstepmax=10000                                                             !!!
  ! Create svg/pdf                                                           !!!
  makesvgpdf=1                                                               !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Optimization                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO j=1,0!nstepmax                                                              !!!
  ! Average bond distance                                                    !!!
  d0=d_ave(nv,r,neigh,neigh_x,neigh_y)                                       !!!
  ! Forces                                                                   !!!
  CALL force(nv,r,neigh,neigh_x,neigh_y,d0,f,k,ka,q,dt)                      !!!
  ! Getting max disp                                                         !!!
  dmax=0                                                                     !!!
  DO i=1,nv                                                                  !!!
    IF(DSQRT(SUM(f(i,:)**2)).gt.dmax) dmax=DSQRT(SUM(f(i,:)**2))             !!!
  END DO                                                                     !!!
  ! Checking for convergence                                                 !!!
  IF(dmax.lt.0.001*d0) EXIT                                                  !!!
  ! Updating coordinates                                                     !!!
  r=r+f                                                                      !!!
  ! Correcting neigh info                                                    !!!
  CALL correct_neigh(nv,r,neigh,neigh_x,neigh_y)                             !!!
  ! Correcting neighs in faces                                               !!!
  CALL correct_nxy(nf,nface,nmax,nv,r,v_in_f,x_in_f,y_in_f)                  !!!
  ! Correcting coordinates                                                   !!!
  CALL correct_coordinates(nv,r)                                             !!!
END DO                                                                       !!!
IF(j.gt.nstepmax)THEN                                                        !!!
  WRITE(*,'(A,I0,A)') 'Not optimized after ',nstepmax,' steps.'              !!!
ELSE                                                                         !!!
  WRITE(*,'(A,I0,A)') 'Optimized in ',j,' steps.'                            !!!
END IF                                                                       !!!
                                                                             !!!
IF(makesvgpdf.ne.0)THEN                                                      !!!
  CALL svgfig_maker(nv,r,neigh,neigh_x,neigh_y,filename)                     !!!
  CALL SYSTEM('inkscape '//TRIM(ADJUSTL(filename))//'.svg -o '//TRIM(ADJUSTL(filename))//'.pdf')
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE xyz_optimization                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION d_ave(nv,r,neigh,neigh_x,neigh_y)                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nv,neigh(nv,3),neigh_x(nv,3),neigh_y(nv,3)                     !!!
REAL(KIND=8):: r(nv,2),v(2),x(2),y(2),d_ave                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
d_ave=0.0D0                                                                  !!!
DO i=1,nv                                                                    !!!
  DO j=1,3                                                                   !!!
    v=r(neigh(i,j),:)+x*neigh_x(i,j)+y*neigh_y(i,j)-r(i,:)                   !!!
    d_ave=d_ave+DSQRT(SUM(v*v))                                              !!!
  END DO                                                                     !!!
END DO                                                                       !!!
d_ave=d_ave/DFLOAT(3*nv)                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END FUNCTION d_ave                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE nxy2neigh(nf,nface,nmax,v_in_f,x_in_f,y_in_f,nv,nn,neigh,neigh_x,neigh_y) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nv,neigh(nv,3),neigh_x(nv,3),neigh_y(nv,3),nf,nface(nf),nn(nv)
INTEGER:: v_in_f(nf,nmax),x_in_f(nf,nmax),y_in_f(nf,nmax),nmax,n             !!!
LOGICAL:: new                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nn=0                                                                         !!!
neigh=0                                                                      !!!
neigh_x=0                                                                    !!!
neigh_y=0                                                                    !!!
DO l=1,nf                                                                    !!!
  DO i=1,nface(l)                                                            !!!
    j=i+1                                                                    !!!
    IF(i.eq.nface(l)) j=1                                                    !!!
    new=.true.                                                               !!!
    DO n=1,nn(v_in_f(l,i))                                                   !!!
      IF(neigh(v_in_f(l,i),n).eq.v_in_f(l,j))THEN                            !!!
        IF(neigh_x(v_in_f(l,i),n).eq.x_in_f(l,i))THEN                        !!!
          IF(neigh_y(v_in_f(l,i),n).eq.y_in_f(l,i))THEN                      !!!
            new=.false.                                                      !!!
            EXIT                                                             !!!
          END IF                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
    IF(new)THEN                                                              !!!
      nn(v_in_f(l,i))=nn(v_in_f(l,i))+1                                      !!!
      neigh(v_in_f(l,i),nn(v_in_f(l,i)))=v_in_f(l,j)                         !!!
      neigh_x(v_in_f(l,i),nn(v_in_f(l,i)))=x_in_f(l,i)                       !!!
      neigh_y(v_in_f(l,i),nn(v_in_f(l,i)))=y_in_f(l,i)                       !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE nxy2neigh                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE order_neigh(nv,r,neigh,neigh_x,neigh_y)                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nv,neigh(nv,3),neigh_x(nv,3),neigh_y(nv,3),tmp                   !!!
REAL(KIND=8):: r(nv,2),r1(2),r2(2),r3(2),pi,tmpr,x(2),y(2)                   !!!
REAL(KIND=8):: theta1,theta2,theta3                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
pi=DACOS(-1.0D0)                                                             !!!
DO i=1,nv                                                                    !!!
  r1=r(neigh(i,1),:)+x*neigh_x(i,1)+y*neigh_y(i,1)-r(i,:)                    !!!
  r2=r(neigh(i,2),:)+x*neigh_x(i,2)+y*neigh_y(i,2)-r(i,:)                    !!!
  r3=r(neigh(i,3),:)+x*neigh_x(i,3)+y*neigh_y(i,3)-r(i,:)                    !!!
  theta1=DACOS(r1(1)/DSQRT(SUM(r1**2)))                                      !!!
  IF(r1(2).lt.0.0D0) theta1=2.0D0*pi-theta1                                  !!!
  theta2=DACOS(r2(1)/DSQRT(SUM(r2**2)))                                      !!!
  IF(r2(2).lt.0.0D0) theta2=2.0D0*pi-theta2                                  !!!
  theta3=DACOS(r3(1)/DSQRT(SUM(r3**2)))                                      !!!
  IF(r3(2).lt.0.0D0) theta3=2.0D0*pi-theta3                                  !!!
  IF(theta3.lt.theta2)THEN                                                   !!!
    tmp=neigh(i,3)                                                           !!!
    neigh(i,3)=neigh(i,2)                                                    !!!
    neigh(i,2)=tmp                                                           !!!
    tmp=neigh_x(i,3)                                                         !!!
    neigh_x(i,3)=neigh_x(i,2)                                                !!!
    neigh_x(i,2)=tmp                                                         !!!
    tmp=neigh_y(i,3)                                                         !!!
    neigh_y(i,3)=neigh_y(i,2)                                                !!!
    neigh_y(i,2)=tmp                                                         !!!
    tmpr=theta3                                                              !!!
    theta3=theta2                                                            !!!
    theta2=tmpr                                                              !!!
  END IF                                                                     !!!
  IF(theta2.lt.theta1)THEN                                                   !!!
    tmp=neigh(i,1)                                                           !!!
    neigh(i,1)=neigh(i,2)                                                    !!!
    neigh(i,2)=tmp                                                           !!!
    tmp=neigh_x(i,1)                                                         !!!
    neigh_x(i,1)=neigh_x(i,2)                                                !!!
    neigh_x(i,2)=tmp                                                         !!!
    tmp=neigh_y(i,1)                                                         !!!
    neigh_y(i,1)=neigh_y(i,2)                                                !!!
    neigh_y(i,2)=tmp                                                         !!!
    tmpr=theta1                                                              !!!
    theta1=theta2                                                            !!!
    theta2=tmpr                                                              !!!
  END IF                                                                     !!!
  IF(theta3.lt.theta2)THEN                                                   !!!
    tmp=neigh(i,3)                                                           !!!
    neigh(i,3)=neigh(i,2)                                                    !!!
    neigh(i,2)=tmp                                                           !!!
    tmp=neigh_x(i,3)                                                         !!!
    neigh_x(i,3)=neigh_x(i,2)                                                !!!
    neigh_x(i,2)=tmp                                                         !!!
    tmp=neigh_y(i,3)                                                         !!!
    neigh_y(i,3)=neigh_y(i,2)                                                !!!
    neigh_y(i,2)=tmp                                                         !!!
    tmpr=theta3                                                              !!!
    theta3=theta2                                                            !!!
    theta2=tmpr                                                              !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE order_neigh                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE correct_neigh(nv,r,neigh,neigh_x,neigh_y)                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,l,nv,delta,i1,i2                                                 !!!
INTEGER:: neigh_x(nv,3),neigh_y(nv,3),neigh(nv,3)                            !!!
REAL(KIND=8):: r(nv,2),r1(2),r2(2),x(2),y(2)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
DO i=1,nv                                                                    !!!
  r1=r(i,:)                                                                  !!!
  i1=FLOOR(r1(1))                                                            !!!
  DO l=1,3                                                                   !!!
    r2=r(neigh(i,l),:)+x*neigh_x(i,l)                                        !!!
    i2=FLOOR(r2(1))                                                          !!!
    delta=i2-i1                                                              !!!
    IF(ABS(delta).gt.2) stop 'aaaa'                                          !!!
    neigh_x(i,l)=delta                                                       !!!
  END DO                                                                     !!!
  i1=FLOOR(r1(2))                                                            !!!
  DO l=1,3                                                                   !!!
    r2=r(neigh(i,l),:)+y*neigh_y(i,l)                                        !!!
    i2=FLOOR(r2(2))                                                          !!!
    delta=i2-i1                                                              !!!
    IF(ABS(delta).gt.2) stop 'aaaa'                                          !!!
    neigh_y(i,l)=delta                                                       !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE correct_neigh                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE correct_nxy(nf,nface,nmax,nv,r,v_in_f,x_in_f,y_in_f)              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,l,nv,delta,i1,i2,nf,nface(nf),nmax,n                             !!!
INTEGER:: v_in_f(nf,nmax),x_in_f(nf,nmax),y_in_f(nf,nmax)                    !!!
REAL(KIND=8):: r(nv,2),r1(2),r2(2),x(2),y(2)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
DO l=1,nf                                                                    !!!
  DO i=1,nface(l)                                                            !!!
    n=i+1                                                                    !!!
    IF(i.eq.nface(l)) n=1                                                    !!!
    r1=r(v_in_f(l,i),:)                                                      !!!
    r2=r(v_in_f(l,n),:)+x*x_in_f(l,i)+y*y_in_f(l,i)                          !!!
    i1=FLOOR(r1(1))                                                          !!!
    i2=FLOOR(r2(1))                                                          !!!
    delta=i2-i1                                                              !!!
    IF(ABS(delta).gt.2) stop 'aaaa'                                          !!!
    x_in_f(l,i)=delta                                                        !!!
    i1=FLOOR(r1(2))                                                          !!!
    i2=FLOOR(r2(2))                                                          !!!
    delta=i2-i1                                                              !!!
    IF(ABS(delta).gt.2) stop 'aaaa'                                          !!!
    y_in_f(l,i)=delta                                                        !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE correct_nxy                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE correct_coordinates(nv,r)                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nv,i1,i2                                                         !!!
REAL(KIND=8):: r(nv,2)                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,nv                                                                    !!!
  i1=FLOOR(r(i,1))                                                           !!!
  r(i,1)=r(i,1)-i1                                                           !!!
  i2=FLOOR(r(i,2))                                                           !!!
  r(i,2)=r(i,2)-i2                                                           !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE correct_coordinates                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE make_graphene(i0,filename)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nv,x_in_f(1,6),y_in_f(1,6),v_in_f(1,6),nface(1)                 !!!
INTEGER:: nn(2),neigh(2,3),neigh_x(2,3),neigh_y(2,3),nmax,i0                 !!!
REAL(KIND=8):: r(2,2)                                                        !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! filename='6'                                                               !!!
print*, 'Making ',filename                                                   !!!
i0=2                                                                         !!!
nf=1                                                                         !!!
nface(1)=6                                                                   !!!
nmax=6                                                                       !!!
v_in_f(1,:)= (/1,2,1,2,1,2/)                                                 !!!
x_in_f(1,:)= (/0,0,-1,0,0,1/)                                                !!!
y_in_f(1,:)= (/0,1,0,0,-1,0/)                                                !!!
nv=2                                                                         !!!
CALL nxy2neigh(nf,nface,nmax,v_in_f,x_in_f,y_in_f,nv,nn,neigh,neigh_x,neigh_y)!!
r(1,:)=0.37D0                                                                !!!
r(2,:)=0.67D0                                                                !!!
CALL svgfig_maker(nv,r,neigh,neigh_x,neigh_y,TRIM(ADJUSTL(filename)))        !!!
CALL SYSTEM('inkscape '//TRIM(ADJUSTL(filename))//'.svg -o '//TRIM(ADJUSTL(filename))//'.pdf')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE make_graphene                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE svgfig_maker(nv,r,neigh,neigh_x,neigh_y,filename)                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nv,i1,i2                                                       !!!
INTEGER:: neigh_x(nv,3),neigh_y(nv,3),neigh(nv,3)                            !!!
REAL(KIND=8):: r(nv,2),x(2),y(2),v1(2),v2(2),s,fac                           !!!
CHARACTER(LEN=*),INTENT(IN)::filename                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
s=100.0                                                                      !!!
fac=1.0/DSQRT(DFLOAT(nv))                                                    !!!
OPEN(UNIT=2,FILE=TRIM(ADJUSTL(filename))//'.svg')                            !!!
write(2,*) '<svg viewBox="0.0 0.0 ',3.0*s,3.0*s,'">'                         !!!
DO i1=0,3                                                                    !!!
  write(2,*) '<line x1="',i1*s,'" y1="',0*s,'" x2="',i1*s,'" y2="',3*s,'" style="stroke:red;stroke-width:',0.01*s,'" />'
  write(2,*) '<line x1="',0*s,'" y1="',i1*s,'" x2="',3*s,'" y2="',i1*s,'" style="stroke:red;stroke-width:',0.01*s,'" />'  
END DO                                                                       !!!
DO i1=-1,3                                                                   !!!
DO i2=-1,3                                                                   !!!
DO i=1,nv                                                                    !!!
  v1=r(i,:)+i1*x+i2*y                                                        !!!
  write(2,*) '<circle cx="',v1(1)*s,'" cy="',v1(2)*s,'" r="',0.05*s*fac,'" stroke="black" stroke-width="' &
  & ,0.001*s*fac,'"',' fill="rgb(',0.0,',',0.0,',',0.0,')" />'               !!!
  DO j=1,3                                                                   !!!
    v2=r(neigh(i,j),:)+x*neigh_x(i,j)+y*neigh_y(i,j)+i1*x+i2*y               !!!
    write(2,*) '<line x1="',v1(1)*s,'" y1="',v1(2)*s,'" x2="',v2(1)*s,'" y2="',v2(2)*s, &
     & '" style="stroke:black;stroke-width:',0.02*s*fac,'" />'               !!!
  END DO                                                                     !!!
END DO                                                                       !!!
END DO                                                                       !!!
END DO                                                                       !!!
write(2,*) ' </svg>'                                                         !!!
CLOSE(UNIT=2)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE svgfig_maker                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE force(nv,r,neigh,neigh_x,neigh_y,d0,f,k,ka,q,dt)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nv,neigh(nv,3),neigh_x(nv,3),neigh_y(nv,3),i1,i2               !!!
REAL(KIND=8):: r(nv,2),r0(2),r1(2),r2(2),r3(2),x(2),y(2),f(nv,2),d,u(2),k,d0 !!!
REAL(KIND=8):: ka,theta,pi,q,v1(2),v2(2),v3(2),theta0,fa,dt,fd               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! pi value                                                                   !!!
pi=DACOS(-1.0D0)                                                             !!!
! 120 degrees sp2 reference angle                                            !!!
theta0=2.0D0*pi/3.0D0                                                        !!!
! Lattice vectors                                                            !!!
x(1)=1.0D0                                                                   !!!
x(2)=0.0D0                                                                   !!!
y(1)=0.0D0                                                                   !!!
y(2)=1.0D0                                                                   !!!
f=0.0D0                                                                      !!!
DO i=1,nv                                                                    !!!
  r0=r(i,:)                                                                  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  ! Elastic bond-stretching forces                                           !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  DO j=1,3                                                                   !!!
    r1=r(neigh(i,j),:)+x*neigh_x(i,j)+y*neigh_y(i,j)                         !!!
    d=DSQRT(SUM((r0-r1)**2))                                                 !!!
    u=(r0-r1)/d                                                              !!!
    fd=k*(d-d0)/d0                                                           !!!
    f(i,:)=f(i,:)-fd*u                                                       !!!
  END DO                                                                     !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  ! Elastic angle-stretching forces                                          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  ! Atom to neighbors vectors                                                !!!
  r1=r(neigh(i,1),:)+x*neigh_x(i,1)+y*neigh_y(i,1)-r(i,:)                    !!!
  r2=r(neigh(i,2),:)+x*neigh_x(i,2)+y*neigh_y(i,2)-r(i,:)                    !!!
  r3=r(neigh(i,3),:)+x*neigh_x(i,3)+y*neigh_y(i,3)-r(i,:)                    !!!
  ! Clockwise orthogonal vectors                                             !!!
  v1(1)=-r1(2) ; v1(2)=r1(1) ; v1=v1/DSQRT(SUM(v1*v1))                       !!!
  v2(1)=-r2(2) ; v2(2)=r2(1) ; v2=v2/DSQRT(SUM(v2*v2))                       !!!
  v3(1)=-r3(2) ; v3(2)=r3(1) ; v3=v3/DSQRT(SUM(v3*v3))                       !!!
  ! 12 pair                                                                  !!!
  theta=SUM(r1*r2)/(DSQRT(SUM(r1*r1))*DSQRT(SUM(r2*r2)))                     !!!
  IF(ABS(theta).gt.1.0D0) theta=theta/ABS(theta)                             !!!
  theta=DACOS(theta)                                                         !!!
  IF(r1(1)*r2(2)-r1(2)*r2(1).lt.0.0D0) theta=2.0D0*pi-theta                  !!!
  fa=ka*(theta-theta0)/theta0                                                !!!
  f(neigh(i,1),:)=f(neigh(i,1),:)+fa*v1                                      !!!
  f(neigh(i,2),:)=f(neigh(i,2),:)-fa*v2                                      !!!
  f(i,:)=f(i,:)-fa*v1+fa*v2                                                  !!!
  ! 23 pair                                                                  !!!
  theta=SUM(r2*r3)/(DSQRT(SUM(r2*r2))*DSQRT(SUM(r3*r3)))                     !!!
  IF(ABS(theta).gt.1.0D0) theta=theta/ABS(theta)                             !!!
  theta=DACOS(theta)                                                         !!!
  IF(r2(1)*r3(2)-r2(2)*r3(1).lt.0.0D0) theta=2.0D0*pi-theta                  !!!
  fa=ka*(theta-theta0)/theta0                                                !!!
  f(neigh(i,2),:)=f(neigh(i,2),:)+fa*v2                                      !!!
  f(neigh(i,3),:)=f(neigh(i,3),:)-fa*v3                                      !!!
  f(i,:)=f(i,:)-fa*v2+fa*v3                                                  !!!
  ! 31 pair                                                                  !!!
  theta=SUM(r3*r1)/(DSQRT(SUM(r3*r3))*DSQRT(SUM(r1*r1)))                     !!!
  IF(ABS(theta).gt.1.0D0) theta=theta/ABS(theta)                             !!!
  theta=DACOS(theta)                                                         !!!
  IF(r3(1)*r1(2)-r3(2)*r1(1).lt.0.0D0) theta=2.0D0*pi-theta                  !!!
  fa=ka*(theta-theta0)/theta0                                                !!!
  f(neigh(i,3),:)=f(neigh(i,3),:)+fa*v3                                      !!!
  f(neigh(i,1),:)=f(neigh(i,1),:)-fa*v1                                      !!!
  f(i,:)=f(i,:)-fa*v3+fa*v1                                                  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  ! Coulomb repulsive forces                                                 !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                   !!!
  DO i1=-1,1                                                                 !!!
  DO i2=-1,1                                                                 !!!
  DO j=1,nv                                                                  !!!
    IF((i1.eq.0).AND.(i2.eq.0).AND.(j.eq.i))CYCLE                            !!!
    r1=r(j,:)+x*i1+y*i2                                                      !!!
    d=DSQRT(SUM((r0-r1)**2))                                                 !!!
    IF(d.gt.2) CYCLE                                                         !!!
    u=(r0-r1)/d                                                              !!!
    f(i,:)=f(i,:)+(q*d0*d0/(d*d))*u                                          !!!
  END DO                                                                     !!!
  END DO                                                                     !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! ! Displacement                                                             !!!
f=dt*f                                                                       !!!
! ! Limiting to the cutoff                                                   !!!
DO i=1,nv                                                                    !!!
  IF(DSQRT(SUM(f(i,:)**2)).gt.0.1D0*d0) f(i,:)=0.1D0*d0*f(i,:)/DSQRT(SUM(f(i,:)**2))
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE force                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_relx                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
