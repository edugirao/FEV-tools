!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_conv                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fcs_to_fev(nf,ne,nv,nmax,nface,e_in_f,v_in_f,fev)                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf,ne,nv,nmax,nface(nf),e_in_f(nf,nmax),v_in_f(nf,nmax) !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: fev(:,:,:)                                 !!!
INTEGER::i,j                                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(fev(nf,ne,nv))                                                      !!!
fev=0                                                                        !!!
DO i=1,nf                                                                    !!!
  DO j=1,nface(i)-1                                                          !!!
    fev(i,e_in_f(i,j),v_in_f(i,j))=1                                         !!!
    fev(i,e_in_f(i,j),v_in_f(i,j+1))=1                                       !!!
  END DO                                                                     !!!
  j=nface(i)                                                                 !!!
  fev(i,e_in_f(i,j),v_in_f(i,j))=1                                           !!!
  fev(i,e_in_f(i,j),v_in_f(i,1))=1                                           !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fcs_to_fev                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fcs_to_flg(nf,nmax,nface,e_in_f,v_in_f,nflags,flag,neigh_flag,flag_color)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: flag(:,:),neigh_flag(:,:),flag_color(:)    !!!
INTEGER,INTENT(IN):: e_in_f(nf,nmax),v_in_f(nf,nmax),nface(nf)               !!!
INTEGER,INTENT(IN):: nf,nmax                                                 !!!
INTEGER,INTENT(OUT):: nflags                                                 !!!
INTEGER:: i,j,l,l0,f                                                         !!!
LOGICAL,ALLOCATABLE:: done(:)                                                !!!
LOGICAL:: out                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nflags=12*nf                                                                 !!!
ALLOCATE(flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags),done(nflags))!!!
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
  IF(flag_color(i)*flag_color(neigh_flag(i,1)).ne.-1) stop 'No bipartition'  !!!
  IF(flag_color(i)*flag_color(neigh_flag(i,2)).ne.-1) stop 'No bipartition'  !!!
  IF(flag_color(i)*flag_color(neigh_flag(i,3)).ne.-1) stop 'No bipartition'  !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fcs_to_flg                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE flg_to_fev(nflags,flag,nf,ne,nv,fev)                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,flag(nflags,3)                          !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: fev(:,:,:)                                 !!!
INTEGER::i                                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(fev(nf,ne,nv))                                                      !!!
fev=0                                                                        !!!
DO i=1,nflags                                                                !!!
  fev(flag(i,1),flag(i,2),flag(i,3))=1                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE flg_to_fev                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fev_in_faces(nflags,flag,neigh_flag,nf,nface,f_in_f,e_in_f,v_in_f,b_in_f,lmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf,nflags,lmax,last                                          !!!
INTEGER:: flag(nflags,3),neigh_flag(nflags,3),nface(nf)                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: f_in_f(:,:),e_in_f(:,:),v_in_f(:,:)        !!!
LOGICAL,ALLOCATABLE,INTENT(OUT):: b_in_f(:,:)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! f_in_f(i,j) -> neighbor face by i-th face                                  !!!
! e_in_f(i,j) -> j-th edge in i-th face                                      !!!
! v_in_f(i,j) -> j-th vertex in i-th face                                    !!!
! -> e_in_f(i,j) starts in v_in_f(i,j) and finishes in v_in_f(i,j+1)         !!!
! -> They're sorted by appearance around the face the face.                  !!!
! -> Bridges appear twice. Vertices on bridges appear twice.                 !!!
ALLOCATE(f_in_f(nf,lmax),e_in_f(nf,lmax),v_in_f(nf,lmax),b_in_f(nf,lmax))    !!!
f_in_f=0                                                                     !!!
e_in_f=0                                                                     !!!
v_in_f=0                                                                     !!!
b_in_f=.false.                                                               !!!
DO i=1,nf                                                                    !!!
  DO j=1,nflags                                                              !!!
    IF(flag(j,1).eq.i)THEN                                                   !!!
      l=1                                                                    !!!
      f_in_f(i,l)=flag(neigh_flag(j,1),1)                                    !!!
      e_in_f(i,l)=flag(j,2)                                                  !!!
      v_in_f(i,l)=flag(j,3)                                                  !!!
      IF(f_in_f(i,l).eq.i) b_in_f(i,l)=.true.                                !!!
      last=j                                                                 !!!
      DO                                                                     !!!
        l=l+1                                                                !!!
        last=neigh_flag(last,3)                                              !!!
        last=neigh_flag(last,2)                                              !!!
        f_in_f(i,l)=flag(neigh_flag(last,1),1)                               !!!
        e_in_f(i,l)=flag(last,2)                                             !!!
        v_in_f(i,l)=flag(last,3)                                             !!!
        IF(f_in_f(i,l).eq.i) b_in_f(i,l)=.true.                              !!!
        IF(l.eq.nface(i)) EXIT                                               !!!
      END DO                                                                 !!!
      EXIT                                                                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fev_in_faces                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neq_fe(nflags,flag,nf,ne,nface,nmax,e_in_f,uneq_face,u_in_f,nmaps,maps)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nflags,flag(nflags,3),f1,f2,e1,e2,nmaps                      !!!
INTEGER:: nf,nface(nf),mapa(nflags),ne,nmax,e_in_f(nf,nmax)                  !!!
INTEGER:: Fadj(nf,nf),Eadj(ne,ne),maps(nmaps,nflags)                         !!!
LOGICAL:: uneq_face(nf),u_in_f(nf,nmax)                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
uneq_face=.true.                                                             !!!
u_in_f=.true.                                                                !!!
Fadj=0                                                                       !!!
Eadj=0                                                                       !!!
DO i=1,nmaps                                                                 !!!
  ! Getting a map                                                            !!!
  mapa=maps(i,:)                                                             !!!
  DO j=1,nflags                                                              !!!
    f1=flag(j,1)                                                             !!!
    f2=flag(mapa(j),1)                                                       !!!
    Fadj(f1,f2)=1                                                            !!!
    Fadj(f2,f1)=1                                                            !!!
    e1=flag(j,2)                                                             !!!
    e2=flag(mapa(j),2)                                                       !!!
    Eadj(e1,e2)=1                                                            !!!
    Eadj(e2,e1)=1                                                            !!!
  END DO                                                                     !!!
END DO                                                                       !!!
DO i=1,nf                                                                    !!!
  ! Eliminating equivalent faces                                             !!!
  DO j=i+1,nf                                                                !!!
    IF(Fadj(i,j).eq.1) uneq_face(j)=.false.                                  !!!
  END DO                                                                     !!!
  ! Eliminating equivalent edges within faces                                !!!
  DO j=1,nface(i)                                                            !!!
    DO l=j+1,nface(i)                                                        !!!
      e1=e_in_f(i,j)                                                         !!!
      e2=e_in_f(i,l)                                                         !!!
      IF(Eadj(e1,e2).eq.1) u_in_f(i,l)=.false.                               !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE neq_fe                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_conv                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
