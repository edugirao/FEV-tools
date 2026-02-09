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
END MODULE tools_conv                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
