!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_conv                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE flg_to_fev(nflags,flag,nf,ne,nv,fev)                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,flag(nflags,-3:3)                          !!!
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
SUBROUTINE faces_in_faces(nflags,flag,nf,nface,f_in_f,nmax)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf,nflags,nmax,last                                          !!!
INTEGER:: flag(nflags,-3:3),nface(nf)                      !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: f_in_f(:,:)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! f_in_f(i,j) -> neighbor face by i-th edge                                  !!!
ALLOCATE(f_in_f(nf,nmax))                                                    !!!
f_in_f=0                                                                     !!!
DO i=1,nf                                                                    !!!
  DO j=1,nflags                                                              !!!
    IF(flag(j,1).eq.i)THEN                                                   !!!
      l=1                                                                    !!!
      f_in_f(i,l)=flag(flag(j,-1),1)                                    !!!
      last=j                                                                 !!!
      DO                                                                     !!!
        l=l+1                                                                !!!
        last=flag(last,-3)                                              !!!
        last=flag(last,-2)                                              !!!
        f_in_f(i,l)=flag(flag(last,-1),1)                               !!!
        IF(l.eq.nface(i)) EXIT                                               !!!
      END DO                                                                 !!!
      EXIT                                                                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE faces_in_faces                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neq_fe(nflags,flag,nf,ne,nface,nmax,e_in_f,uneq_face,u_in_f,nmaps,maps)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nflags,flag(nflags,-3:3),f1,f2,e1,e2,nmaps                      !!!
INTEGER:: nf,nface(nf),mapa(nflags),ne,nmax,e_in_f(nf,nmax)                  !!!
INTEGER:: Fadj(nf,nf),Eadj(ne,ne),maps(nmaps,nflags)                         !!!
LOGICAL,INTENT(OUT):: uneq_face(nf),u_in_f(nf,nmax)                                      !!!
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
SUBROUTINE face_size_from_flag(nface,flag_0,nflags,flag)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
INTEGER:: nface,nflags,flag(nflags,-3:3),flag_0,flag_i,i
LOGICAL:: closed
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nface=1
flag_i=flag_0
closed=.false.
DO i=1,nflags
  flag_i=flag(flag_i,-3)
  flag_i=flag(flag_i,-2)
  IF(flag_i.eq.flag_0)THEN
    closed=.true.
    EXIT
  END IF
  nface=nface+1
END DO
IF(.not.closed) STOP 'Could not close a face.'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE face_size_from_flag
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_conv                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
