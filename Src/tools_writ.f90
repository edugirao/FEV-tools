!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_writ                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_fcs(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &   !!!
                                                 & b_in_f,u_in_f,filename)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nmax,nface(nf)                                 !!!
INTEGER,INTENT(IN):: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax)         !!!
LOGICAL,INTENT(IN):: uneq_face(nf),b_in_f(nf,nmax),u_in_f(nf,nmax)           !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.fcs')                         !!!
! Writing tensor sizes                                                       !!!
WRITE(u,'(4I5)') nf,ne,nv,nmax                                               !!!
! Writing faces 1) size,uneq_face, 2) edges, and 3) vertices                 !!!
DO i=1,nf                                                                    !!!
  WRITE(u,'(I0,2X,L1)') nface(i),uneq_face(i)                                !!!
  WRITE(u,'(100I7)') f_in_f(i,1:nface(i))                                    !!!
  WRITE(u,'(100I7)') e_in_f(i,1:nface(i))                                    !!!
  WRITE(u,'(100I7)') v_in_f(i,1:nface(i))                                    !!!
  WRITE(u,'(100L7)') b_in_f(i,1:nface(i))                                    !!!
  WRITE(u,'(100L7)') u_in_f(i,1:nface(i))                                    !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')    !!!
! Writing tensor sizes                                                       !!!
WRITE(u) nf,ne,nv,nmax                                                       !!!
! Writing faces 1) size,uneq_face, 2) edges, and 3) vertices                 !!!
DO i=1,nf                                                                    !!!
  WRITE(u) nface(i),uneq_face(i)                                             !!!
  WRITE(u) f_in_f(i,1:nface(i))                                              !!!
  WRITE(u) e_in_f(i,1:nface(i))                                              !!!
  WRITE(u) v_in_f(i,1:nface(i))                                              !!!
  WRITE(u) b_in_f(i,1:nface(i))                                              !!!
  WRITE(u) u_in_f(i,1:nface(i))                                              !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_fcs                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_fcs_bin(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &!!
                                                 & b_in_f,u_in_f,filename)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nmax,nface(nf)                                 !!!
INTEGER,INTENT(IN):: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax)         !!!
LOGICAL,INTENT(IN):: uneq_face(nf),b_in_f(nf,nmax),u_in_f(nf,nmax)           !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')    !!!
! Writing tensor sizes                                                       !!!
WRITE(u) nf,ne,nv,nmax                                                       !!!
! Writing faces 1) size,uneq_face, 2) edges, and 3) vertices                 !!!
DO i=1,nf                                                                    !!!
  WRITE(u) nface(i),uneq_face(i)                                             !!!
  WRITE(u) f_in_f(i,1:nface(i))                                              !!!
  WRITE(u) e_in_f(i,1:nface(i))                                              !!!
  WRITE(u) v_in_f(i,1:nface(i))                                              !!!
  WRITE(u) b_in_f(i,1:nface(i))                                              !!!
  WRITE(u) u_in_f(i,1:nface(i))                                              !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_fcs_bin                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_fcs_for(nf,ne,nv,nmax,nface,uneq_face,f_in_f,e_in_f,v_in_f, &!!
                                                 & b_in_f,u_in_f,filename)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nmax,nface(nf)                                 !!!
INTEGER,INTENT(IN):: f_in_f(nf,nmax),e_in_f(nf,nmax),v_in_f(nf,nmax)         !!!
LOGICAL,INTENT(IN):: uneq_face(nf),b_in_f(nf,nmax),u_in_f(nf,nmax)           !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.fcs')                         !!!
! Writing tensor sizes                                                       !!!
WRITE(u,*) nf,ne,nv,nmax                                                       !!!
! Writing faces 1) size,uneq_face, 2) edges, and 3) vertices                 !!!
DO i=1,nf                                                                    !!!
  WRITE(u,*) nface(i),uneq_face(i)                                             !!!
  WRITE(u,*) f_in_f(i,1:nface(i))                                              !!!
  WRITE(u,*) e_in_f(i,1:nface(i))                                              !!!
  WRITE(u,*) v_in_f(i,1:nface(i))                                              !!!
  WRITE(u,*) b_in_f(i,1:nface(i))                                              !!!
  WRITE(u,*) u_in_f(i,1:nface(i))                                              !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_fcs_for                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_fev(nf,ne,nv,fev,filename)                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,fev(nf,ne,nv)                                  !!!
INTEGER:: i,j,l,u                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.fev')                         !!!
WRITE(u,*) nf,ne,nv          ! .fev file format:                             !!!
DO i=1,nf                   ! - 1st line: nf ne nv                           !!!
  DO j=1,ne                 ! - Then one line per each tensor entry (0 or 1) !!!
    DO l=1,nv               ! -> Outer loop --> Faces                        !!!
      WRITE(u,*) fev(i,j,l)  ! -> Middle loop -> Edges                       !!!
    END DO                  ! -> Inner loop --> Vertices                     !!!
  END DO                    !                                                !!!
END DO                      !                                                !!!
CLOSE(UNIT=u)               !                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_fev                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags)  !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg')                         !!!
WRITE(u,*) nflags,nf,ne,nv                                                   !!!
WRITE(u,*) nface                                                             !!!
DO i=1,nflags                                                                !!!
  WRITE(u,*) flag(i,:),neigh_flag(i,:),flag_color(i)                         !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')    !!!
WRITE(u) nflags,nf,ne,nv                                                     !!!
WRITE(u) nface                                                               !!!
WRITE(u) flag                                                                !!!
WRITE(u) neigh_flag                                                          !!!
WRITE(u) flag_color                                                          !!!
CLOSE(UNIT=3)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_flg                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_flg_bin(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags)  !!!
INTEGER:: u                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')    !!!
WRITE(u) nflags,nf,ne,nv                                                     !!!
WRITE(u) nface                                                               !!!
WRITE(u) flag                                                                !!!
WRITE(u) neigh_flag                                                          !!!
WRITE(u) flag_color                                                          !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_flg_bin                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_flg_for(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags)  !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg')                         !!!
WRITE(u,*) nflags,nf,ne,nv                                                   !!!
WRITE(u,*) nface                                                             !!!
DO i=1,nflags                                                                !!!
  WRITE(u,*) flag(i,:),neigh_flag(i,:),flag_color(i)                         !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_flg_for                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_map(nflags,nmaps,maps,maps_nfixed,filename)                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nflags,nmaps,maps(nmaps,nflags),maps_nfixed(nmaps)      !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.map')                         !!!
WRITE(u,*) nmaps                                                             !!!
DO i=1,nmaps                                                                 !!!
  WRITE(u,*) maps_nfixed(i)                                                  !!!
  WRITE(u,*) maps(i,:)                                                       !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_map                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_rot(nrot,rot_e,rot_i,rot_n,filename)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nrot,rot_i(nrot),rot_n(nrot)                            !!!
CHARACTER*1:: rot_e(nrot)                                                    !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.rot')                         !!!
WRITE(u,*) nrot                                                              !!!
DO i=1,nrot                                                                  !!!
  IF(rot_e(i).ne.'f') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') rot_e(i),rot_i(i),rot_n(i)                          !!!
END DO                                                                       !!!
DO i=1,nrot                                                                  !!!
  IF(rot_e(i).ne.'e') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') rot_e(i),rot_i(i),rot_n(i)                          !!!
END DO                                                                       !!!
DO i=1,nrot                                                                  !!!
  IF(rot_e(i).ne.'v') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') rot_e(i),rot_i(i),rot_n(i)                          !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_rot                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_mir(nmir,mir_e,mir_i,mir_m,filename)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nmir,mir_i(nmir),mir_m(nmir)                            !!!
CHARACTER*1:: mir_e(nmir)                                                    !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.mir')                         !!!
WRITE(u,'(I0)') nmir                                                         !!!
DO i=1,nmir                                                                  !!!
  IF(mir_e(i).ne.'f') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') mir_e(i),mir_i(i),mir_m(i)                          !!!
END DO                                                                       !!!
DO i=1,nmir                                                                  !!!
  IF(mir_e(i).ne.'e') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') mir_e(i),mir_i(i),mir_m(i)                          !!!
END DO                                                                       !!!
DO i=1,nmir                                                                  !!!
  IF(mir_e(i).ne.'v') CYCLE                                                  !!!
  WRITE(u,'(A1,1X,2I7)') mir_e(i),mir_i(i),mir_m(i)                          !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_mir                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_graphene                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: u                                                                  !!!
LOGICAL:: tru,fal                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tru=.true.                                                                   !!!
fal=.false.                                                                  !!!
OPEN(NEWUNIT=u,FILE='6.fev')                                                 !!!
WRITE(u,*) '1    3    2'                                                     !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.flg')                                                 !!!
WRITE(u,*) '12    1    3    2'                                               !!!
WRITE(u,*) '6'                                                               !!!
WRITE(u,*) '1      1      1      2      6      8      1'                     !!!
WRITE(u,*) '1      1      1      1      3      7     -1'                     !!!
WRITE(u,*) '1      2      1      4      2     10      1'                     !!!
WRITE(u,*) '1      2      1      3      5      9     -1'                     !!!
WRITE(u,*) '1      3      1      6      4     12      1'                     !!!
WRITE(u,*) '1      3      1      5      1     11     -1'                     !!!
WRITE(u,*) '1      1      2      8     12      2      1'                     !!!
WRITE(u,*) '1      1      2      7      9      1     -1'                     !!!
WRITE(u,*) '1      2      2     10      8      4      1'                     !!!
WRITE(u,*) '1      2      2      9     11      3     -1'                     !!!
WRITE(u,*) '1      3      2     12     10      6      1'                     !!!
WRITE(u,*) '1      3      2     11      7      5     -1'                     !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.fcs')                                                 !!!
WRITE(u,*) '1    3    2    6'                                                !!!
WRITE(u,*) '6  T'                                                            !!!
WRITE(u,*) '1      1      1      1      1      1'                            !!!
WRITE(u,*) '1      2      3      1      2      3'                            !!!
WRITE(u,*) '1      2      1      2      1      2'                            !!!
WRITE(u,*) 'T      T      T      T      T      T'                            !!!
WRITE(u,*) 'T      F      F      F      F      F'                            !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.b.fcs',FORM='UNFORMATTED')                            !!!
WRITE(u) 1,3,2,6                                                             !!!
WRITE(u) 6,tru                                                               !!!
WRITE(u) 1,1,1,1,1,1                                                         !!!
WRITE(u) 1,2,3,1,2,3                                                         !!!
WRITE(u) 1,2,1,2,1,2                                                         !!!
WRITE(u) tru,tru,tru,tru,tru,tru                                             !!!
WRITE(u) tru,fal,fal,fal,fal,fal                                             !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.nxy')                                                 !!!
WRITE(u,*) '6'                                                               !!!
WRITE(u,*) '0      0     -1      0      0      1'                            !!!
WRITE(u,*) '0      1      0      0     -1      0'                            !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.xyz')                                                 !!!
WRITE(u,*) '2'                                                               !!!
WRITE(u,*)                                                                   !!!
WRITE(u,*) 'C  0.37  0.37  0.00'                                             !!!
WRITE(u,*) 'C  0.67  0.67  0.00'                                             !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_graphene                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE make_gen1                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
LOGICAL:: tru,fal                                                            !!!
INTEGER:: u                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tru=.true.                                                                   !!!
fal=.false.                                                                  !!!
OPEN(NEWUNIT=u,FILE='6.fev')                                                 !!!
WRITE(u,*) '1    3    2'                                                     !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*) '1'                                                               !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.flg')                                                 !!!
WRITE(u,*) '12    1    3    2'                                               !!!
WRITE(u,*) '6'                                                               !!!
WRITE(u,*) '1      1      1      2      6      8      1'                     !!!
WRITE(u,*) '1      1      1      1      3      7     -1'                     !!!
WRITE(u,*) '1      2      1      4      2     10      1'                     !!!
WRITE(u,*) '1      2      1      3      5      9     -1'                     !!!
WRITE(u,*) '1      3      1      6      4     12      1'                     !!!
WRITE(u,*) '1      3      1      5      1     11     -1'                     !!!
WRITE(u,*) '1      1      2      8     12      2      1'                     !!!
WRITE(u,*) '1      1      2      7      9      1     -1'                     !!!
WRITE(u,*) '1      2      2     10      8      4      1'                     !!!
WRITE(u,*) '1      2      2      9     11      3     -1'                     !!!
WRITE(u,*) '1      3      2     12     10      6      1'                     !!!
WRITE(u,*) '1      3      2     11      7      5     -1'                     !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.fcs')                                                 !!!
WRITE(u,*) '1    3    2    6'                                                !!!
WRITE(u,*) '6  T'                                                            !!!
WRITE(u,*) '1      1      1      1      1      1'                            !!!
WRITE(u,*) '1      2      3      1      2      3'                            !!!
WRITE(u,*) '1      2      1      2      1      2'                            !!!
WRITE(u,*) 'T      T      T      T      T      T'                            !!!
WRITE(u,*) 'T      F      F      F      F      F'                            !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='6.b.fcs',FORM='UNFORMATTED')                            !!!
WRITE(u) 1,3,2,6                                                             !!!
WRITE(u) 6,tru                                                               !!!
WRITE(u) 1,1,1,1,1,1                                                         !!!
WRITE(u) 1,2,3,1,2,3                                                         !!!
WRITE(u) 1,2,1,2,1,2                                                         !!!
WRITE(u) tru,tru,tru,tru,tru,tru                                             !!!
WRITE(u) tru,fal,fal,fal,fal,fal                                             !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='gensize1')                                              !!!
WRITE(u,*) 1                                                                 !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE='gensystems1')                                           !!!
WRITE(u,*) 6,1,1,0                                                           !!!
CLOSE(UNIT=u)                                                                !!!
STOP                                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE make_gen1                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_xyz(nv,r,filename)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nv,i,u                                                             !!!
REAL(KIND=8):: r(nv,3)                                                       !!!
CHARACTER(LEN=*):: filename                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.xyz')                            !!!
WRITE(u,*) nv                                                                !!!
WRITE(u,*)                                                                   !!!
DO i=1,nv                                                                    !!!
  WRITE(u,*) 'C',r(i,:),0.0D0                                                !!!
END DO                                                                       !!!
WRITE(u,*)                                                                   !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_xyz                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_nxy(nf,nmax,nface,x_in_f,y_in_f,filename)                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nmax,nface(nf),x_in_f(nf,nmax),y_in_f(nf,nmax)                  !!!
INTEGER::u,i                                                                   !!!
CHARACTER(LEN=*):: filename                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.nxy')                         !!!
DO i=1,nf                                                                    !!!
  WRITE(u,*) nface(i)                                                        !!!
  WRITE(u,*) x_in_f(i,1:nface(i))                                            !!!
  WRITE(u,*) y_in_f(i,1:nface(i))                                            !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_nxy                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_anc(f0,ie1,ie2,filename_k,filename_p)                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: f0,ie1,ie2                                                         !!!
INTEGER::u                                                                   !!!
CHARACTER(LEN=*):: filename_k,filename_p                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename_k))//'.anc')                       !!!
WRITE(u,*) TRIM(ADJUSTL(filename_p))                                         !!!
WRITE(u,*) f0,ie1,ie2                                                        !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_anc                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_writ                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
