!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_writ                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
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
SUBROUTINE write_flg(nf,ne,nv,nflags,nface,flag,filename)!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,-3:3)  !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg')                         !!!
WRITE(u,*) nflags,nf,ne,nv                                                   !!!
WRITE(u,*) nface                                                             !!!
DO i=1,nflags                                                                !!!
  WRITE(u,*) flag(i,:)                         !!!
END DO                                                                       !!!
CLOSE(UNIT=u)                                                                !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')    !!!
WRITE(u) nflags,nf,ne,nv                                                     !!!
WRITE(u) nface                                                               !!!
WRITE(u) flag                                                                !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_flg                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_flg_bin(nf,ne,nv,nflags,nface,flag,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,-3:3)  !!!
INTEGER:: u                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.b.flg',FORM='UNFORMATTED')    !!!
WRITE(u) nflags,nf,ne,nv                                                     !!!
WRITE(u) nface                                                               !!!
WRITE(u) flag                                                                !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_flg_bin                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_flg_for(nf,ne,nv,nflags,nface,flag,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
INTEGER,INTENT(IN):: nf,ne,nv,nflags,nface(nf)                               !!!
INTEGER,INTENT(IN):: flag(nflags,-3:3)  !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.flg')                         !!!
WRITE(u,*) nflags,nf,ne,nv                                                   !!!
WRITE(u,*) nface                                                             !!!
DO i=1,nflags                                                                !!!
  WRITE(u,*) flag(i,:)                         !!!
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
SUBROUTINE write_graphene(filename,fev,flg,cfl,xyz,gen)                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: u,fev,flg,cfl,xyz,gen
INTEGER:: nface(1),flag(12,-3:3)               !!!
CHARACTER(LEN=*):: filename                                                     !!!
LOGICAL:: tru,fal                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tru=.true.                                                                   !!!
fal=.false.                                                                  !!!
IF(fev.ne.0)THEN                                                             !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.fev')                       !!!
  WRITE(u,*) '1    3    2'                                                   !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*) '1'                                                             !!!
  WRITE(u,*)                                                                 !!!
  CLOSE(UNIT=u)                                                              !!!
END IF                                                                       !!!
IF(flg.ne.0)THEN                                                             !!!
  nface(1)=6
  flag(:,1)=1
  flag(1:2,2)=1 ; flag(7:8,2)=1
  flag(3:4,2)=2 ; flag(9:10,2)=2
  flag(5:6,2)=3 ; flag(11:12,2)=3
  flag(1:6,3)=1 ; flag(7:12,3)=2
  flag(1,-1)= 2  ; flag(1,-2)= 6  ; flag(1,-3)= 8
  flag(2,-1)= 1  ; flag(2,-2)= 3  ; flag(2,-3)= 7
  flag(3,-1)= 4  ; flag(3,-2)= 2  ; flag(3,-3)=10
  flag(4,-1)= 3  ; flag(4,-2)= 5  ; flag(4,-3)= 9
  flag(5,-1)= 6  ; flag(5,-2)= 4  ; flag(5,-3)=12
  flag(6,-1)= 5  ; flag(6,-2)= 1  ; flag(6,-3)=11
  flag(7,-1)= 8  ; flag(7,-2)=12  ; flag(7,-3)= 2
  flag(8,-1)= 7  ; flag(8,-2)= 9  ; flag(8,-3)= 1
  flag(9,-1)=10  ; flag(9,-2)= 8  ; flag(9,-3)= 4
  flag(10,-1)= 9 ; flag(10,-2)=11 ; flag(10,-3)= 3
  flag(11,-1)=12 ; flag(11,-2)=10 ; flag(11,-3)= 6
  flag(12,-1)=11 ; flag(12,-2)= 7 ; flag(12,-3)= 5
  flag(1,0)=  1 ; flag(2,0)= -1 ; flag(3,0)=  1
  flag(4,0)= -1 ; flag(5,0)=  1 ; flag(6,0)= -1
  flag(7,0)=  1 ; flag(8,0)= -1 ; flag(9,0)=  1
  flag(10,0)=-1 ; flag(11,0)= 1 ; flag(12,0)=-1
  CALL write_flg(1,3,2,12,nface,flag,filename)
END IF                                                                       !!!
IF(cfl.ne.0)THEN                                                             !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.cfl')
  WRITE(u,*) 12
  WRITE(u,*) 0,0
  WRITE(u,*) 0,0
  WRITE(u,*) 0,-1
  WRITE(u,*) 0,-1
  WRITE(u,*) -1,0
  WRITE(u,*) -1,0
  WRITE(u,*) 0,0
  WRITE(u,*) 0,0
  WRITE(u,*) 0,1
  WRITE(u,*) 0,1
  WRITE(u,*) 1,0
  WRITE(u,*) 1,0
  CLOSE(UNIT=u)   
END IF                                                                       !!!
IF(xyz.ne.0)THEN                                                             !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.xyz')                       !!!
  WRITE(u,*) '2'                                                             !!!
  WRITE(u,*)                                                                 !!!
  WRITE(u,*) 'C  0.37  0.37  0.00'                                           !!!  
  WRITE(u,*) 'C  0.67  0.67  0.00'                                           !!!
  WRITE(u,*)                                                                 !!!
  CLOSE(UNIT=u)                                                              !!!
END IF                                                                       !!!
IF(gen.ne.0)THEN                                                             !!!
  OPEN(NEWUNIT=u,FILE='gensize1')                                            !!!
  WRITE(u,*) 1                                                               !!!
  CLOSE(UNIT=u)                                                              !!!
  OPEN(NEWUNIT=u,FILE='gensystems1')                                         !!!
  WRITE(u,*) 6,1,1,0                                                         !!!
  CLOSE(UNIT=u)                                                              !!!
  STOP                                                                       !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_graphene                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_xyz(nv,r,filename)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nv,i,u                                                             !!!
REAL(KIND=8):: r(nv,2)                                                       !!!
CHARACTER(LEN=*):: filename                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.xyz')                         !!!
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
SUBROUTINE write_sum_rules(nf,ne,nv,srule_fe,srule_fv,srule_ev,nface,filename)!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv                                                           !!!
INTEGER:: srule_fe(nv),srule_fv(ne),srule_ev(nf),nface(nf)                   !!!
INTEGER::u,i,j,k                                                             !!!
CHARACTER(LEN=*):: filename                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.sum')                         !!!
WRITE(u,'(A)') '!****************** Sum Rules *******************!'          !!!
WRITE(u,'(A,I0)') 'Euler Characteristic -> ',nf-ne+nv                        !!!
WRITE(u,'(A)') '!------------------------------------------------!'          !!!
WRITE(u,'(A)') 'Face-Edge rule test'                                         !!!
DO k=1,nv                                                                    !!!
  WRITE(u,'(A,I3,A,I2)')  'Vertex',k,'->',srule_fe(k)                        !!!
END DO                                                                       !!!
WRITE(u,'(A)') '!------------------------------------------------!'          !!!
WRITE(u,'(A)') 'Face-Vertex rule test'                                       !!!
DO j=1,ne                                                                    !!!
  WRITE(u,'(A,I3,A,I2)') 'Edge',j,'->',srule_fv(j)                           !!!
END DO                                                                       !!!
WRITE(u,'(A)') '!------------------------------------------------!'          !!!
WRITE(u,'(A)') 'Edge-Vertex rule test'                                       !!!
DO i=1,nf                                                                    !!!
  WRITE(u,'(A,I3,A,I2)')  'Face',i,'->',srule_ev(i)                          !!!
END DO                                                                       !!!
WRITE(u,'(A)') '!************************************************!'          !!!
WRITE(u,'(A)') '! Faces Sizes                                    !'          !!!
WRITE(u,'(A)') '!------------------------------------------------!'          !!!
WRITE(u,'(100I7)') nface                                                     !!!
WRITE(u,'(A)') '!************************************************!'          !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_sum_rules                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_symbol(filename,string)                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: u                                                                  !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*4000:: string(3)                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.smb')                         !!!
WRITE(u,'(A)') '!*********** System Polygonal Symbol ************!'          !!!
WRITE(u,'(A)') TRIM(ADJUSTL(string(1)))                                      !!!
WRITE(u,'(A)') '!*** System Polygonal Symbol (semi-extended) ****!'          !!!
WRITE(u,'(A)') TRIM(ADJUSTL(string(2)))                                      !!!
WRITE(u,'(A)') '!*** System Polygonal Symbol (full-extended) ****!'          !!!
WRITE(u,'(A)') TRIM(ADJUSTL(string(3)))                                      !!!
CLOSE(UNIT=u)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_symbol                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE write_cfl(nflags,cflag,filename)                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nflags                                      !!!
INTEGER:: cflag(nflags,2)    !!!
INTEGER:: i,u
CHARACTER(LEN=*),INTENT(IN):: filename                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Opening formatted flg file                                             !!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.cfl')
! Writing tensor sizes                                                   !!!    
WRITE(u,*) nflags                                     !!!
DO i=1,nflags                                                            !!!
  WRITE(u,*) cflag(i,:)           !!!
END DO                                                                   !!!
CLOSE(UNIT=u)                                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE write_cfl                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_writ                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
