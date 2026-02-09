!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_symm                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
PRIVATE:: highest_axis                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_rot(nflags,flag,m1,nmaps,maps,maps_nfixed,nrot,rot_e,rot_i,rot_n)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,n,p,nrot,n_c_axis,nflags,flag(nflags,3),fixed(3),tmpsize       !!!
INTEGER:: m1(nflags,nflags),mapa(nflags),nmaps,nfixed                        !!!
INTEGER:: maps(nmaps,nflags),maps_nfixed(nmaps)                              !!!
INTEGER,ALLOCATABLE:: rot(:),rot_i(:),rot_n(:),rot2(:),rot_i2(:),rot_n2(:)   !!!
CHARACTER*1,ALLOCATABLE:: rot_e(:),rot_e2(:)                                 !!!
CHARACTER*1:: element(3)                                                     !!!
LOGICAL:: new                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
element(1)='f'                                                               !!!
element(2)='e'                                                               !!!
element(3)='v'                                                               !!!
tmpsize=1                                                                    !!!
ALLOCATE(rot_e(tmpsize),rot_i(tmpsize),rot_n(tmpsize),rot(tmpsize))          !!!
nrot=0                                                                       !!!
DO i=1,nmaps                                                                 !!!
  nfixed=maps_nfixed(i)                                                      !!!
  mapa=maps(i,:)                                                             !!!
  ! Checking for rotation symmetries                                         !!!
  IF(nfixed.eq.1)THEN                                                        !!!
    ! Getting map rotation order                                             !!!
    n_c_axis=1                                                               !!!
    CALL highest_axis(nflags,mapa,n_c_axis)                                  !!!
    ! Searching for axes inside the map                                      !!!
    DO n=1,nflags                                                            !!!
      IF(m1(n,mapa(n)).eq.1)THEN                                             !!!
        DO j=1,3                                                             !!!
          fixed=0                                                            !!!
          IF(d_kro(flag(n,j),flag(mapa(n),j)).ne.1) CYCLE                    !!!
          fixed(1)=j                                                         !!!
          fixed(2)=flag(n,j)                                                 !!!
          fixed(3)=n_c_axis                                                  !!!
          IF(fixed(1).eq.0) STOP 'Problem in rot'                            !!!
          new=.true.                                                         !!!
          DO p=1,nrot                                                        !!!
            IF((fixed(1).eq.rot(p)).AND.(fixed(2).eq.rot_i(p)))THEN          !!!
              new=.false.                                                    !!!
              IF(n_c_axis.gt.rot_n(p)) rot_n(p)=fixed(3)                     !!!
              EXIT                                                           !!!
            END IF                                                           !!!
          END DO                                                             !!!
          IF(new)THEN                                                        !!!
            nrot=nrot+1                                                      !!!
            IF(nrot.gt.tmpsize)THEN                                          !!!
              ALLOCATE(rot_e2(2*tmpsize),rot_i2(2*tmpsize),rot_n2(2*tmpsize),rot2(2*tmpsize))
              rot2(1:nrot-1)=rot                                             !!!
              rot_e2(1:nrot-1)=rot_e                                         !!!
              rot_i2(1:nrot-1)=rot_i                                         !!!
              rot_n2(1:nrot-1)=rot_n                                         !!!
              CALL MOVE_ALLOC(rot2,rot)                                      !!!
              CALL MOVE_ALLOC(rot_e2,rot_e)                                  !!!
              CALL MOVE_ALLOC(rot_i2,rot_i)                                  !!!
              CALL MOVE_ALLOC(rot_n2,rot_n)                                  !!!
              tmpsize=2*tmpsize                                              !!!
            END IF                                                           !!!
            rot(nrot)=fixed(1)                                               !!!
            rot_e(nrot)=element(fixed(1))                                    !!!
            rot_i(nrot)=fixed(2)                                             !!!
            rot_n(nrot)=fixed(3)                                             !!!
          END IF                                                             !!!
        END DO                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_rot                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_mir(nflags,flag,m2,nmaps,maps,maps_nfixed,nmir,mir_e,mir_i,mir_m)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,js,n,p,nmir,nflags,flag(nflags,3),fixed(3),v1(3),v2(3)         !!!
INTEGER:: m2(nflags,nflags),mapa(nflags),nmaps,nfixed,tmpsize                !!!
INTEGER:: maps(nmaps,nflags),maps_nfixed(nmaps)                              !!!
INTEGER,ALLOCATABLE:: mir(:),mir_i(:),mir_m(:),mir2(:),mir_i2(:),mir_m2(:)   !!!
CHARACTER*1,ALLOCATABLE:: mir_e(:),mir_e2(:)                                 !!!
CHARACTER*1:: element(3)                                                     !!!
LOGICAL:: new                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
element(1)='f'                                                               !!!
element(2)='e'                                                               !!!
element(3)='v'                                                               !!!
tmpsize=1                                                                    !!!
ALLOCATE(mir_e(tmpsize),mir_i(tmpsize),mir_m(tmpsize),mir(tmpsize))          !!!
nmir=0                                                                       !!!
DO i=1,nmaps                                                                 !!!
  nfixed=maps_nfixed(i)                                                      !!!
  mapa=maps(i,:)                                                             !!!
  ! Checking for rotation symmetries                                         !!!
  IF(nfixed.eq.2)THEN                                                        !!!
    ! Searching for mirrors inside the map                                   !!!
    DO n=1,nflags                                                            !!!
      IF(m2(n,mapa(n)).eq.1)THEN                                             !!!
        ! Searching for mirror elements inside the map                       !!!
        js=1                                                                 !!!
        v1=flag(n,:)                                                         !!!
        v2=flag(mapa(n),:)                                                   !!!
        IF(tri_d_kro(v1,v2).eq.3) js=2                                       !!!
        DO j=js,3                                                            !!!
          fixed=0                                                            !!!
          IF(d_kro(flag(n,j),flag(mapa(n),j)).ne.1) CYCLE                    !!!
          fixed(1)=j                                                         !!!
          fixed(2)=flag(n,j)                                                 !!!
          fixed(3)=i                                                         !!!
          IF(fixed(1).eq.0) STOP 'Problem in mir'                            !!!
          new=.true.                                                         !!!
          DO p=1,nmir                                                        !!!
            IF((fixed(1).eq.mir(p)).AND.(fixed(2).eq.mir_i(p)).AND.(fixed(3).eq.mir_m(p)))THEN
              new=.false.                                                    !!!
              EXIT                                                           !!!
            END IF                                                           !!!
          END DO                                                             !!!
          IF(new)THEN                                                        !!!
            nmir=nmir+1                                                      !!!
            IF(nmir.gt.tmpsize)THEN                                          !!!
              ALLOCATE(mir_e2(2*tmpsize),mir_i2(2*tmpsize),mir_m2(2*tmpsize),mir2(2*tmpsize))
              mir2(1:nmir-1)=mir                                             !!!
              mir_e2(1:nmir-1)=mir_e                                         !!!
              mir_i2(1:nmir-1)=mir_i                                         !!!
              mir_m2(1:nmir-1)=mir_m                                         !!!
              CALL MOVE_ALLOC(mir2,mir)                                      !!!
              CALL MOVE_ALLOC(mir_e2,mir_e)                                  !!!
              CALL MOVE_ALLOC(mir_i2,mir_i)                                  !!!
              CALL MOVE_ALLOC(mir_m2,mir_m)                                  !!!
              tmpsize=2*tmpsize                                              !!!
            END IF                                                           !!!
            mir(nmir)=fixed(1)                                               !!!
            mir_e(nmir)=element(fixed(1))                                    !!!
            mir_i(nmir)=fixed(2)                                             !!!
            mir_m(nmir)=fixed(3)                                             !!!            
          END IF                                                             !!!
        END DO                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_mir                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE highest_axis(nflags,mapa,n_c_axis)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: l,n,p,q,nflags,mapa(nflags),n_c_axis                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO l=1,nflags                                                                !!!
  n=1                                                                        !!!
  p=l                                                                        !!!
  DO                                                                         !!!
    q=mapa(p)                                                                !!!
    IF(q.eq.l)THEN                                                           !!!
      EXIT                                                                   !!!
    ELSE                                                                     !!!
      n=n+1                                                                  !!!
      p=q                                                                    !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(n.gt.n_c_axis) n_c_axis=n                                               !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE highest_axis                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE counting_mirrors(nflags,flag,m2,nmaps,maps,maps_nfixed,mirror_count)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nflags,mapa(nflags),mirror_count,nfix,ilast                        !!!
INTEGER:: flag(nflags,3),fixed(nflags,2),i,j,n,p                             !!!
INTEGER:: m2(nflags,nflags),fix(2,2),nmaps,nfixed                            !!!
INTEGER:: maps(nmaps,nflags),maps_nfixed(nmaps)                              !!!
INTEGER:: neighfix(nflags,2),nnfix(nflags),ii(2),nmirmaps                    !!!
INTEGER,ALLOCATABLE:: walk(:)                                                !!!
LOGICAL:: new,done,found,add                                                 !!!
LOGICAL,ALLOCATABLE:: ok(:)                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mirror_count=0                                                               !!!
nmirmaps=0                                                                   !!!
DO i=1,nmaps                                                                 !!!
  nfixed=maps_nfixed(i)                                                      !!!
  IF(nfixed.eq.2)THEN                                                        !!!
    mapa=maps(i,:)                                                           !!!
    nmirmaps=1                                                               !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
IF(nmirmaps.eq.0) RETURN                                                     !!!
nfix=0                                                                       !!!
fixed=0                                                                      !!!
neighfix=0                                                                   !!!
nnfix=0                                                                      !!!
DO n=1,nflags                                                                !!!
  IF(m2(n,mapa(n)).eq.1)THEN                                                 !!!
    p=0    ! In a mirror map, two mapped flags will have...                  !!!
    fix=0  ! ...either 0 or 2 fixed elements                                 !!!
    IF(((flag(n,2)-flag(mapa(n),2))**2.ne.0) &                               !!!
    & .OR.((flag(n,3)-flag(mapa(n),3))**2.ne.0))THEN                         !!!
      ! The face will be a fixed element only if either the...               !!!
      !... edge or vertex isn't, to avoid problems with twins.               !!!
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) p=p+1                        !!!
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fix(p,1)=1                   !!!
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fix(p,2)=flag(n,1)           !!!
    END IF                                                                   !!!
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) p=p+1                          !!!
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fix(p,1)=2                     !!!
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fix(p,2)=flag(n,2)             !!!
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) p=p+1                          !!!
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fix(p,1)=3                     !!!
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fix(p,2)=flag(n,3)             !!!
    IF(p.ne.2) STOP 'Problem in count mirror.'                               !!!
    DO j=1,2                                                                 !!!
      new=.true.                                                             !!!
      DO i=1,nfix                                                            !!!
        IF((fixed(i,1).eq.fix(j,1)).AND.(fixed(i,2).eq.fix(j,2)))THEN        !!!
          ii(j)=i                                                            !!!
          new=.false.                                                        !!!
          EXIT                                                               !!!
        END IF                                                               !!!
      END DO                                                                 !!!
      IF(new)THEN                                                            !!!
        nfix=nfix+1                                                          !!!
        fixed(nfix,:)=fix(j,:)                                               !!!
        ii(j)=nfix                                                           !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
    DO j=1,2                                                                 !!!
      add=.false.                                                            !!!
      IF(nnfix(ii(j)).eq.0)THEN                                              !!!
        neighfix(ii(j),1)=ii(3-j)                                            !!!
        add=.true.                                                           !!!
      ELSE IF(nnfix(ii(j)).eq.1)THEN                                         !!!
        IF(neighfix(ii(j),1).ne.ii(3-j))THEN                                 !!!
          neighfix(ii(j),2)=ii(3-j)                                          !!!
          add=.true.                                                         !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      IF(nnfix(ii(j)).lt.2)THEN                                              !!!
        IF(add) nnfix(ii(j))=nnfix(ii(j))+1                                  !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
                                                                             !!!
DO n=1,nfix                                                                  !!!
 IF(nnfix(n).eq.1)THEN                                                       !!!
   neighfix(n,2)=neighfix(n,1)                                               !!!
   nnfix(n)=2                                                                !!!
 END IF                                                                      !!!
END DO                                                                       !!!
                                                                             !!!
ALLOCATE(walk(nfix),ok(nfix))                                                !!!
n=1                                                                          !!!
walk=0                                                                       !!!
ok=.false.                                                                   !!!
walk(1)=1                                                                    !!!
ok(1)=.true.                                                                 !!!
done=.false.                                                                 !!!
ilast=0                                                                      !!!
DO i=2,nfix+1                                                                !!!
  found=.false.                                                              !!!
  DO j=1,2                                                                   !!!
    IF(ok(neighfix(walk(i-1),j))) CYCLE                                      !!!
    walk(i)=neighfix(walk(i-1),j)                                            !!!
    ok(neighfix(walk(i-1),j))=.true.                                         !!!
    found=.true.                                                             !!!
    EXIT                                                                     !!!
  END DO                                                                     !!!
  IF(.not.found)THEN                                                         !!!
    IF((neighfix(walk(i-1),1).eq.1).OR.(neighfix(walk(i-1),2).eq.1))THEN     !!!
      ilast=i-1                                                              !!!
      done=.true.                                                            !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
  IF(done) EXIT                                                              !!!
END DO                                                                       !!!
IF(.not.done) STOP 'Error in mirror count'                                   !!!
IF(ilast.eq.nfix) mirror_count=1                                             !!!
IF(ilast.ne.nfix) mirror_count=2                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE counting_mirrors                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE axis_off_mirrors(nrot,rot_i,rot_e,nmir,mir_i,mir_e,nocross)       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nrot,nmir,i,j                                                      !!!
INTEGER:: rot_i(nrot),mir_i(nmir)                                            !!!
LOGICAL:: nocross                                                            !!!
CHARACTER*1:: rot_e(nrot),mir_e(nmir)                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,nrot                                                                  !!!
  nocross=.true.                                                             !!!
  DO j=1,nmir                                                                !!!
    IF((rot_e(i).eq.mir_e(j)).AND.(rot_i(i).eq.mir_i(j)))THEN                !!!
      nocross=.false.                                                        !!!
      EXIT                                                                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(nocross) RETURN                                                         !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE axis_off_mirrors                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE symmetry_group(nrotmaps,nmirmaps,nglimaps,mirror_count,nocross,group)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nrotmaps,nmirmaps,nglimaps,mirror_count                            !!!
LOGICAL:: nocross                                                            !!!
CHARACTER*4:: group                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
group=''                                                                     !!!
IF((nrotmaps.eq.5).AND.(nmirmaps.eq.6).AND.(nglimaps.eq.0)) group='p6m'      !!!
IF((nrotmaps.eq.5).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p6'       !!!
IF((nrotmaps.eq.2).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p3'       !!!
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.4).AND.(nglimaps.eq.0)) group='p4m'      !!!
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.2).AND.(nglimaps.eq.2)) group='p4g'      !!!
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p4'       !!!
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p2'       !!!
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.2)) group='pgg'      !!!
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.1)) group='pmg'      !!!
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.1)) group='pg'       !!!
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.0).AND.(mirror_count.eq.1)) group='cm'
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.0).AND.(mirror_count.eq.2)) group='pm'
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p1'       !!!
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.2).AND.(nglimaps.eq.0))THEN              !!!
  ! Cheking for axis off-mirrors                                             !!!
  IF(nocross) group='cmm'                                                    !!!
  IF(.not.nocross) group='pmm'                                               !!!
END IF                                                                       !!!
IF((nrotmaps.eq.2).AND.(nmirmaps.eq.3).AND.(nglimaps.eq.0))THEN              !!!
  ! Cheking for axis off-mirrors                                             !!!
  IF(nocross) group='p31m'                                                   !!!
  IF(.not.nocross) group='p3m1'                                              !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE symmetry_group                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION d_kro(i,j)                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: d_kro,i,j                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(i.eq.j)THEN                                                               !!!
  d_kro=1                                                                    !!!
ELSE                                                                         !!!
  d_kro=0                                                                    !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END FUNCTION d_kro                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION tri_d_kro(v1,v2)                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: tri_d_kro,v1(3),v2(3)                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tri_d_kro=d_kro(v1(1),v2(1))+ &                                              !!!
        & d_kro(v1(2),v2(2))+ &                                              !!!
        & d_kro(v1(3),v2(3))                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END FUNCTION tri_d_kro                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_symm                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
