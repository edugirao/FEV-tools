!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_maps                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
PRIVATE:: n_fix_elements,check_map,same_local                                !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE creating_maps(nflags,flag,neigh_flag,nf,nface,nmaps,maps)         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nmaps,nflags,flag(nflags),neigh_flag(nflags,3),tmpsize         !!!
INTEGER:: nf,nface(nf),mapa(nflags)                                          !!!
INTEGER,ALLOCATABLE:: maps(:,:),mtmp(:,:)                                    !!!
LOGICAL:: skip(nflags),map                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
skip=.false.                                                                 !!!
nmaps=0                                                                      !!!
tmpsize=1                                                                    !!!
ALLOCATE(maps(tmpsize,nflags))                                               !!!
i=1                                                                          !!!
! Avoiding identity symmetry                                                 !!!
skip(i)=.true.                                                               !!!
DO j=1,nflags                                                                !!!
  ! Avoiding repeated maps                                                   !!!
  IF(skip(j)) cycle                                                          !!!
  ! Cheking for a map starting with i->j                                     !!!
  CALL check_map(nflags,nf,flag,nface,neigh_flag, &                          !!!
             &   nflags,nf,flag,nface,neigh_flag,i,j,map,mapa)               !!!
  IF(.not.map) CYCLE                                                         !!!
  ! Preparing to avoiding repeated maps later                                !!!
  skip(mapa(1))=.true.                                                       !!!
  ! Updating number of maps                                                  !!!  
  nmaps=nmaps+1                                                              !!!  
  ! Expanding maps if needed                                                 !!!  
  IF(nmaps.gt.tmpsize)THEN                                                   !!!
    ALLOCATE(mtmp(2*tmpsize,nflags))                                         !!!
    mtmp(1:nmaps-1,:)=maps                                                   !!!
    mtmp(nmaps:2*tmpsize,:)=0                                                !!!
    CALL MOVE_ALLOC(mtmp,maps)                                               !!!
    tmpsize=2*tmpsize                                                        !!!
  END IF                                                                     !!!
  ! Storing map                                                              !!!  
  maps(nmaps,:)=mapa                                                         !!!
END DO                                                                       !!!
! Redimensioninf maps if needed                                              !!!
IF(nmaps.lt.tmpsize)THEN                                                     !!!
  ALLOCATE(mtmp(nmaps,nflags))                                               !!!
  mtmp=maps(1:nmaps,:)                                                       !!!
  CALL MOVE_ALLOC(mtmp,maps)                                                 !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE creating_maps                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE get_nfixed(nflags,m1,m2,nmaps,maps,maps_nfixed)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nmaps,nflags,nfixed                                              !!!
INTEGER:: mapa(nflags),m1(nflags,nflags),m2(nflags,nflags),maps(nmaps,nflags)!!!
INTEGER,ALLOCATABLE:: maps_nfixed(:)                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(maps_nfixed(nmaps))                                                 !!!
DO i=1,nmaps                                                                 !!!
  mapa=maps(i,:)                                                             !!!
  ! Calculating max number of fixed elements                                 !!!
  CALL n_fix_elements(nflags,mapa,m1,m2,nfixed)                              !!!
  maps_nfixed(i)=nfixed                                                      !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE get_nfixed                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE n_fix_elements(nflags,mapa,m1,m2,nfixed)                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: n,nflags,m1(nflags,nflags),m2(nflags,nflags),mapa(nflags),nfixed   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nfixed=0                                                                     !!!
DO n=1,nflags                                                                !!!
  IF(m1(n,mapa(n)).eq.1)THEN                                                 !!!
    nfixed=1                                                                 !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
DO n=1,nflags                                                                !!!
  IF(m2(n,mapa(n)).eq.1)THEN                                                 !!!
    nfixed=2                                                                 !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE n_fix_elements                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE map_types(nflags,flag_color,nrotmaps,nmirmaps,nglimaps,ntramaps,nmaps,maps,maps_nfixed)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nflags,flag_color(nflags),nrotmaps,nmirmaps,nglimaps,ntramaps      !!!
INTEGER:: nmaps,i,maps(nmaps,nflags),maps_nfixed(nmaps)  !!!                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nrotmaps=0                                                                   !!!
nmirmaps=0                                                                   !!!
nglimaps=0                                                                   !!!
ntramaps=0                                                                   !!!
DO i=1,nmaps                                                                 !!!
  IF(maps_nfixed(i).eq.1) nrotmaps=nrotmaps+1                                !!!
  IF(maps_nfixed(i).eq.2) nmirmaps=nmirmaps+1                                !!!
  IF(maps_nfixed(i).eq.0)THEN                                                !!!
    IF(flag_color(1)*flag_color(maps(i,1)).eq.1) ntramaps=ntramaps+1         !!!
    IF(flag_color(1)*flag_color(maps(i,1)).eq.-1) nglimaps=nglimaps+1        !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE map_types                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_equiv_fgraphs(nflags1,nf1,flag1,nface1,neigh_flag1, &       !!!
                                &  nflags2,nf2,flag2,nface2,neigh_flag2,same)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nflags1,nf1,flag1(nflags1),nface1(nf1),neigh_flag1(nflags1,3)      !!!
INTEGER:: nflags2,nf2,flag2(nflags2),nface2(nf2),neigh_flag2(nflags2,3)      !!!
INTEGER:: i,j                                                                !!!
LOGICAL:: map,same                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
same=.false.                                                                 !!!
i=1                                                                          !!!
DO j=1,nflags2                                                               !!!
  ! Cheking for a map starting with i->j                                     !!!
  CALL check_map(nflags1,nf1,flag1,nface1,neigh_flag1, &                     !!!
              &  nflags2,nf2,flag2,nface2,neigh_flag2,i,j,map)               !!!
  IF(map)THEN                                                                !!!
    same=.true.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_equiv_fgraphs                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_map(nflags1,nf1,flag1,nface1,neigh_flag1, &                 !!!
                    &   nflags2,nf2,flag2,nface2,neigh_flag2,i,j,map,mapping)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,m,p,inext,jnext,nflags1,nflags2,nf1,nf2,f1,f2,e1,e2,v1,v2    !!!
INTEGER:: flag1(nflags1,3),nface1(nf1),neigh_flag1(nflags1,3)                !!!
INTEGER:: flag2(nflags2,3),nface2(nf2),neigh_flag2(nflags2,3)                !!!
INTEGER,INTENT(OUT),OPTIONAL:: mapping(nflags1)                              !!!
INTEGER:: mapa_m,mapa_l,times_mapped(nflags2),mapa(nflags1)                  !!!
LOGICAL:: ok(nflags1),same,map,candidate                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Checking for local symmetry                                                !!!
CALL same_local(nflags1,nf1,flag1,nface1,neigh_flag1, &                      !!!
             &  nflags2,nf2,flag2,nface2,neigh_flag2,i,j,same)               !!!
! Flags not ok? Not a map! Get out!                                          !!!
IF(.not.same)THEN                                                            !!!
  map=.false.                                                                !!!
  RETURN                                                                     !!!
END IF                                                                       !!!
! Empty initial map, except for the i-th position                            !!!
mapa=0                                                                       !!!
mapa(i)=j                                                                    !!!
times_mapped=0                                                               !!!
times_mapped(j)=1                                                            !!!
! No flags ok, except the i-th one                                           !!!
ok=.false.                                                                   !!!
ok(i)=.true.                                                                 !!!
! Next flags to be compared                                                  !!!
inext=neigh_flag1(i,1)                                                       !!!
jnext=neigh_flag2(j,1)                                                       !!!
! Initially, we have a map                                                   !!!
map=.true.                                                                   !!!
DO p=1,nflags1-1                                                             !!!
  ! Compare the inext and jnext flags                                        !!!
  CALL same_local(nflags1,nf1,flag1,nface1,neigh_flag1, &                    !!!
             &    nflags2,nf2,flag2,nface2,neigh_flag2,inext,jnext,same)     !!!
  IF(same)THEN                                                               !!!
    ! Flags with same local symmetry, add to map                             !!!
    mapa(inext)=jnext                                                        !!!
    ok(inext)=.true.                                                         !!!
    times_mapped(jnext)=times_mapped(jnext)+1                                !!!
  ELSE IF(.not.same)THEN                                                     !!!
    ! Flags not ok, not a map, get out                                       !!!
    map=.false.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
  ! Search next testing flag over the neighs of ok flags                     !!!
  candidate=.false.                                                          !!!
  DO m=1,nflags1                                                             !!!
    IF(ok(m))THEN                                                            !!!
      mapa_m=mapa(m)                                                         !!!
      DO l=1,3                                                               !!!
        IF(.not.ok(neigh_flag1(m,l)))THEN                                    !!!
          candidate=.true.                                                   !!!
          inext=neigh_flag1(m,l)                                             !!!
          jnext=neigh_flag2(mapa_m,l)                                        !!!
          EXIT                                                               !!!
        END IF                                                               !!!
      END DO                                                                 !!!
    END IF                                                                   !!!
    IF(candidate) EXIT                                                       !!!
  END DO                                                                     !!!
END DO                                                                       !!!
IF(.not.map) RETURN                                                          !!!
! Double check if a map                                                      !!!
DO m=2,nflags2                                                               !!!
  DO l=1,m-1                                                                 !!!
    IF(times_mapped(l).ne.times_mapped(m))THEN                               !!!
      map=.false.                                                            !!!
      RETURN                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Double check if a map                                                      !!!
! Checking for double-mapped elements                                        !!!
DO m=1,nflags1                                                               !!!
  mapa_m=mapa(m)                                                             !!!
  f1=flag1(m,1)                                                              !!!
  f2=flag2(mapa_m,1)                                                         !!!
  e1=flag1(m,2)                                                              !!!
  e2=flag2(mapa_m,2)                                                         !!!
  v1=flag1(m,3)                                                              !!!
  v2=flag2(mapa_m,3)                                                         !!!
  DO l=m+1,nflags1                                                           !!!
    mapa_l=mapa(l)                                                           !!!
    ! Checking face mapping                                                  !!!
    IF(flag1(l,1).eq.f1)THEN                                                 !!!
      IF(flag2(mapa_l,1).ne.f2)THEN                                          !!!
        map=.false.                                                          !!!
        RETURN                                                               !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
    ! Checking edge mapping                                                  !!!
    IF(flag1(l,2).eq.e1)THEN                                                 !!!
      IF(flag2(mapa_l,2).ne.e2)THEN                                          !!!
        map=.false.                                                          !!!
        RETURN                                                               !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
    ! Checking vertex mapping                                                !!!
    IF(flag1(l,3).eq.v1)THEN                                                 !!!
      IF(flag2(mapa_l,3).ne.v2)THEN                                          !!!
        map=.false.                                                          !!!
        RETURN                                                               !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
IF(PRESENT(mapping)) mapping=mapa                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_map                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE same_local(nflags1,nf1,flag1,nface1,neigh_flag1, &                !!!
                  &   nflags2,nf2,flag2,nface2,neigh_flag2,i,j,same)         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ii,iii,jj,jjj,nflags1,nf1,nflags2,nf2                          !!!
INTEGER:: flag1(nflags1,3),nface1(nf1),neigh_flag1(nflags1,3)                !!!
INTEGER:: flag2(nflags2,3),nface2(nf2),neigh_flag2(nflags2,3)                !!!
LOGICAL:: same                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
same=.false.                                                                 !!!
! Same polygon for the flag                                                  !!!
IF(nface1(flag1(i,1)).eq.nface2(flag2(j,1)))THEN                             !!!
  ! Same polygon for the face-neighbor flag                                  !!!
  ii=neigh_flag1(i,1)                                                        !!!
  jj=neigh_flag2(j,1)                                                        !!!
  IF(nface1(flag1(ii,1)).eq.nface2(flag2(jj,1)))THEN                         !!!
    ! Same polygon for the face-neighbor of the edge-neighbor flag           !!!
    iii=neigh_flag1(neigh_flag1(i,2),1)                                      !!!
    jjj=neigh_flag2(neigh_flag2(j,2),1)                                      !!!
    IF(nface1(flag1(iii,1)).eq.nface2(flag2(jjj,1)))THEN                     !!!
      same=.true.                                                            !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE same_local                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,nmaps,maps,maps_nfixed)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags),nmaps       !!!
INTEGER:: nface(nf),nf,nflags                                                !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:),m2(:,:),m1(:,:)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! First Neighbors                                                            !!!
CALL first_neighbors(nflags,neigh_flag,m2)                                   !!!
! Second Neighbors                                                           !!!
CALL second_neighbors(nflags,flag,m2,m1,flag_color)                          !!!
! Creating the maps (allocations inside)                                     !!!
CALL creating_maps(nflags,flag,neigh_flag,nf,nface,nmaps,maps)               !!!
CALL get_nfixed(nflags,m1,m2,nmaps,maps,maps_nfixed)                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE flg2map                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE first_neighbors(nflags,neigh_flag,m2)                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,ALLOCATABLE,INTENT(OUT):: m2(:,:)                                    !!!
INTEGER:: i,j,l,nflags,neigh_flag(nflags,3)                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(m2(nflags,nflags))                                                  !!!
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
INTEGER,ALLOCATABLE,INTENT(OUT):: m1(:,:)                                    !!!
INTEGER:: i,j,nflags,flag(nflags,3),m2(nflags,nflags),flag_color(nflags)     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(m1(nflags,nflags))                                                  !!!
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
END MODULE tools_maps                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
