!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE creating_maps(nflags,flag,neigh_flag,m1,m2,nf,nface,filename)     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nmaps,nflags,flag(nflags),neigh_flag(nflags,3),nfixed          !!!
INTEGER:: nf,nface(nf),mapa(nflags),m1(nflags,nflags),m2(nflags,nflags)      !!!
LOGICAL:: skip(nflags),map                                                   !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
skip=.false.                                                                 !!!
OPEN(UNIT=7,FILE='maps',FORM='UNFORMATTED')                                  !!!
nmaps=0                                                                      !!!
i=1                                                                          !!!
! Avoiding identity symmetry                                                 !!!
skip(i)=.true.                                                               !!!
DO j=1,nflags                                                                !!!
  ! Avoiding repeated maps                                                   !!!
  IF(skip(j)) cycle                                                          !!!
  ! Cheking for a map starting with i->j                                     !!!
  CALL check_map(nflags,flag,neigh_flag,nf,nface,i,j,map,mapa)               !!!
  IF(.not.map) CYCLE                                                         !!!
  ! Preparing to avoiding repeated maps later                                !!!
  skip(mapa(1))=.true.                                                       !!!
  ! Calculating max number of fixed elements                                 !!!
  CALL n_fix_elements(nflags,mapa,m1,m2,nfixed)                              !!!
  ! Saving map                                                               !!!
  WRITE(7) nfixed,mapa                                                       !!!
  nmaps=nmaps+1                                                              !!!
END DO                                                                       !!!
CLOSE(UNIT=7)                                                                !!!
OPEN(UNIT=7,FILE='maps',FORM='UNFORMATTED')                                  !!!
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')                            !!!
WRITE(8,*) nmaps                                                             !!!
DO i=1,nmaps                                                                 !!!
  READ(7) nfixed,mapa                                                        !!!
  WRITE(8,*) nfixed                                                          !!!
  WRITE(8,*) mapa                                                            !!!
END DO                                                                       !!!
CLOSE(UNIT=8)                                                                !!!
CLOSE(UNIT=7,STATUS='DELETE')                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE creating_maps                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE same_local(nflags,flag,neigh_flag,nf,nface,i,j,same)              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ii,iii,jj,jjj,nflags,nf                                        !!!
INTEGER:: flag(nflags,3),nface(nf),neigh_flag(nflags,3)                      !!!
LOGICAL:: same                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
same=.false.                                                                 !!!
! Same polygon for the flag                                                  !!!
IF(nface(flag(i,1)).eq.nface(flag(j,1)))THEN                                 !!!
  ! Same polygon for the face-neighbor flag                                  !!!
  ii=neigh_flag(i,1)                                                         !!!
  jj=neigh_flag(j,1)                                                         !!!
  IF(nface(flag(ii,1)).eq.nface(flag(jj,1)))THEN                             !!!
    ! Same polygon for the face-neighbor of the edge-neighbor flag           !!!
    iii=neigh_flag(neigh_flag(i,2),1)                                        !!!
    jjj=neigh_flag(neigh_flag(j,2),1)                                        !!!
    IF(nface(flag(iii,1)).eq.nface(flag(jjj,1)))THEN                         !!!
      same=.true.                                                            !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE same_local                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_map(nflags,flag,neigh_flag,nf,nface,i,j,map,mapa)           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,m,p,inext,jnext,nflags,mapa(nflags),nf,f1,f2,e1,e2,v1,v2     !!!
INTEGER:: flag(nflags,3),nface(nf),neigh_flag(nflags,3),mapa_inv(nflags)     !!!
LOGICAL:: ok(nflags),same,map,candidate                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Checking for local symmetry                                                !!!
CALL same_local(nflags,flag,neigh_flag,nf,nface,i,j,same)                    !!!
! Flags not ok? Not a map! Get out!                                          !!!
IF(.not.same)THEN                                                            !!!
  map=.false.                                                                !!!
  RETURN                                                                     !!!
END IF                                                                       !!!
! Empty initial map, except for the i-th position                            !!!
mapa=0                                                                       !!!
mapa(i)=j                                                                    !!!
mapa_inv=0                                                                   !!!
mapa_inv(j)=i                                                                !!!
! No flags ok, excpt the i-th one                                            !!!
ok=.false.                                                                   !!!
ok(i)=.true.                                                                 !!!
! Next flags to be compared                                                  !!!
inext=neigh_flag(i,1)                                                        !!!
jnext=neigh_flag(j,1)                                                        !!!
! Initially, we have a map                                                   !!!
map=.true.                                                                   !!!
DO p=1,nflags-1                                                              !!!
  ! First check if jnext hasn't been taken by someone before.                !!!
  IF(mapa_inv(jnext).ne.0)THEN                                               !!!
    ! Flags not ok, not a map, get out                                       !!!
    map=.false.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
  ! Compare the inext and jnext flags                                        !!!
  CALL same_local(nflags,flag,neigh_flag,nf,nface,inext,jnext,same)          !!!
  IF(same)THEN                                                               !!!
    ! Flags with same local symmetry, add to map                             !!!
    mapa(inext)=jnext                                                        !!!
    mapa_inv(jnext)=inext                                                    !!!
    ok(inext)=.true.                                                         !!!
  ELSE IF(.not.same)THEN                                                     !!!
    ! Flags not ok, not a map, get out                                       !!!
    map=.false.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
  ! Search next testing flag over the neighs of ok flags                     !!!
  candidate=.false.                                                          !!!
  DO m=1,nflags                                                              !!!
    IF(ok(m))THEN                                                            !!!
      DO l=1,3                                                               !!!
        IF(.not.ok(neigh_flag(m,l)))THEN                                     !!!
          candidate=.true.                                                   !!!
          inext=neigh_flag(m,l)                                              !!!
          jnext=neigh_flag(mapa(m),l)                                        !!!
          EXIT                                                               !!!
        END IF                                                               !!!
      END DO                                                                 !!!
    END IF                                                                   !!!
    IF(candidate) EXIT                                                       !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Double check if a map                                                      !!!
IF(map)THEN                                                                  !!!
  ! Checking for double-mapped elements                                      !!!
  DO m=1,nflags                                                              !!!
    f1=flag(m,1)                                                             !!!
    f2=flag(mapa(m),1)                                                       !!!
    e1=flag(m,2)                                                             !!!
    e2=flag(mapa(m),2)                                                       !!!
    v1=flag(m,3)                                                             !!!
    v2=flag(mapa(m),3)                                                       !!!
    DO l=m+1,nflags                                                          !!!
      ! Checking face mapping                                                !!!
      IF(flag(l,1).eq.f1)THEN                                                !!!
        IF(flag(mapa(l),1).ne.f2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking edge mapping                                                !!!
      IF(flag(l,2).eq.e1)THEN                                                !!!
        IF(flag(mapa(l),2).ne.e2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking vertex mapping                                              !!!
      IF(flag(l,3).eq.v1)THEN                                                !!!
        IF(flag(mapa(l),3).ne.v2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_map                                                     !!!
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
SUBROUTINE map_types(nflags,flag_color,nrotmaps,nmirmaps,nglimaps,ntramaps,filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nflags,flag_color(nflags),nrotmaps,nmirmaps,nglimaps,ntramaps      !!!
INTEGER:: nmaps,mapa(nflags),nfixed,i                                        !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nrotmaps=0                                                                   !!!
nmirmaps=0                                                                   !!!
nglimaps=0                                                                   !!!
ntramaps=0                                                                   !!!
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')                            !!!
READ(8,*) nmaps                                                              !!!
DO i=1,nmaps                                                                 !!!
  READ(8,*) nfixed                                                           !!!
  READ(8,*) mapa                                                             !!!
  IF(nfixed.eq.1) nrotmaps=nrotmaps+1                                        !!!
  IF(nfixed.eq.2) nmirmaps=nmirmaps+1                                        !!!
  IF(nfixed.eq.0)THEN                                                        !!!
    IF(flag_color(1)*flag_color(mapa(1)).eq.1) ntramaps=ntramaps+1           !!!
    IF(flag_color(1)*flag_color(mapa(1)).eq.-1) nglimaps=nglimaps+1          !!!
  END IF                                                                     !!!
END DO                                                                       !!!
CLOSE(UNIT=8)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE map_types                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_equiv_fgraphs(nflags,nf,flag1,nface1,neigh_flag1, &         !!!
                                       & flag2,nface2,neigh_flag2,same)      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nflags,nf,flag1(nflags),nface1(nf),neigh_flag1(nflags,3)       !!!
INTEGER:: flag2(nflags),nface2(nf),neigh_flag2(nflags,3)                     !!!
LOGICAL:: map,same                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
same=.false.                                                                 !!!
i=1                                                                          !!!
DO j=1,nflags                                                                !!!
  ! Cheking for a map starting with i->j                                     !!!
  CALL check_map_ab(nflags,nf,flag1,nface1,neigh_flag1, &                    !!!
                            & flag2,nface2,neigh_flag2,i,j,map)              !!!
  IF(map)THEN                                                                !!!
    same=.true.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_equiv_fgraphs                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_equiv_fgraphs_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &   !!!
                                &  nflags2,nf2,flag2,nface2,neigh_flag2,same)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nflags1,nf1,flag1(nflags1),nface1(nf1),neigh_flag1(nflags1,3)  !!!
INTEGER:: nflags2,nf2,flag2(nflags2),nface2(nf2),neigh_flag2(nflags2,3)      !!!
LOGICAL:: map,same                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
same=.false.                                                                 !!!
i=1                                                                          !!!
DO j=1,nflags2                                                               !!!
  ! Cheking for a map starting with i->j                                     !!!
  CALL check_map_ab_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &              !!!
                     &  nflags2,nf2,flag2,nface2,neigh_flag2,i,j,map)        !!!
  IF(map)THEN                                                                !!!
    same=.true.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_equiv_fgraphs_dif                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_map_ab(nflags,nf,flag1,nface1,neigh_flag1, &                !!!
                                & flag2,nface2,neigh_flag2,i,j,map)          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,m,p,inext,jnext,nflags,mapa(nflags),nf,f1,f2,e1,e2,v1,v2     !!!
INTEGER:: flag1(nflags,3),nface1(nf),neigh_flag1(nflags,3),mapa_inv(nflags)  !!!
INTEGER:: flag2(nflags,3),nface2(nf),neigh_flag2(nflags,3)                   !!!
INTEGER:: mapa_m,mapa_l                                                      !!!
LOGICAL:: ok(nflags),same,map,candidate                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Checking for local symmetry                                                !!!
CALL same_local_ab(nflags,nf,flag1,nface1,neigh_flag1, &                     !!!
                           & flag2,nface2,neigh_flag2,i,j,same)              !!!
! Flags not ok? Not a map! Get out!                                          !!!
IF(.not.same)THEN                                                            !!!
  map=.false.                                                                !!!
  RETURN                                                                     !!!
END IF                                                                       !!!
! Empty initial map, except for the i-th position                            !!!
mapa=0                                                                       !!!
mapa(i)=j                                                                    !!!
mapa_inv=0                                                                   !!!
mapa_inv(j)=i                                                                !!!
! No flags ok, except the i-th one                                           !!!
ok=.false.                                                                   !!!
ok(i)=.true.                                                                 !!!
! Next flags to be compared                                                  !!!
inext=neigh_flag1(i,1)                                                       !!!
jnext=neigh_flag2(j,1)                                                       !!!
! Initially, we have a map                                                   !!!
map=.true.                                                                   !!!
DO p=1,nflags-1                                                              !!!
  ! First check if jnext hasn't been taken by someone before.                !!!
  IF(mapa_inv(jnext).ne.0)THEN                                               !!!
    ! Flags not ok, not a map, get out                                       !!!
    map=.false.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
  ! Compare the inext and jnext flags                                        !!!
  CALL same_local_ab(nflags,nf,flag1,nface1,neigh_flag1, &                   !!!
                             & flag2,nface2,neigh_flag2,inext,jnext,same)    !!!
  IF(same)THEN                                                               !!!
    ! Flags with same local symmetry, add to map                             !!!
    mapa(inext)=jnext                                                        !!!
    mapa_inv(jnext)=inext                                                    !!!
    ok(inext)=.true.                                                         !!!
  ELSE IF(.not.same)THEN                                                     !!!
    ! Flags not ok, not a map, get out                                       !!!
    map=.false.                                                              !!!
    RETURN                                                                   !!!
  END IF                                                                     !!!
  ! Search next testing flag over the neighs of ok flags                     !!!
  candidate=.false.                                                          !!!
  DO m=1,nflags                                                              !!!
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
! Double check if a map                                                      !!!
IF(map)THEN                                                                  !!!
  ! Checking for double-mapped elements                                      !!!
  DO m=1,nflags                                                              !!!
    mapa_m=mapa(m)                                                           !!!
    f1=flag1(m,1)                                                            !!!
    f2=flag2(mapa_m,1)                                                       !!!
    e1=flag1(m,2)                                                            !!!
    e2=flag2(mapa_m,2)                                                       !!!
    v1=flag1(m,3)                                                            !!!
    v2=flag2(mapa_m,3)                                                       !!!
    DO l=m+1,nflags                                                          !!!
      mapa_l=mapa(l)                                                         !!!
      ! Checking face mapping                                                !!!
      IF(flag1(l,1).eq.f1)THEN                                               !!!
        IF(flag2(mapa_l,1).ne.f2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking edge mapping                                                !!!
      IF(flag1(l,2).eq.e1)THEN                                               !!!
        IF(flag2(mapa_l,2).ne.e2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking vertex mapping                                              !!!
      IF(flag1(l,3).eq.v1)THEN                                               !!!
        IF(flag2(mapa_l,3).ne.v2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_map_ab                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE check_map_ab_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &          !!!
                        &   nflags2,nf2,flag2,nface2,neigh_flag2,i,j,map)    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,m,p,inext,jnext,nflags1,nflags2,mapa(nflags1),nf1,nf2,f1,f2,e1,e2,v1,v2
INTEGER:: flag1(nflags1,3),nface1(nf1),neigh_flag1(nflags1,3)                !!!
INTEGER:: flag2(nflags2,3),nface2(nf2),neigh_flag2(nflags2,3)                !!!
INTEGER:: mapa_m,mapa_l,times_mapped(nflags2)                                !!!
LOGICAL:: ok(nflags1),same,map,candidate                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Checking for local symmetry                                                !!!
CALL same_local_ab_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &               !!!
                    &  nflags2,nf2,flag2,nface2,neigh_flag2,i,j,same)        !!!
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
  CALL same_local_ab_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &             !!!
                    &    nflags2,nf2,flag2,nface2,neigh_flag2,inext,jnext,same)  
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
! Double check if a map                                                      !!!
IF(map)THEN                                                                  !!!
  DO m=2,nflags2                                                             !!!
    DO l=1,m-1                                                               !!!
      IF(times_mapped(l).ne.times_mapped(m))THEN                             !!!
        map=.false.                                                          !!!
        RETURN                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END IF                                                                       !!!
! Double check if a map                                                      !!!
IF(map)THEN                                                                  !!!
  ! Checking for double-mapped elements                                      !!!
  DO m=1,nflags1                                                             !!!
    mapa_m=mapa(m)                                                           !!!
    f1=flag1(m,1)                                                            !!!
    f2=flag2(mapa_m,1)                                                       !!!
    e1=flag1(m,2)                                                            !!!
    e2=flag2(mapa_m,2)                                                       !!!
    v1=flag1(m,3)                                                            !!!
    v2=flag2(mapa_m,3)                                                       !!!
    DO l=m+1,nflags1                                                         !!!
      mapa_l=mapa(l)                                                         !!!
      ! Checking face mapping                                                !!!
      IF(flag1(l,1).eq.f1)THEN                                               !!!
        IF(flag2(mapa_l,1).ne.f2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking edge mapping                                                !!!
      IF(flag1(l,2).eq.e1)THEN                                               !!!
        IF(flag2(mapa_l,2).ne.e2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
      ! Checking vertex mapping                                              !!!
      IF(flag1(l,3).eq.v1)THEN                                               !!!
        IF(flag2(mapa_l,3).ne.v2)THEN                                        !!!
          map=.false.                                                        !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE check_map_ab_dif                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE same_local_ab(nflags,nf,flag1,nface1,neigh_flag1, &               !!!
                                 & flag2,nface2,neigh_flag2,i,j,same)        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ii,iii,jj,jjj,nflags,nf                                        !!!
INTEGER:: flag1(nflags,3),nface1(nf),neigh_flag1(nflags,3)                   !!!
INTEGER:: flag2(nflags,3),nface2(nf),neigh_flag2(nflags,3)                   !!!
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
END SUBROUTINE same_local_ab                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE same_local_ab_dif(nflags1,nf1,flag1,nface1,neigh_flag1, &         !!!
                         &   nflags2,nf2,flag2,nface2,neigh_flag2,i,j,same)  !!!
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
END SUBROUTINE same_local_ab_dif                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE flg2map(nflags,flag,neigh_flag,flag_color,nf,nface,filename)      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: flag(nflags,3),neigh_flag(nflags,3),flag_color(nflags)             !!!
INTEGER:: nface(nf),m2(nflags,nflags),m1(nflags,nflags),nf,nflags            !!!
CHARACTER*100:: filename                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! First Neighbors                                                            !!!
CALL first_neighbors(nflags,neigh_flag,m2)                                   !!!
! Second Neighbors                                                           !!!
CALL second_neighbors(nflags,flag,m2,m1,flag_color)                          !!!
! Searching for the maps                                                     !!!
CALL creating_maps(nflags,flag,neigh_flag,m1,m2,nf,nface,filename)           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE flg2map                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
