!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_bipt                                                            !!!
IMPLICIT NONE                                                                !!!
PRIVATE                                                                      !!!
PUBLIC:: bipartition                                                         !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bipartition(nflags,flag_color,neigh_flag,flag,nf,nface,ng,neigh_flag_set,flag_color_set)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,c1,c2,f,tmpsize                                                  !!!
INTEGER:: nflags,flag_color(nflags),flag(nflags,3),ng,ncluster,nf            !!!
INTEGER:: neigh_flag(nflags,3),neigh_flag2(nflags,3),flag_color2(nflags)     !!!
INTEGER:: cluster(nflags),cluster2(nflags),nface(nf)                         !!!
INTEGER,ALLOCATABLE:: inv_bit(:),neigh_flag_set(:,:,:),flag_color_set(:,:)   !!!
INTEGER,ALLOCATABLE:: neigh_flag_set2(:,:,:),flag_color_set2(:,:)            !!!
LOGICAL:: done,torus,endsearch                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Generating initial bipartition split                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ncluster=0       ! Initially no clusters                                     !!!
cluster=0                                                                    !!!
DO                                                                           !!!
  ! Starter engine                                                           !!!
  CALL bipartition_nucleation(ncluster,nflags,flag_color,cluster)            !!!
  ! Adding as much flags as possible to the bipartition                      !!!
  CALL bipartition_add(nflags,flag_color,neigh_flag,ncluster,cluster,done)   !!!
  IF(done) EXIT                                                              !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adding missing intra-cluster bipartition connections                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL wrap_clusters(nflags,flag,flag_color,neigh_flag,cluster)                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Adding missing inter-cluster bipartition connections                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tmpsize=1                                                                    !!!
ALLOCATE(neigh_flag_set(tmpsize,nflags,3),flag_color_set(tmpsize,nflags))    !!!
IF(ncluster.eq.1)THEN                                                        !!!
  ng=1                                                                       !!!
  neigh_flag_set(ng,:,:)=neigh_flag                                          !!!
  flag_color_set(ng,:)=flag_color                                            !!!
ELSE IF(ncluster.gt.1)THEN                                                   !!!
  neigh_flag2=0                                                              !!!
  flag_color2=0                                                              !!!
  cluster2=0                                                                 !!!
  ng=0                                                                       !!!
  ALLOCATE(inv_bit(ncluster-1))                                              !!!
  inv_bit=-1                                                                 !!!
  endsearch=.false.                                                          !!!
  DO                                                                         !!!
    ! Setting temporary trial variables                                      !!!
    neigh_flag2=neigh_flag                                                   !!!
    flag_color2=flag_color                                                   !!!
    cluster2=cluster                                                         !!!
    ! Merging clusters into one, one-at-a-time                               !!!
    DO i=1,ncluster-1                                                        !!!
      ! Look for neighbor clusters by inter-cluster pair of v-neighbor flags !!!
      CALL intercluster_vneigh_flags(nflags,neigh_flag2,cluster2,flag,c1,c2) !!!
      IF(c1.eq.0) EXIT                                                       !!!
      ! Need for inversion?                                                  !!!
      DO f=1,nflags                                                          !!!
        IF(cluster2(f).eq.c2) flag_color2(f)=inv_bit(i)*flag_color2(f)       !!!
      END DO                                                                 !!!
      ! Merging the clusters                                                 !!!
      DO f=1,nflags                                                          !!!
        IF(cluster2(f).eq.c2) cluster2(f)=c1                                 !!!
      END DO                                                                 !!!
      ! Making missing connections inside the new cluster                    !!!
      CALL wrap_clusters(nflags,flag,flag_color2,neigh_flag2,cluster2)       !!!
    END DO                                                                   !!!
    ! Toriodal test                                                          !!!
    CALL toroidal_test(nflags,neigh_flag2,torus,nf,nface,flag)               !!!
    IF(torus)THEN                                                            !!!
      ng=ng+1                                                                !!!
      IF(ng.gt.tmpsize)THEN                                                  !!!
        ALLOCATE(neigh_flag_set2(2*tmpsize,nflags,3),flag_color_set2(2*tmpsize,nflags))
        neigh_flag_set2(1:tmpsize,:,:)=neigh_flag_set                        !!!
        flag_color_set2(1:tmpsize,:)=flag_color_set                          !!!
        CALL MOVE_ALLOC(neigh_flag_set2,neigh_flag_set)                      !!!
        CALL MOVE_ALLOC(flag_color_set2,flag_color_set)                      !!!
        tmpsize=2*tmpsize                                                    !!!
      END IF                                                                 !!!
      neigh_flag_set(ng,:,:)=neigh_flag2                                     !!!
      flag_color_set(ng,:)=flag_color2                                       !!!
    END IF                                                                   !!!
    ! Updating bit structure if not torus                                    !!!
    DO i=1,ncluster-1                                                        !!!
      inv_bit(i)=inv_bit(i)+2                                                !!!
      IF(inv_bit(i).eq.1) EXIT                                               !!!
      IF(inv_bit(i).eq.3)THEN                                                !!!
        IF(i.eq.ncluster-1)THEN                                              !!!
          endsearch=.true.                                                   !!!
        ELSE                                                                 !!!
          inv_bit(i)=-1                                                      !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
    IF(endsearch) EXIT                                                       !!!
  END DO                                                                     !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bipartition                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bipartition_nucleation(ncluster,nflags,flag_color,cluster)        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nflags,flag_color(nflags),ncluster,cluster(nflags)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Updating number of clusters                                                !!!
ncluster=ncluster+1                                                          !!!
! Starting the cluster with the first non-ok flag                            !!!
DO i=1,nflags                                                                !!!
  IF(cluster(i).eq.0)THEN                                                    !!!
    flag_color(i)=1  ! We start coloring...                                  !!!
    cluster(i)=ncluster                                                      !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bipartition_nucleation                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bipartition_add(nflags,flag_color,neigh_flag,ncluster,cluster,done)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,jj,l,nflags                                                    !!!
INTEGER:: neigh_flag(nflags,3),flag_color(nflags),ncluster,cluster(nflags)   !!!
LOGICAL:: done,candidate                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO                                                                           !!!
  ! Adding atoms to the bipartite set, one at a time                         !!!
  done=.true.       ! Suppose all flags are ok                               !!!
  candidate=.false. ! Suppose we don't have a candidate to add               !!!
  DO i=1,nflags                                                              !!!
    ! Looking for a non-added flag                                           !!!
    IF(cluster(i).eq.0)THEN                                                  !!!
      done=.false. ! Found a non-added, so we are not done yet               !!!
      ! Look for an added atom from the i-neighbors                          !!!
      DO j=1,3                                                               !!!
        IF(neigh_flag(i,j).eq.0) CYCLE                                       !!!
        jj=neigh_flag(i,j)                                                   !!!
        IF(cluster(jj).ne.0)THEN           ! If we find it:                  !!!
          l=i                              ! Mark the added atom             !!!
          candidate=.true.                 ! Remark we have a candidate      !!!
          flag_color(i)=-flag_color(jj)    ! Set its color                   !!!
          cluster(i)=ncluster              ! Add to the cluster              !!!
          EXIT                             ! Exit the search                 !!!
        END IF                                                               !!!
      END DO                                                                 !!!
      IF(candidate) EXIT ! Candidate found, exit this loop                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(done) EXIT ! If done, exit                                              !!!
  IF(.not.candidate)THEN                                                     !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bipartition_add                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE wrap_clusters(nflags,flag,flag_color,neigh_flag,cluster)          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: f1,f2,nflags,flag_color(nflags),flag(nflags,3)                     !!!
INTEGER:: cluster(nflags),neigh_flag(nflags,3)                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Look for 1st flag with no v-neighbor                                       !!!
DO f1=1,nflags                                                               !!!
  IF(neigh_flag(f1,3).ne.0) CYCLE                                            !!!
  ! Look for 2nd flag with no v-neighbor                                     !!!
  DO f2=f1+1,nflags                                                          !!!
    IF(neigh_flag(f2,3).ne.0) CYCLE                                          !!!
    IF(cluster(f2).ne.cluster(f1)) CYCLE                                     !!!
    ! Cheking if v-neighbors                                                 !!!
    IF((flag(f1,1).eq.flag(f2,1)).AND.(flag(f1,2).eq.flag(f2,2)).AND.(flag(f1,3).ne.flag(f2,3)))THEN
      IF(flag_color(f1)*flag_color(f2).eq.-1)THEN                            !!!
        neigh_flag(f1,3)=f2                                                  !!!
        neigh_flag(f2,3)=f1                                                  !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE wrap_clusters                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE intercluster_vneigh_flags(nflags,neigh_flag,cluster,flag,c1,c2)   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: f1,f2,nflags,flag(nflags,3),c1,c2                                  !!!
INTEGER:: cluster(nflags),neigh_flag(nflags,3)                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Look for an inter-cluster pair of v-neighbor flags                         !!!
c1=0                                                                         !!!
c2=0                                                                         !!!
DO f1=1,nflags                                                               !!!
  ! Search for 1st flag with no v-neighbor                                   !!!
  IF(neigh_flag(f1,3).gt.0) CYCLE                                            !!!
  DO f2=f1+1,nflags                                                          !!!
    ! Search for 2nd flag with no v-neighbor                                 !!!
    IF(neigh_flag(f2,3).gt.0) CYCLE                                          !!!
    ! Cheking if inter-clusters                                              !!!
    IF(cluster(f1).eq.cluster(f2)) CYCLE                                     !!!
    ! Cheking if v-neighbors                                                 !!!
    IF((flag(f1,1).eq.flag(f2,1)).AND.(flag(f1,2).eq.flag(f2,2)).AND.(flag(f1,3).ne.flag(f2,3)))THEN
      c1=cluster(f1)                                                         !!!
      c2=cluster(f2)                                                         !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE intercluster_vneigh_flags                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE toroidal_test(nflags,neigh_flag,torus,nf,nface,flag)              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,m,nflags,ncluster,nf_test,nf,f                                 !!!
INTEGER:: cluster(nflags),nface(nf),nface3(nf),nface2(nf)                    !!!
INTEGER:: neigh_flag(nflags,3),Adj(nflags,nflags),flag(nflags,3)             !!!
LOGICAL:: ok(nflags),torus,done!,nface_test(nf)                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Look for an inter-cluster pair of v-neighbor flags                         !!!
Adj=0                                                                        !!!
DO i=1,nflags ! we are excluding the face-neighs to have only the...         !!!
  Adj(i,neigh_flag(i,2:3))=1  ! ...2i cycles (breaking hexs and tetras)      !!!
END DO                                                                       !!!
ok=.false.                                                                   !!!
ncluster=0                                                                   !!!
cluster=0                                                                    !!!
DO                                                                           !!!
  ! Starter engine                                                           !!!
  CALL bp_starter(ncluster,nflags,ok,cluster)                                !!!
  ! Adding as much flags as possible to the bipartition                      !!!
  CALL bp_add(nflags,ok,Adj,ncluster,cluster,done)                           !!!
  IF(done) EXIT                                                              !!!
END DO                                                                       !!!
torus=.true.                                                                 !!!
! Number of faces test                                                       !!!
nf_test=nflags/12                                                            !!!
IF(ncluster.ne.nf_test)THEN                                                  !!!
  torus=.false.                                                              !!!
  RETURN                                                                     !!!
END IF                                                                       !!!
! Faces size test                                                            !!!
nface2=0                                                                     !!!
DO i=1,nflags                                                                !!!
  nface2(cluster(i))=nface2(cluster(i))+1                                    !!!
END DO                                                                       !!!
nface2=nface2/2                                                              !!!
nface3=nface                                                                 !!!
DO i=1,nf-1                                                                  !!!
  DO j=1,nf-i                                                                !!!
    IF(nface2(j).gt.nface2(j+1))THEN                                         !!!
      m=nface2(j)                                                            !!!
      nface2(j)=nface2(j+1)                                                  !!!
      nface2(j+1)=m                                                          !!!
    END IF                                                                   !!!
    IF(nface3(j).gt.nface3(j+1))THEN                                         !!!
      m=nface3(j)                                                            !!!
      nface3(j)=nface3(j+1)                                                  !!!
      nface3(j+1)=m                                                          !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
IF(SUM(ABS(nface2-nface3)).ne.0)THEN                                         !!!
  torus=.false.                                                              !!!
  RETURN                                                                     !!!
END IF                                                                       !!!
! Mixed faces test                                                           !!!
DO j=1,ncluster                                                              !!!
  f=0                                                                        !!!
  DO i=1,nflags                                                              !!!
    IF(cluster(i).eq.j)THEN                                                  !!!
      IF(f.eq.0)THEN                                                         !!!
        f=flag(i,1)                                                          !!!
        CYCLE                                                                !!!
      ELSE                                                                   !!!
        IF(flag(i,1).ne.f)THEN                                               !!!
          torus=.false.                                                      !!!
          RETURN                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE toroidal_test                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bp_starter(ncluster,nflags,ok,cluster)                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nflags,ncluster,cluster(nflags)                                  !!!
LOGICAL:: ok(nflags)                                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Updating number of clusters                                                !!!
ncluster=ncluster+1                                                          !!!
! Starting the cluster with the first non-ok flag                            !!!
DO i=1,nflags                                                                !!!
  IF(.not.ok(i))THEN                                                         !!!
    ok(i)=.true.     ! We mark the first not-ok flag                         !!!
    cluster(i)=ncluster                                                      !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bp_starter                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bp_add(nflags,ok,Adj,ncluster,cluster,done)                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nflags,ncluster,cluster(nflags)                              !!!
INTEGER:: Adj(nflags,nflags)                                                 !!!
LOGICAL:: ok(nflags),done,candidate                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO                                                                           !!!
  ! Adding atoms to the cluster, one at a time                               !!!
  done=.true.       ! Suppose all flags are ok                               !!!
  candidate=.false. ! Suppose we don't have a candidate to add               !!!
  DO i=1,nflags                                                              !!!
    ! Looking for a non-added flag                                           !!!
    IF(.not.ok(i))THEN                                                       !!!
      done=.false. ! Found a non-added, so we are not done yet               !!!
      ! Look for an added atom from the i-neighbors                          !!!
      DO j=1,nflags                                                          !!!
        IF((Adj(i,j).eq.1).AND.(ok(j)))THEN ! If we find it:                 !!!
          l=i                              ! Mark the added atom             !!!
          candidate=.true.                 ! Remark we have a candidate      !!!
          ok(i)=.true.                     ! Mark it as added (ok)           !!!
          cluster(i)=ncluster              ! Add to the cluster              !!!
          EXIT                             ! Exit the search                 !!!
        END IF                                                               !!!
      END DO                                                                 !!!
      IF(candidate) EXIT ! Candidate found, exit this loop                   !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(done) EXIT ! If done, exit                                              !!!
  IF(.not.candidate)THEN                                                     !!!
    EXIT                                                                     !!!
  END IF                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE bp_add                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_bipt                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
