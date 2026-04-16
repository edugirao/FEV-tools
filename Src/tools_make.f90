!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_make                                                            !!!
IMPLICIT NONE                                                                !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE reading(filename,nv,r,a1,a2)                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER,INTENT(OUT):: nv                                                     !!!
REAL(KIND=8),INTENT(OUT):: a1(3),a2(3)                                       !!!
REAL(KIND=8),ALLOCATABLE,INTENT(OUT):: r(:,:)                                !!!
CHARACTER*100,INTENT(IN):: filename                                          !!!
INTEGER:: i                                                                  !!!
CHARACTER*1:: atom                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.xyz')                            !!!
READ(1,*) nv                                                                 !!!
ALLOCATE(r(nv,3))                                                            !!!
READ(1,*)                                                                    !!!
DO i=1,nv                                                                    !!!
  READ(1,*) atom,r(i,:)            ! Reading atomic positions                !!!
END DO                                                                       !!!
CLOSE(UNIT=1)                                                                !!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.lat')                            !!!
READ(1,*) a1                       ! Reading lattice vector 1                !!!
READ(1,*) a2                       ! Reading lattice vector 1                !!!
CLOSE(UNIT=1)                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE reading                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE neighbors(nv,r,a1,a2,acc,tol,neigh,cell)                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nv,i,j,l,ii,jj,itmp,nn                                             !!!
INTEGER,ALLOCATABLE:: neigh(:,:),cell(:,:,:)                                 !!!
REAL(KIND=8):: acc,tol,a1(3),a2(3),v(3),theta(3),r(nv,3),tmp                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(neigh(nv,3),cell(nv,3,2))                                           !!!
neigh=0                                                                      !!!
cell=0                                                                       !!!
DO i=1,nv                                                                    !!!
  ! Computing i-neighbors                                                    !!!
  nn=0                                                                       !!!
  theta=0                                                                    !!!
  DO j=1,nv                                                                  !!!
    ! Scanning all the nv atoms from each neighboring cell                   !!!
    DO ii=-1,1                                                               !!!
      DO jj=-1,1                                                             !!!
        v=r(j,:)+ii*a1+jj*a2-r(i,:)                                          !!!
        IF(ABS(DSQRT(SUM(v**2))-acc).lt.tol)THEN                             !!!
          ! Distance inside the range?                                       !!!
          nn=nn+1         ! Update number of neighs                          !!!
          IF(nn.eq.4) STOP 'Too many neighbors.'                             !!!
          neigh(i,nn)=j   ! Store the neighbor index                         !!!
          cell(i,nn,1)=ii ! Store the neighbor cell                          !!!
          cell(i,nn,2)=jj ! indexes ii (a1) and jj (a2)                      !!!
          theta(nn)=ATAN2(v(1),v(2))                                         !!!
        END IF                                                               !!!
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
  ! Checking if correct number of neighbors                                  !!!
  IF(nn.ne.3) STOP 'Atom with wrong number fo neighbors.'                    !!!
  ! Ordering i-neighbors                                                     !!!
  DO l=2,1,-1                                                                !!!
    DO ii=1,l                                                                !!!
      jj=ii+1                                                                !!!
      IF(theta(ii).gt.theta(jj))THEN                                         !!!
        itmp=neigh(i,ii)                                                     !!!
        neigh(i,ii)=neigh(i,jj)                                              !!!
        neigh(i,jj)=itmp                                                     !!!
        itmp=cell(i,ii,1)                                                    !!!
        cell(i,ii,1)=cell(i,jj,1)                                            !!!
        cell(i,jj,1)=itmp                                                    !!!
        itmp=cell(i,ii,2)                                                    !!!
        cell(i,ii,2)=cell(i,jj,2)                                            !!!
        cell(i,jj,2)=itmp                                                    !!!
        tmp=theta(ii)                                                        !!!
        theta(ii)=theta(jj)                                                  !!!
        theta(jj)=tmp                                                        !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE neighbors                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE make_edges(nv,neigh,cell,ne,edge,nedge)                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: nv,i,j,ie,ne,vcell(2),i1,i2,l                                      !!!
INTEGER,ALLOCATABLE:: neigh(:,:),cell(:,:,:),edge(:,:),nedge(:,:),ecell(:,:) !!!
LOGICAL:: new                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ne=3*nv/2                                                                    !!!
ALLOCATE(edge(ne,2),nedge(nv,3),ecell(ne,2))                                 !!!
ie=0                                                                         !!!
edge=0                                                                       !!!
nedge=0                                                                      !!!
DO i=1,nv                                                                    !!!
  DO j=1,3                                                                   !!!
    ! Check for a new edge                                                   !!!
    i1=i ; i2=neigh(i,j) ; vcell(1)=cell(i,j,1) ; vcell(2)=cell(i,j,2)       !!!
    IF(i.gt.neigh(i,j))THEN ! edge(x,1) has to be lower than edge(x,2)       !!!
      i1=neigh(i,j) ; i2=i ; vcell=-vcell                                    !!!
    END IF                                                                   !!!
    new=.true.   ! In principle a new cell                                   !!!
    DO l=1,ie    ! Compare with the previous edges                           !!!
      IF(i1.eq.edge(l,1))THEN   ! Same vertex 1?                             !!!
        IF(i2.eq.edge(l,2))THEN ! Same vertex 2?                             !!!
          IF(vcell(1).eq.ecell(l,1))THEN   ! Same cell vector:               !!!
            IF(vcell(2).eq.ecell(l,2))THEN ! ii and jj?                      !!!
              nedge(i,j)=l                                                   !!!
              new=.false.   ! Not a new edge                                 !!!
              EXIT                                                           !!!
            END IF                                                           !!!
          END IF                                                             !!!
        END IF                                                               !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
    ! If got a new edge, add it to the set                                   !!!
    IF(new)THEN                                                              !!!
      ie=ie+1                                                                !!!
      edge(ie,1)=i1                                                          !!!
      edge(ie,2)=i2                                                          !!!
      ecell(ie,1)=vcell(1)                                                   !!!
      ecell(ie,2)=vcell(2)                                                   !!!
      nedge(i,j)=ie                                                          !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Checking if all edges were found                                           !!!
IF(ie.ne.ne) STOP 'Missing edge(s).'                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE make_edges                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE faces_maker(nv,ne,nf,neigh,cell,nedge,nface,face_edges,face_verts)!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,ii,jj,ll,ne,e                                                !!!
INTEGER:: nv,nf,RR(2)                                                        !!!
INTEGER,ALLOCATABLE:: face_edges(:,:),face_verts(:,:),nface(:)               !!!
INTEGER:: vpassed(nv),epassed(ne),neigh(nv,3),cell(nv,3,2)                   !!!
INTEGER:: nedge(nv,3),i_f,ii_last                                            !!!
LOGICAL:: cancel                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nf=nv/2                                                                      !!!
ALLOCATE(nface(nf),face_edges(nf,3*nv),face_verts(nf,3*nv))                  !!!
vpassed=0                                                                    !!!
epassed=0                                                                    !!!
i_f=0                                                                        !!!
DO i=1,nv                                                                    !!!
  ! Let's look for faces passing by vertex i                                 !!!
  IF(vpassed(i).eq.6) CYCLE ! Skip if passed here 6 times                    !!!
  ! Trying a face for each edge emerging from i                              !!!
  DO j=1,3                                                                   !!!
    ! Identifying the edge                                                   !!!
    e=nedge(i,j)                                                             !!!
    ! Skip if passed here 2 times                                            !!!
    IF(epassed(e).eq.2) CYCLE                                                !!!
    ! Update times passed in this edge                                       !!!
    epassed(e)=epassed(e)+1                                                  !!!
    ! Update times passed in vertex i                                        !!!
    vpassed(i)=vpassed(i)+1                                                  !!!
    ! Update times passed in i's j-th neighbor                               !!!
    vpassed(neigh(i,j))=vpassed(neigh(i,j))+1                                !!!
    ! Update number of faces                                                 !!!
    i_f=i_f+1                                                                !!!
    ! Store last face vertex                                                 !!!
    ii_last=neigh(i,j)                                                       !!!
    ! Set first face's edge                                                  !!!
    face_edges(i_f,1)=e                                                      !!!
    ! Set first face's vertex                                                !!!
    face_verts(i_f,1)=i                                                      !!!
    ! Start accumulated Bravais traslation                                   !!!
    RR(:)=cell(i,j,:)                                                        !!!
    ! Start face size                                                        !!!
    nface(i_f)=1                                                             !!!
    ii=i ! Edge merging from this vertex                                     !!!
    jj=j ! Edge arriving at this neighbor                                    !!!
    cancel=.false. ! Starting cancel check as false                          !!!
    DO                                                                       !!!
      ! New emerging vertex (l)                                              !!!
      jj=jj+1          ! Next (cyclic) ii's neigh...                         !!!
      IF(jj.eq.4) jj=1 ! ...will be the new...                               !!!
      l=neigh(ii,jj)   ! ...emerging vertex.                                 !!!
      ! New arriving neighbor index (jj)                                     !!!
      jj=0      ! Previous emerging atom is now...                           !!!
      DO ll=1,3 ! ...the new arriving neighbor.                              !!!
        IF(neigh(l,ll).eq.ii) jj=ll                                          !!!
      END DO                                                                 !!!
      IF(jj.eq.0) STOP 'Next arriving neighbor not found.'                   !!!
      ! New edge                                                             !!!
      e=nedge(l,jj)                                                          !!!
      ! Updating times passed by the edge                                    !!!
      epassed(e)=epassed(e)+1                                                !!!
      ! Update face size                                                     !!!
      nface(i_f)=nface(i_f)+1                                                !!!
      ! Set next face's edge                                                 !!!
      face_edges(i_f,nface(i_f))=e                                           !!!
      ! Set next face's vertex                                               !!!
      face_verts(i_f,nface(i_f))=l                                           !!!
      ! Updating times passed by vertices                                    !!!
      vpassed(l)=vpassed(l)+1   ! Emerging vertex                            !!!
      vpassed(ii)=vpassed(ii)+1 ! Arriving neighbor                          !!!
      ! Checking if passed too much                                          !!!
      IF((vpassed(l).eq.7).or.(epassed(ii).eq.3))THEN                        !!!
        cancel=.true.                                                        !!!
        EXIT                                                                 !!!
      END IF                                                                 !!!
      ! Moving emerging vertex from l to ii                                  !!!
      ii=l                                                                   !!!
      ! Updating Bravais                                                     !!!
      RR(:)=RR(:)+cell(ii,jj,:)                                              !!!
      ! Check for closed face                                                !!!
      IF((ii.eq.ii_last).AND.(SUM(RR**2).eq.0)) EXIT                         !!!
    END DO                                                                   !!!
    ! Checking cancel-case                                                   !!!
    IF(cancel)THEN                                                           !!!
      DO ll=1,nface(i_f)                                                     !!!
        vpassed(face_verts(i_f,ll))=vpassed(face_verts(i_f,ll))-1            !!!
        epassed(face_edges(i_f,ll))=epassed(face_edges(i_f,ll))-1            !!!
      END DO                                                                 !!!
      nface(i_f)=0                                                           !!!
      i_f=i_f-1                                                              !!!
      CYCLE                                                                  !!!
    END IF                                                                   !!!
    ! Not cancelled? -> Print face data                                      !!!
    print*,nface(i_f),'e',face_edges(i_f,1:nface(i_f))                       !!!
    print*,nface(i_f),'v',face_verts(i_f,1:nface(i_f))                       !!!
  END DO                                                                     !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE faces_maker                                                   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE flg_maker(nf,nv,nface,face_edges,face_verts,nflags,flag)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,k,l,l1,l2,nf,nv,nflags                               !!!
INTEGER:: face_edges(nf,3*nv),face_verts(nf,3*nv),nface(nf)                  !!!
INTEGER,ALLOCATABLE:: flag(:,:)                !!!
LOGICAL:: getout                                                             !!!
LOGICAL,ALLOCATABLE:: ok(:),done(:)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nflags=nv*6                                                                  !!!
ALLOCATE(flag(nflags,-3:3))             !!!
flag=0                                                                       !!!
l=0                                                                          !!!
DO i=1,nf                                                                    !!!
  l1=l+1                                                                     !!!
  ! Building flags                                                           !!!
  DO j=1,nface(i)                                                            !!!
    l=l+1                                                                    !!!
    flag(l,1)=i                                                              !!!
    flag(l,2)=face_edges(i,j)                                                !!!
    flag(l,3)=face_verts(i,j)                                                !!!
    l=l+1                                                                    !!!
    k=j+1                                                                    !!!
    IF(j.eq.nface(i)) k=1                                                    !!!
    flag(l,1)=i                                                              !!!
    flag(l,2)=face_edges(i,k)                                                !!!
    flag(l,3)=face_verts(i,j)                                                !!!
  END DO                                                                     !!!
  ! Building edge and vertex neighbors                                       !!!
  l2=l                                                                       !!!
  DO j=l1,l2-1,2                                                             !!!
    ! Edge-neighs                                                            !!!
    flag(j,-2)=j+1                                                      !!!
    flag(j+1,-2)=j                                                      !!!
    ! Vertex-neighs                                                          !!!
    flag(j,-3)=j-1                                                      !!!
    flag(j+1,-3)=j+2                                                    !!!
  END DO                                                                     !!!
  ! Correcting extreme cases                                                 !!!
  flag(l1,-3)=l2                                                        !!!
  flag(l2,-3)=l1                                                        !!!
END DO                                                                       !!!
IF(l.ne.nflags) STOP 'Missing flags.'                                        !!!
! Building Face-neighbors                                                    !!!
DO i=1,nflags                                                                !!!
  DO j=i+1,nflags                                                            !!!
    IF(flag(i,2).eq.flag(j,2))THEN                                           !!!
      IF(flag(i,3).eq.flag(j,3))THEN                                         !!!
        flag(i,-1)=j                                                    !!!
        flag(j,-1)=i                                                    !!!
      END IF                                                                 !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Building flag-colors                                                       !!!
flag(1,0)=1                                                              !!!
flag(flag(1,-3:-1),0)=-1                                               !!!
ALLOCATE(ok(nflags),done(nflags))                                            !!!
ok=.false.                                                                   !!!
ok(1)=.true.                                                                 !!!
ok(flag(1,-3:-1))=.true.                                                   !!!
done=.false.                                                                 !!!
done(1)=.true.                                                               !!!
DO                                                                           !!!
  getout=.true.                                                              !!!
  DO i=1,nflags                                                              !!!
    IF(done(i)) CYCLE                                                        !!!
    IF(ok(i))THEN                                                            !!!
      flag(flag(i,-3:-1),0)=-flag(i,0)                             !!!
      ok(flag(i,-3:-1))=.true.                                             !!!
      done(i)=.true.                                                         !!!
      getout=.false.                                                         !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  IF(getout) EXIT                                                            !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE flg_maker                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE rflg_maker(nv,nflags,flag,neigh,cell,nedge,a1,a2,r,rflag,fcell)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: k,l,nv,nflags,i_v,i_e,m,s                               !!!
INTEGER,INTENT(IN),OPTIONAL:: neigh(nv,3),nedge(nv,3),cell(nv,3,2)           !!!
INTEGER,ALLOCATABLE:: flag(:,:)                !!!
INTEGER,ALLOCATABLE,INTENT(OUT),OPTIONAL:: fcell(:,:,:)                      !!!
REAL(KIND=8):: v(3),u(3),dv(3),d                                             !!!
REAL(KIND=8),ALLOCATABLE,INTENT(OUT),OPTIONAL:: rflag(:,:)                   !!!
REAL(KIND=8),INTENT(IN),OPTIONAL:: r(nv,3),a1(3),a2(3)                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(rflag(nflags,3),fcell(nflags,3,2))                                  !!!
fcell=0
DO l=1,nflags                                                            !!!
  s=1
  IF(MOD(l,2).eq.1) s=-1
  i_v=flag(l,3)
  v=r(i_v,:)
  i_e=flag(l,2)
  DO m=1,3
    IF(nedge(i_v,m).eq.i_e) k=m
  END DO
  dv=r(neigh(i_v,k),:)+cell(i_v,k,1)*a1+cell(i_v,k,2)*a2-v
  d=DSQRT(SUM(dv**2))
  u(1)=-dv(2)/d
  u(2)=dv(1)/d
  u(3)=0.0D0
  rflag(l,:)=v+0.3*dv+s*(0.3*d/DSQRT(3.0D0))*u
  fcell(l,3,:)=cell(i_v,k,:)
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE rflg_maker                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE fev_maker(nf,ne,nv,nface,face_edges,edge,fev)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,nf,ne,nv                                                       !!!
INTEGER,ALLOCATABLE:: fev(:,:,:)                                             !!!
INTEGER:: face_edges(nf,3*nv),nface(nf),edge(ne,2)                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(fev(nf,ne,nv))
fev=0
DO i=1,nf
  DO j=1,nface(i)
    fev(i,face_edges(i,j),edge(face_edges(i,j),1))=1
    fev(i,face_edges(i,j),edge(face_edges(i,j),2))=1
  END DO  
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE fev_maker                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_make                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
