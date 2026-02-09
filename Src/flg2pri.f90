!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2pri                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_maps                                                               !!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,nflags,ntramaps,fi,ei,vi,ne,nv,nf2,nflags2,ne2,nv2              !!!
INTEGER:: n,i,j,l,nmaps                                                      !!!
INTEGER,ALLOCATABLE:: flag(:,:),nface(:),m2(:,:),m1(:,:)                     !!!
INTEGER,ALLOCATABLE:: flag_color(:),neigh_flag(:,:),mapa(:),map(:,:)         !!!
INTEGER,ALLOCATABLE:: face(:),edge(:),vertex(:),fev(:,:,:)                   !!!
INTEGER,ALLOCATABLE:: f2f(:),e2e(:),v2v(:)                                   !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:)                               !!!
CHARACTER*100:: filename                                                     !!!
LOGICAL,ALLOCATABLE:: ok_f(:),ok_e(:),ok_v(:)                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading the flag graph from a .flg or a .b.flg file                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_flg(nf,ne,nv,nflags,nface,flag,neigh_flag,flag_color,filename)     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Neighbors matrices (fev_flag.f90)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! First Neighbors                                                            !!!
ALLOCATE(m2(nflags,nflags))                                                  !!!
CALL first_neighbors(nflags,neigh_flag,m2)                                   !!!
! Second Neighbors                                                           !!!
ALLOCATE(m1(nflags,nflags))                                                  !!!
CALL second_neighbors(nflags,flag,m2,m1,flag_color)                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating maps (fev_maps.f90)                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating the maps                                                          !!!
CALL creating_maps(nflags,flag,neigh_flag,m1,m2,nf,nface,nmaps,maps,maps_nfixed)
! Selecting only translation maps                                            !!!
ALLOCATE(mapa(nflags),map(nmaps,nflags))                                     !!!
ntramaps=0                                                                   !!!
DO n=1,nmaps                                                                 !!!
  IF(maps_nfixed(n).eq.0)THEN                                                !!!
    IF(flag_color(1)*flag_color(maps(n,1)).eq.1)THEN                         !!!
      ntramaps=ntramaps+1                                                    !!!
      map(ntramaps,:)=maps(n,:)                                              !!!
    END IF                                                                   !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! New sizes                                                                  !!!
nflags2=nflags/(ntramaps+1)                                                  !!!
nf2=nf/(ntramaps+1)                                                          !!!
ne2=ne/(ntramaps+1)                                                          !!!
nv2=nv/(ntramaps+1)                                                          !!!
IF(ntramaps.eq.0)THEN                                                        !!!
  WRITE(*,'(A)') 'Structure in the '//TRIM(ADJUSTL(filename))// &            !!!
                        & '.flg file is already a primitive cell.'           !!!
ELSE                                                                         !!!
  WRITE(*,'(A)') 'Structure in the '//TRIM(ADJUSTL(filename))// &            !!!
                        & '.flg file is a conventional cell.'                !!!
END IF                                                                       !!!
ALLOCATE(face(nf),edge(ne),vertex(nv))                                       !!!
ALLOCATE(ok_f(nf),ok_e(ne),ok_v(nv))                                         !!!
ALLOCATE(f2f(nf),e2e(ne),v2v(nv))                                            !!!
! ok indicates if the element has been marked already                        !!!
ok_f=.false.                                                                 !!!
ok_e=.false.                                                                 !!!
ok_v=.false.                                                                 !!!
fi=0                                                                         !!!
ei=0                                                                         !!!
vi=0                                                                         !!!
f2f=0                                                                        !!!
e2e=0                                                                        !!!
v2v=0                                                                        !!!
DO i=1,nflags                                                                !!!
  ! If face not marked yet                                                   !!!
  IF(.not.ok_f(flag(i,1)))THEN                                               !!!
    fi=fi+1                                                                  !!!
    ! Get the list of equivalent faces                                       !!!
    l=1                                                                      !!!
    face(l)=flag(i,1)                                                        !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      face(l)=flag(map(j,i),1)                                               !!!
    END DO                                                                   !!!
    ! Set these faces as marked                                              !!!
    ok_f(face(1:l))=.true.                                                   !!!
    ! Map them all to fi                                                     !!!
    f2f(face(1:l))=fi                                                        !!!
  END IF                                                                     !!!
  ! If edge not marked yet                                                   !!!
  IF(.not.ok_e(flag(i,2)))THEN                                               !!!
    ei=ei+1                                                                  !!!
    ! Get the list of equivalent edges                                       !!!
    l=1                                                                      !!!
    edge(l)=flag(i,2)                                                        !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      edge(l)=flag(map(j,i),2)                                               !!!
    END DO                                                                   !!!
    ! Set these edges as marked                                              !!!
    ok_e(edge(1:l))=.true.                                                   !!!
    ! Map them all to ei                                                     !!!
    e2e(edge(1:l))=ei                                                        !!!
  END IF                                                                     !!!
  ! If vertex not marked yet                                                 !!!
  IF(.not.ok_v(flag(i,3)))THEN                                               !!!
    vi=vi+1                                                                  !!!
    ! Get the list of equivalent vertices                                    !!!
    l=1                                                                      !!!
    vertex(l)=flag(i,3)                                                      !!!
    DO j=1,ntramaps                                                          !!!
      l=l+1                                                                  !!!
      vertex(l)=flag(map(j,i),3)                                             !!!
    END DO                                                                   !!!
    ! Set these vertices as marked                                           !!!
    ok_v(vertex(1:l))=.true.                                                 !!!
    ! Map them all to vi                                                     !!!
    v2v(vertex(1:l))=vi                                                      !!!
  END IF                                                                     !!!
END DO                                                                       !!!
! Relabelling flags                                                          !!!
flag(:,1)=f2f(flag(:,1))                                                     !!!
flag(:,2)=e2e(flag(:,2))                                                     !!!
flag(:,3)=v2v(flag(:,3))                                                     !!!
! Building primitive FEV tensor                                              !!!
ALLOCATE(fev(nf2,ne2,nv2))                                                   !!!
fev=0                                                                        !!!
DO i=1,nflags                                                                !!!
  fev(flag(i,1),flag(i,2),flag(i,3))=1                                       !!!
END DO                                                                       !!!
! Writing primitive FEV tensor                                               !!!
filename=TRIM(ADJUSTL(filename))//'primitive'                                !!!
CALL write_fev(nf2,ne2,nv2,fev,filename)                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Deallocations                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DEALLOCATE(flag)                                                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2pri                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
