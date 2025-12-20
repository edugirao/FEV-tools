!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE get_axes(nflags,flag,m1,filename)
IMPLICIT NONE
INTEGER:: i,j,n,p,nrot,n_c_axis,nflags,flag(nflags,3),fixed(3)
INTEGER:: m1(nflags,nflags),mapa(nflags),nmaps,nfixed,d_kro
INTEGER,ALLOCATABLE:: rot(:,:)
CHARACTER*100:: filename
CHARACTER*1:: element(3)
LOGICAL:: new
element(1)='f'
element(2)='e'
element(3)='v'
nrot=MAXVAL(flag(:,1))+MAXVAL(flag(:,2))+MAXVAL(flag(:,3))
ALLOCATE(rot(nrot,3))
rot=0
nrot=0
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')
READ(8,*) nmaps
DO i=1,nmaps
  READ(8,*) nfixed
  READ(8,*) mapa
  ! Checking for rotation symmetries
  IF(nfixed.eq.1)THEN
    ! Getting map rotation order
    n_c_axis=1
    CALL highest_axis(nflags,mapa,n_c_axis)
    ! Searching for axes inside the map
    DO n=1,nflags
      IF(m1(n,mapa(n)).eq.1)THEN
        DO j=1,3
          fixed=0
          IF(d_kro(flag(n,j),flag(mapa(n),j)).ne.1) CYCLE
          fixed(1)=j
          fixed(2)=flag(n,j)
          fixed(3)=n_c_axis
          IF(fixed(1).eq.0) STOP 'Problem in rot'
          new=.true.
          DO p=1,nrot
            IF((fixed(1).eq.rot(p,1)).AND.(fixed(2).eq.rot(p,2)))THEN
              new=.false.
              IF(n_c_axis.gt.rot(p,3)) rot(p,3)=fixed(3)
              EXIT
            END IF
          END DO
          IF(new)THEN
            nrot=nrot+1
            rot(nrot,1)=fixed(1)
            rot(nrot,2)=fixed(2)
            rot(nrot,3)=fixed(3)
          END IF
        END DO  
      END IF    
    END DO          
  END IF
END DO
CLOSE(UNIT=8)
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.rot')
WRITE(8,*) nrot
DO i=1,nrot
  IF(rot(i,1).ne.1) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(rot(i,1)),rot(i,2),rot(i,3)
END DO
DO i=1,nrot
  IF(rot(i,1).ne.2) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(rot(i,1)),rot(i,2),rot(i,3)
END DO
DO i=1,nrot
  IF(rot(i,1).ne.3) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(rot(i,1)),rot(i,2),rot(i,3)
END DO
CLOSE(UNIT=8)
END SUBROUTINE get_axes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE get_mirror(nflags,flag,m2,filename)
IMPLICIT NONE
INTEGER:: i,j,js,n,p,nmir,nflags,flag(nflags,3),fixed(3),v1(3),v2(3)
INTEGER:: m2(nflags,nflags),mapa(nflags),nmaps,nfixed,d_kro,tri_d_kro
INTEGER,ALLOCATABLE:: mir(:,:)
CHARACTER*100:: filename
CHARACTER*1:: element(3)
LOGICAL:: new
element(1)='f'
element(2)='e'
element(3)='v'
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')
READ(8,*) nmaps
nmir=MAXVAL(flag(:,1))+MAXVAL(flag(:,2))+MAXVAL(flag(:,3))*nmaps
ALLOCATE(mir(nmir,3))
mir=0
nmir=0
DO i=1,nmaps
  READ(8,*) nfixed
  READ(8,*) mapa
  ! Checking for rotation symmetries
  IF(nfixed.eq.2)THEN
    ! Searching for mirrors inside the map
    DO n=1,nflags
      IF(m2(n,mapa(n)).eq.1)THEN
        ! Searching for mirror elements inside the map
        js=1
        v1=flag(n,:)
        v2=flag(mapa(n),:)
        IF(tri_d_kro(v1,v2).eq.3) js=2
        DO j=js,3
          fixed=0
          IF(d_kro(flag(n,j),flag(mapa(n),j)).ne.1) CYCLE
          fixed(1)=j
          fixed(2)=flag(n,j)
          fixed(3)=i
          IF(fixed(1).eq.0) STOP 'Problem in mir'
          new=.true.
          DO p=1,nmir
            IF((fixed(1).eq.mir(p,1)).AND.(fixed(2).eq.mir(p,2)).AND.(fixed(3).eq.mir(p,3)))THEN
              new=.false.
              EXIT
            END IF
          END DO
          IF(new)THEN
            nmir=nmir+1
            mir(nmir,1)=fixed(1)
            mir(nmir,2)=fixed(2)
            mir(nmir,3)=fixed(3)
          END IF
        END DO          
      END IF    
    END DO          
  END IF
END DO
CLOSE(UNIT=8)
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.mir')
WRITE(8,'(I0)') nmir
DO i=1,nmir
  IF(mir(i,1).ne.1) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(mir(i,1)),mir(i,2),mir(i,3)
END DO
DO i=1,nmir
  IF(mir(i,1).ne.2) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(mir(i,1)),mir(i,2),mir(i,3)
END DO
DO i=1,nmir
  IF(mir(i,1).ne.3) CYCLE
  WRITE(8,'(A1,1X,2I7)') element(mir(i,1)),mir(i,2),mir(i,3)
END DO
CLOSE(UNIT=8)
END SUBROUTINE get_mirror

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_rot(nflags,flag,mapa,m1,nrot,rot,m)
IMPLICIT NONE
INTEGER:: n,j,p,q,m,nflags,flag(nflags,3),m1(nflags,nflags)
INTEGER:: mapa(nflags),fixed(2),rot(m,2)
INTEGER:: nrot,d_kro
LOGICAL:: new
CHARACTER*1:: element(0:3)
element(1)='f'
element(2)='e'
element(3)='v'
element(0)=' '
DO n=1,nflags
  IF(m1(n,mapa(n)).eq.1)THEN
    DO j=1,3
      fixed=0
      IF(d_kro(flag(n,j),flag(mapa(n),j)).ne.1) CYCLE
      fixed(1)=j
      fixed(2)=flag(n,j)
      IF(fixed(1).eq.0) STOP 'Problem in rot'
      new=.true.
      q=nrot
      DO p=1,q
        IF((fixed(1).eq.rot(p,1)).AND.(fixed(2).eq.rot(p,2)))THEN
          new=.false.
        END IF
      END DO            
      IF(new)THEN
        nrot=nrot+1
        rot(nrot,1)=fixed(1)
        rot(nrot,2)=fixed(2)
      END IF
    END DO  
  END IF    
END DO      
END SUBROUTINE check_rot

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE highest_axis(nflags,mapa,n_c_axis)
IMPLICIT NONE
INTEGER:: l,n,p,q,nflags,mapa(nflags),n_c_axis
DO l=1,nflags
  n=1
  p=l
  DO 
    q=mapa(p)
    IF(q.eq.l)THEN
      EXIT
    ELSE
      n=n+1
      p=q
    END IF
  END DO
  IF(n.gt.n_c_axis) n_c_axis=n
END DO
END SUBROUTINE highest_axis

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_mir(nflags,flag,mapa,m2,nmirror,mapa_mirror,m)
IMPLICIT NONE
INTEGER:: i,n,p,m,nflags,flag(nflags,3),mapa(nflags),m2(nflags,nflags)
INTEGER:: nmirror,mirror(m,4),fixed(4),mapa_mirror(m,nflags),d_kro
LOGICAL:: new
CHARACTER*1:: element(0:3)
element(1)='f'  ! We store two fixed elements for the i-th mirror in 
element(2)='e'  ! mirror(i,j), with j=1,3 being 1,2,3 for f,e,v element
element(3)='v'  ! and j=2,4 giving the index of the corresponding
element(0)=' '  ! mirror(i,1) and mirror(i,3) elements
! Looking for mirror elements among all flags
DO n=1,nflags
  ! We have fixed elements when a flag is mapped to one of its neighbors
  IF(m2(n,mapa(n)).eq.1)THEN  
    p=1     ! Starting index for mirror(n,p). But, before, ...
    fixed=0 ! ...we store a potential mirror(n,:) in fixed(:)
    ! Checking if the face is a fixed element
    IF(((flag(n,2)-flag(mapa(n),2))**2.ne.0) &
    & .OR.((flag(n,3)-flag(mapa(n),3))**2.ne.0))THEN
      !This "IF" is to avoid computing face as fixed in a false flag
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fixed(p)=1           ! "1" for face 
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fixed(p+1)=flag(n,1) ! Face index
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) p=p+2                ! Update counter
    END IF
    ! Checking if the face is a fixed element
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fixed(p)=2           ! "2" for edge
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fixed(p+1)=flag(n,2) ! Edge index
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) p=p+2                ! Update counter
    ! Checking if the face is a fixed element
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fixed(p)=3           ! "3" for vertex
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fixed(p+1)=flag(n,3) ! Vertex index
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) p=p+2                ! Update counter
    ! Error check
    IF(p.ne.5) STOP 'Problem in mirror.'
    ! Testing if new against the previous    
    new=.true.
    DO i=1,nmirror
      IF((fixed(1).eq.mirror(i,1)).AND.(fixed(2).eq.mirror(i,2)).AND. &
       & (fixed(3).eq.mirror(i,3)).AND.(fixed(4).eq.mirror(i,4)))THEN
        new=.false.
      END IF
    END DO                
    ! Adding mirror to the set if new
    IF(new)THEN
      nmirror=nmirror+1
      mirror(nmirror,1)=fixed(1)
      mirror(nmirror,2)=fixed(2)
      mirror(nmirror,3)=fixed(3)
      mirror(nmirror,4)=fixed(4)
      mapa_mirror(nmirror,:)=mapa
      EXIT
    END IF          
    ! If not new, we look for other representative
  END IF    
END DO      



END SUBROUTINE check_mir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE counting_mirrors(filename,nflags,flag,m2,mirror_count)
IMPLICIT NONE
INTEGER:: mapa(nflags),nflags,mirror_count,nfix,ilast
INTEGER:: flag(nflags,3),fixed(nflags,2),i,j,n,p,d_kro
INTEGER:: m2(nflags,nflags),fix(2,2),nmaps,nfixed
INTEGER:: neighfix(nflags,2),nnfix(nflags),ii(2),nmirmaps
INTEGER,ALLOCATABLE:: walk(:)
LOGICAL:: new,done,found,add
LOGICAL,ALLOCATABLE:: ok(:)
CHARACTER*100:: filename


mirror_count=0
nmirmaps=0
OPEN(UNIT=8,FILE=TRIM(ADJUSTL(filename))//'.map')
READ(8,*) nmaps
DO i=1,nmaps
  READ(8,*) nfixed
  IF(nfixed.eq.2)THEN
    READ(8,*) mapa
    nmirmaps=1
    EXIT
  END IF
END DO
CLOSE(UNIT=8)
IF(nmirmaps.eq.0) RETURN
nfix=0
fixed=0
neighfix=0
nnfix=0
DO n=1,nflags
  IF(m2(n,mapa(n)).eq.1)THEN
    p=0    ! In a mirror map, two mapped flags will have...
    fix=0  ! ...either 0 or 2 fixed elements
    IF(((flag(n,2)-flag(mapa(n),2))**2.ne.0) &
    & .OR.((flag(n,3)-flag(mapa(n),3))**2.ne.0))THEN
      ! The face will be a fixed element only if either the...
      !... edge or vertex isn't, to avoid problems with twins.
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) p=p+1    
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fix(p,1)=1
      IF(d_kro(flag(n,1),flag(mapa(n),1)).eq.1) fix(p,2)=flag(n,1)
    END IF
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) p=p+1
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fix(p,1)=2
    IF(d_kro(flag(n,2),flag(mapa(n),2)).eq.1) fix(p,2)=flag(n,2)
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) p=p+1
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fix(p,1)=3
    IF(d_kro(flag(n,3),flag(mapa(n),3)).eq.1) fix(p,2)=flag(n,3)
    IF(p.ne.2) STOP 'Problem in count mirror.'
    DO j=1,2
      new=.true.
      DO i=1,nfix
        IF((fixed(i,1).eq.fix(j,1)).AND.(fixed(i,2).eq.fix(j,2)))THEN
          ii(j)=i
          new=.false.
          EXIT
        END IF
      END DO
      IF(new)THEN
        nfix=nfix+1
        fixed(nfix,:)=fix(j,:)
        ii(j)=nfix
      END IF    
    END DO
    DO j=1,2
      add=.false.
      IF(nnfix(ii(j)).eq.0)THEN
        neighfix(ii(j),1)=ii(3-j)
        add=.true.
      ELSE IF(nnfix(ii(j)).eq.1)THEN  
        IF(neighfix(ii(j),1).ne.ii(3-j))THEN
          neighfix(ii(j),2)=ii(3-j)
          add=.true.
        END IF
      END IF
      IF(nnfix(ii(j)).lt.2)THEN
        IF(add) nnfix(ii(j))=nnfix(ii(j))+1
      END IF
    END DO  
  END IF    
END DO      

DO n=1,nfix
 IF(nnfix(n).eq.1)THEN
   neighfix(n,2)=neighfix(n,1)
   nnfix(n)=2
 END IF
END DO

ALLOCATE(walk(nfix),ok(nfix))
n=1
walk=0
ok=.false.
walk(1)=1
ok(1)=.true.
done=.false.
ilast=0
DO i=2,nfix+1
  found=.false.
  DO j=1,2
    IF(ok(neighfix(walk(i-1),j))) CYCLE
    walk(i)=neighfix(walk(i-1),j)
    ok(neighfix(walk(i-1),j))=.true.
    found=.true.
    EXIT
  END DO
  IF(.not.found)THEN
    IF((neighfix(walk(i-1),1).eq.1).OR.(neighfix(walk(i-1),2).eq.1))THEN
      ilast=i-1
      done=.true.    
    END IF
  END IF
  IF(done) EXIT
END DO
IF(.not.done) STOP 'Error in mirror count'
IF(ilast.eq.nfix) mirror_count=1
IF(ilast.ne.nfix) mirror_count=2

END SUBROUTINE counting_mirrors

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE axis_off_mirrors(filename,nocross)
IMPLICIT NONE
INTEGER:: nrot,nmir,i,j
INTEGER,ALLOCATABLE:: rot_n(:),mir_n(:)
LOGICAL:: nocross
CHARACTER*100:: filename
CHARACTER*1,ALLOCATABLE:: rot_e(:),mir_e(:)
OPEN(UNIT=7,FILE=TRIM(ADJUSTL(filename))//'.rot')
READ(7,*) nrot
ALLOCATE(rot_e(nrot),rot_n(nrot))
DO i=1,nrot
  READ(7,*) rot_e(i),rot_n(i)
END DO
CLOSE(UNIT=7)
OPEN(UNIT=7,FILE=TRIM(ADJUSTL(filename))//'.mir')
READ(7,*) nmir
ALLOCATE(mir_e(nmir),mir_n(nmir))
DO i=1,nmir
  READ(7,*) mir_e(i),mir_n(i)
END DO
CLOSE(UNIT=7)

DO i=1,nrot
  nocross=.true.
  DO j=1,nmir
    IF((rot_e(i).eq.mir_e(j)).AND.(rot_n(i).eq.mir_n(j)))THEN
      nocross=.false.
      EXIT
    END IF
  END DO
  IF(nocross) RETURN
END DO
END SUBROUTINE axis_off_mirrors

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE symmetry_group(nrotmaps,nmirmaps,nglimaps,filename,mirror_count,group)
IMPLICIT NONE
INTEGER:: nrotmaps,nmirmaps,nglimaps,mirror_count
LOGICAL:: nocross
CHARACTER*100:: filename
CHARACTER*4:: group

group=''
IF((nrotmaps.eq.5).AND.(nmirmaps.eq.6).AND.(nglimaps.eq.0)) group='p6m' ! p6mm
IF((nrotmaps.eq.5).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p6'
IF((nrotmaps.eq.2).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p3'
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.4).AND.(nglimaps.eq.0)) group='p4m' ! p4mm
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.2).AND.(nglimaps.eq.2)) group='p4g' ! p4gm
IF((nrotmaps.eq.3).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p4'
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p2'
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.2)) group='pgg' ! pgg2
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.1)) group='pmg' ! pmg2
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.1)) group='pg'
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.0).AND.(mirror_count.eq.1)) group='cm'
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.1).AND.(nglimaps.eq.0).AND.(mirror_count.eq.2)) group='pm'
IF((nrotmaps.eq.0).AND.(nmirmaps.eq.0).AND.(nglimaps.eq.0)) group='p1'
IF((nrotmaps.eq.1).AND.(nmirmaps.eq.2).AND.(nglimaps.eq.0))THEN
  ! Cheking for axis off-mirrors
  CALL axis_off_mirrors(filename,nocross)
  IF(nocross) group='cmm'
  IF(.not.nocross) group='pmm'
END IF
IF((nrotmaps.eq.2).AND.(nmirmaps.eq.3).AND.(nglimaps.eq.0))THEN
  ! Cheking for axis off-mirrors
  CALL axis_off_mirrors(filename,nocross)
  IF(nocross) group='p31m'
  IF(.not.nocross) group='p3m1'
END IF

END SUBROUTINE symmetry_group

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



