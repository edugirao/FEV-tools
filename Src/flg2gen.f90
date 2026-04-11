!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2gen                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool to create the generation of structures with N+1 faces from the gen    !!!
! of structures with N faces. Uses the .b.fcs format as basic instrument.    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_adim                                                               !!!
USE tools_conv                                                               !!!
USE tools_maps                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf0,ne0,nv0,nf1,ne1,nv1,ie1,ie2,nflags0,nflags1              !!!
INTEGER:: f0,f1,f2,f3,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                       !!!
INTEGER:: nmax0,nmaps,ntmaps,nsys,nsys_p,isys,fp                             !!!
INTEGER,ALLOCATABLE:: flag0(:,:),neigh_flag0(:,:),flag_color0(:),nface0(:)   !!!
INTEGER,ALLOCATABLE:: flag1(:,:),neigh_flag1(:,:),flag_color1(:),nface1(:)   !!!
INTEGER,ALLOCATABLE:: flag2(:,:),neigh_flag2(:,:),flag_color2(:),nface2(:)   !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:),fp_gen(:)                     !!!
INTEGER,ALLOCATABLE:: e_in_f0(:,:)                                           !!!
LOGICAL,ALLOCATABLE:: u_in_f0(:,:),uneq_face0(:)                             !!!
INTEGER,ALLOCATABLE:: init_flag0(:)                                          !!!
INTEGER:: flag_1a,flag_1b,flag_2a,flag_2b,flag_3a,flag_3b,flag_4a,flag_4b    !!!
INTEGER:: flag_a,flag_b,flag_c                                               !!!
LOGICAL:: file_exist,same                                                    !!!
CHARACTER*100:: filename,tmpfile,gensize,aux,trialfile                       !!!
CHARACTER*100,ALLOCATABLE:: gen_filenames(:)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying parent generation                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Structures' size                                                           !!!
CALL read_igen(nf0,'igen')                                                   !!!
! Create gen-0 if the case                                                   !!!
! SUBROUTINE write_graphene(filename,fev,flg,fcs,nxy,xyz,gen)                !!!
IF(nf0.eq.0) CALL write_graphene('6',1,1,1,0,0,1)                            !!!
! Generation size                                                            !!!
CALL read_gensize(nf0,nsys_p)                                                !!!
! Reading generation filenames                                               !!!
CALL read_genfilenames(nf0,nsys_p,gen_filenames,fp_gen)                      !!!
! Starting next generation size                                              !!!
WRITE(gensize,'(I0)') nf0+1                                                  !!!
OPEN(UNIT=1,FILE='gensize'//TRIM(ADJUSTL(gensize)))                          !!!
WRITE(1,*) 0                                                                 !!!
CLOSE(UNIT=1)                                                                !!!
nsys=0                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Running over all the structures of the parent database                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO isys=1,nsys_p                                                             !!!
  ! Evolution bar                                                            !!!
  WRITE(*,'(I0,A,I0)') isys,'/',nsys_p                                       !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting parent system filelabel                                          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Getting parent structure's filename & ancestor's dimer added face        !!!
  filename=gen_filenames(isys) ! Ancestor's filename                         !!!
  fp=fp_gen(isys)              ! Ancestor's face where dimmer was added      !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reading flag graph for the parent system (allocations inside read_flg)   !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL read_flg(nf0,ne0,nv0,nflags0,nface0,flag0,neigh_flag0,flag_color0,filename)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting initial flags for each face                                      !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(init_flag0(nf0))                                                  !!!
  init_flag0=0                                                               !!!
  DO i=1,nflags0                                                             !!!
    IF((flag_color0(i).eq.1).AND.(init_flag0(flag0(i,1)).eq.0))THEN          !!!
      init_flag0(flag0(i,1))=i                                               !!!
    END IF                                                                   !!!
  END DO                                                                     !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting e_in_f for the parent system                                     !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  nmax0=MAXVAL(nface0)                                                       !!!
  ALLOCATE(e_in_f0(nf0,nmax0))                                               !!!
  e_in_f0=0                                                                  !!!
  DO i=1,nf0                                                                 !!!
    flag_a=init_flag0(i)                                                     !!!
    DO j=1,nface0(i)                                                         !!!
      e_in_f0(i,j)=flag0(flag_a,2)                                           !!!
      flag_a=neigh_flag0(flag_a,3)                                           !!!
      flag_a=neigh_flag0(flag_a,2)                                           !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting maps for the parent system                                       !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  CALL flg2map(nflags0,flag0,neigh_flag0,flag_color0,nf0,nface0,nmaps,maps,maps_nfixed)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting uneq_face and u_in_f for the parent system                       !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
  ALLOCATE(uneq_face0(nf0),u_in_f0(nf0,nmax0))
  CALL neq_fe(nflags0,flag0,nf0,ne0,nface0,nmax0,e_in_f0,uneq_face0,u_in_f0,nmaps,maps)
  DEALLOCATE(maps,maps_nfixed,e_in_f0)  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Allocating children data                                                 !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  nf1=nf0+1                                                                  !!!
  ne1=ne0+3                                                                  !!!
  nv1=nv0+2                                                                  !!!
  nflags1=6*nv1                                                              !!!  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Running add dimer procedure for the parent system                        !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Who's F0?                                                                !!!
  DO f0=fp,nf0!iif,iif                                                       !!!
    IF(.not.uneq_face0(f0)) CYCLE                                            !!!
    ! Initial flag for the face f0                                           !!!
    flag_a=init_flag0(f0)                                                    !!!
    DO ie1=1,nface0(f0) ! ie1 gives E1                                       !!!
      ! Skipping if not a unique flag                                        !!!
      IF(.not.u_in_f0(f0,ie1))THEN                                           !!!
        ! But before, move flag_a one edge ahead                             !!!
        flag_a=neigh_flag0(flag_a,3)                                         !!!
        flag_a=neigh_flag0(flag_a,2)                                         !!!
        CYCLE                                                                !!!
      END IF                                                                 !!!
      ! Border flags                                                         !!!
      flag_1a=flag_a                                                         !!!
      flag_1b=neigh_flag0(flag_1a,1)                                         !!!
      ! F1/E1/V1 from the current flag_a                                     !!!
      f1=flag0(neigh_flag0(flag_a,1),1)                                      !!!
      e1=flag0(flag_a,2)                                                     !!!
      v1=flag0(flag_a,3)                                                     !!!
      ! Move flag_a one edge ahead (part 1 - v-swap)                         !!!
      flag_a=neigh_flag0(flag_a,3)                                           !!!
      ! Border flags                                                         !!!
      flag_4a=flag_a                                                         !!!
      flag_4b=neigh_flag0(flag_4a,1)                                         !!!
      ! Move flag_a one edge ahead (part 2 - e-swap)                         !!!
      flag_a=neigh_flag0(flag_a,2)                                           !!!
      ! V4 from the moved flag_a                                             !!!
      v4=flag0(flag_a,3)                                                     !!!
      ! Moved flag_a will be starting flag_b for the other edge loop         !!!
      flag_b=flag_a                                                          !!!
      DO ie2=ie1+1,nface0(f0) ! ie2 gives E2                                 !!!
        ! Border flags                                                       !!!
        flag_3a=flag_b                                                       !!!
        flag_3b=neigh_flag0(flag_3a,1)                                       !!!
        ! F2/E2/V3 from the current flag_b                                   !!!
        f2=flag0(neigh_flag0(flag_b,1),1)                                    !!!
        e2=flag0(flag_b,2)                                                   !!!
        v3=flag0(flag_b,3)                                                   !!!
!         IF(e1.eq.e2) print*,'equal'
!         IF(e1.ne.e2) print*,'diffe'
        ! Move flag_b one edge ahead (part 1)                                !!!
        flag_b=neigh_flag0(flag_b,3)                                         !!!
        ! Border flags                                                       !!!
        flag_2a=flag_b                                                       !!!
        flag_2b=neigh_flag0(flag_2a,1)                                       !!!
        ! Move flag_b one edge ahead (part 2)                                !!!
        flag_b=neigh_flag0(flag_b,2)                                         !!!
        ! V2 from the moved flag_b                                           !!!
        v2=flag0(flag_b,3)                                                   !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Expansion                                                          !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        f3=nf0+1                                                             !!!
        e3=ne0+1                                                             !!!
        e4=ne0+2                                                             !!!
        e0=ne0+3                                                             !!!
        v5=nv0+1                                                             !!!
        v6=nv0+2                                                             !!!        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Repetition                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ALLOCATE(nface1(nf1))                                                !!!
        ALLOCATE(flag1(nflags1,3),neigh_flag1(nflags1,3),flag_color1(nflags1))
        nface1(1:nf0)=nface0                                                 !!!
        flag1(1:nflags0,:)=flag0(:,:)                                        !!!
        neigh_flag1(1:nflags0,:)=neigh_flag0                                 !!!
        flag_color1(1:nflags0)=flag_color0                                   !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Inheritance                                                        !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! F3 inherites flags between V4 and V3 from F0                       !!!
        flag_c=flag_a ! Starting inherited flag                              !!!
        DO                                                                   !!!
          ! Leave when reaching E3 flag                                      !!!
          IF(flag_c.eq.flag_3a) EXIT                                         !!!
          ! Checking if F3 inherites F1
          IF(flag_c.eq.flag_4b) f1=f3
          ! Checking if F3 inherites F2
          IF(flag_c.eq.flag_2b) f2=f3
          ! Correcting face for inherited flag                               !!!
          flag1(flag_c,1)=f3                                                 !!!
          ! Move flag_c one edge ahead (part 1 - v-swap)                     !!!
          flag_c=neigh_flag0(flag_c,3)                                       !!!
          ! Correcting face for inherited flag                               !!!
          flag1(flag_c,1)=f3                                                 !!!
          ! Move flag_c one edge ahead (part 2 - e-swap)                     !!!
          flag_c=neigh_flag0(flag_c,2)                                       !!!
        END DO                                                               !!!
        ! F3 inherites flag before V4                                        !!!
        flag1(flag_4a,1)=f3                                                  !!!
        ! E4 inherites flags before V4                                       !!!
        flag1(flag_4a,2)=e4                                                  !!!
        flag1(flag_4b,2)=e4                                                  !!!
        ! F3 inherites flag after V3                                         !!!
        flag1(flag_3a,1)=f3                                                  !!!
        ! E3 inherites flags after V3 (but not when e1=e2)                   !!!
        IF(e1.ne.e2)THEN                                                     !!!
          flag1(flag_3a,2)=e3                                                !!!
          flag1(flag_3b,2)=e3                                                !!!
        END IF                                                               !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Creation                                                           !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!        
        ! New flags from V5                                                  !!!
        flag1(nflags0+1,1)=f0                                                !!!
        flag1(nflags0+1,2)=e1                                                !!!
        flag1(nflags0+1,3)=v5                                                !!!
        flag_color1(nflags0+1)=-flag_color1(flag_1a)
        flag1(nflags0+2,1)=f0                                                !!!
        flag1(nflags0+2,2)=e0                                                !!!
        flag1(nflags0+2,3)=v5                                                !!!
        flag_color1(nflags0+2)=flag_color1(flag_1a)
        flag1(nflags0+3,1)=f3                                                !!!
        flag1(nflags0+3,2)=e0                                                !!!
        flag1(nflags0+3,3)=v5                                                !!!
        flag_color1(nflags0+3)=-flag_color1(flag_1a)        
        flag1(nflags0+4,1)=f3                                                !!!
        flag1(nflags0+4,2)=e4 ! correct to e3 if e1=e2                       !!!
        flag1(nflags0+4,3)=v5                                                !!!
        flag_color1(nflags0+4)=flag_color1(flag_1a)
        flag1(nflags0+5,1)=f1                                                !!!
        flag1(nflags0+5,2)=e4 ! correct to e3 if e1=e2                       !!!
        flag1(nflags0+5,3)=v5                                                !!!
        flag_color1(nflags0+5)=-flag_color1(flag_1a)        
        flag1(nflags0+6,1)=f1                                                !!!
        flag1(nflags0+6,2)=e1                                                !!!
        flag1(nflags0+6,3)=v5                                                !!!
        flag_color1(nflags0+6)=flag_color1(flag_1a)
        ! New flags from V6                                                  !!!
        flag1(nflags0+7,1)=f0                                                !!!
        flag1(nflags0+7,2)=e2 ! correct to e3 if e1=e2                       !!!
        flag1(nflags0+7,3)=v6                                                !!!
        flag_color1(nflags0+7)=flag_color1(flag_3a)
        flag1(nflags0+8,1)=f0                                                !!!
        flag1(nflags0+8,2)=e0                                                !!!
        flag1(nflags0+8,3)=v6                                                !!!
        flag_color1(nflags0+8)=-flag_color1(flag_3a)
        flag1(nflags0+9,1)=f3                                                !!!
        flag1(nflags0+9,2)=e0                                                !!!
        flag1(nflags0+9,3)=v6                                                !!!
        flag_color1(nflags0+9)=flag_color1(flag_3a)        
        flag1(nflags0+10,1)=f3                                               !!!
        flag1(nflags0+10,2)=e3 ! correct to e4 if e1=e2                      !!!
        flag1(nflags0+10,3)=v6                                               !!!
        flag_color1(nflags0+10)=-flag_color1(flag_3a)
        flag1(nflags0+11,1)=f2 ! correct to f3 if e1=e2                      !!!
        flag1(nflags0+11,2)=e3 ! correct to e4 if e1=e2                      !!!
        flag1(nflags0+11,3)=v6                                               !!!
        flag_color1(nflags0+11)=flag_color1(flag_3a)        
        flag1(nflags0+12,1)=f2  ! correct to f3 if e1=e2                     !!!
        flag1(nflags0+12,2)=e2  ! correct to e3 if e1=e2                     !!!
        flag1(nflags0+12,3)=v6                                               !!!
        flag_color1(nflags0+12)=-flag_color1(flag_3a)
        ! Corrections if e1=e2
        IF(e1.eq.e2)THEN
          flag1(nflags0+4,2)=e3                                              !!!
          flag1(nflags0+5,2)=e3                                              !!!
          flag1(nflags0+7,2)=e3                                              !!!
          flag1(nflags0+10,2)=e4                                             !!!
          flag1(nflags0+11,1)=f3                                             !!!
          flag1(nflags0+11,2)=e4                                             !!!
          flag1(nflags0+12,1)=f3                                             !!!
          flag1(nflags0+12,2)=e3                                             !!!
        END IF
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Creating connections                                               !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!        
        ! V1-to-V5 (substitutes V2-to-V6 if e1=e2)
        neigh_flag1(flag_1a,3)=nflags0+1
        neigh_flag1(nflags0+1,3)=flag_1a
        neigh_flag1(nflags0+1,2)=nflags0+2
        neigh_flag1(nflags0+1,1)=nflags0+6
        neigh_flag1(flag_1b,3)=nflags0+6
        neigh_flag1(nflags0+6,3)=flag_1b
        neigh_flag1(nflags0+6,2)=nflags0+5
        neigh_flag1(nflags0+6,1)=nflags0+1        
        ! V3-to-V6 (it works as V4-V6 if e1=e2)
        neigh_flag1(flag_3a,3)=nflags0+10
        neigh_flag1(nflags0+10,3)=flag_3a
        neigh_flag1(nflags0+10,2)=nflags0+9
        neigh_flag1(nflags0+10,1)=nflags0+11
        neigh_flag1(flag_3b,3)=nflags0+11
        neigh_flag1(nflags0+11,3)=flag_3b
        neigh_flag1(nflags0+11,2)=nflags0+12
        neigh_flag1(nflags0+11,1)=nflags0+10
        ! V4-to-V5 (only for e1/=e2)
        IF(e1.ne.e2)THEN
          neigh_flag1(flag_4a,3)=nflags0+4
          neigh_flag1(nflags0+4,3)=flag_4a
          neigh_flag1(nflags0+4,2)=nflags0+3
          neigh_flag1(nflags0+4,1)=nflags0+5        
          neigh_flag1(flag_4b,3)=nflags0+5
          neigh_flag1(nflags0+5,3)=flag_4b
          neigh_flag1(nflags0+5,2)=nflags0+6
          neigh_flag1(nflags0+5,1)=nflags0+4
        END IF
        ! V2-to-V6 (only for e1/=e2)
        IF(e1.ne.e2)THEN
          neigh_flag1(flag_2a,3)=nflags0+7
          neigh_flag1(nflags0+7,3)=flag_2a
          neigh_flag1(nflags0+7,2)=nflags0+8
          neigh_flag1(nflags0+7,1)=nflags0+12        
          neigh_flag1(flag_2b,3)=nflags0+12
          neigh_flag1(nflags0+12,3)=flag_2b
          neigh_flag1(nflags0+12,2)=nflags0+11
          neigh_flag1(nflags0+12,1)=nflags0+7        
        END IF
        ! V5-to-V6 (through E0)
        neigh_flag1(nflags0+2,3)=nflags0+8
        neigh_flag1(nflags0+2,2)=nflags0+1
        neigh_flag1(nflags0+2,1)=nflags0+3        
        neigh_flag1(nflags0+8,3)=nflags0+2
        neigh_flag1(nflags0+8,2)=nflags0+7
        neigh_flag1(nflags0+8,1)=nflags0+9        
        neigh_flag1(nflags0+3,3)=nflags0+9
        neigh_flag1(nflags0+3,2)=nflags0+4
        neigh_flag1(nflags0+3,1)=nflags0+2        
        neigh_flag1(nflags0+9,3)=nflags0+3
        neigh_flag1(nflags0+9,2)=nflags0+10
        neigh_flag1(nflags0+9,1)=nflags0+8        
        IF(e1.eq.e2)THEN
          ! V5-to-V6 (through E3)
          neigh_flag1(nflags0+4,3)=nflags0+12
          neigh_flag1(nflags0+4,2)=nflags0+3
          neigh_flag1(nflags0+4,1)=nflags0+5        
          neigh_flag1(nflags0+5,3)=nflags0+7
          neigh_flag1(nflags0+5,2)=nflags0+6
          neigh_flag1(nflags0+5,1)=nflags0+4

          neigh_flag1(nflags0+7,3)=nflags0+5
          neigh_flag1(nflags0+7,2)=nflags0+8
          neigh_flag1(nflags0+7,1)=nflags0+12        
          neigh_flag1(nflags0+12,3)=nflags0+4
          neigh_flag1(nflags0+12,2)=nflags0+11
          neigh_flag1(nflags0+12,1)=nflags0+7
          
          
        END IF
        
!         ALLOCATE(mat(nflags1,nflags1))
!         mat=0
!         DO i=1,nflags1
!           mat(i,neigh_flag1(i,1))=1
!           mat(i,neigh_flag1(i,2))=1
!           mat(i,neigh_flag1(i,3))=1
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,1)).eq.0) stop 'BBBB0'
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,2)).eq.0) stop 'CCCC0'
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,3)).eq.0) stop 'DDDD0'
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,1)).eq.1) stop 'BBBB1'
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,2)).eq.1) stop 'CCCC1'
!           IF(flag_color1(i)*flag_color1(neigh_flag1(i,3)).eq.1) stop 'DDDD1'          
!         END DO
!         DO i=1,nflags1
!         DO l=i,nflags1
!           IF(mat(i,l).ne.mat(l,i)) stop 'AAAAA'
!         END DO
!         END DO
!         DEALLOCATE(mat)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Recalculating nface1 for F0/F1/F2/F3                               !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!       
        CALL face_size_from_flag(nface1(f0),flag_1a,nflags1,neigh_flag1)
        CALL face_size_from_flag(nface1(f3),flag_4a,nflags1,neigh_flag1)
!         IF(f1.ne.f0) CALL face_size_from_flag(nface1(f1),flag_1b,nflags1,neigh_flag1)
!         IF((f2.ne.f0).AND.(f2.ne.f1)) CALL face_size_from_flag(nface1(f2),flag_2b,nflags1,neigh_flag1)        
        IF((f1.ne.f0).AND.(f1.ne.f3)) CALL face_size_from_flag(nface1(f1),flag_1b,nflags1,neigh_flag1)
        IF((f2.ne.f0).AND.(f2.ne.f3).AND.(f2.ne.f1)) CALL face_size_from_flag(nface1(f2),flag_2b,nflags1,neigh_flag1)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! flg to map                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        CALL flg2map(nflags1,flag1,neigh_flag1,flag_color1,nf1,nface1,nmaps,maps,maps_nfixed)  ! alocs inside
        ntmaps=0                                                             !!!
        DO i=1,nmaps                                                         !!!
          IF(maps_nfixed(i).ne.0) CYCLE                                      !!!
          IF(flag_color1(1)*flag_color1(maps(i,1)).eq.1) ntmaps=ntmaps+1     !!!
        END DO                                                               !!!
        DEALLOCATE(maps,maps_nfixed)                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Comparing child with previous database                             !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Define new system's name                                           !!!
        CALL get_lname(nf1,nface1,tmpfile)                                  !!!
        same=.false.                                                         !!!
        l=0                                                                  !!!
        DO                                                                   !!!
          l=l+1                                                              !!!
          trialfile=tmpfile                                                  !!!
          ! If name has been taken before, add sufix                         !!!
          IF(l.gt.1)THEN                                                     !!!
            WRITE(aux,'(I0)') l                                              !!!
            trialfile=TRIM(ADJUSTL(tmpfile))//'_'//TRIM(ADJUSTL(aux))        !!!
          END IF                                                             !!!
          INQUIRE(FILE=TRIM(ADJUSTL(trialfile))//'.b.flg',EXIST=file_exist)  !!!        
          ! If file exist, compare                                           !!!
          IF(file_exist)THEN                                                 !!!
            ALLOCATE(nface2(nf1))                                                      !!!
            ALLOCATE(flag2(nflags1,3),neigh_flag2(nflags1,3),flag_color2(nflags1))     !!!          
            ! Get previous flag graph                                        !!!          
            CALL read_flg4kid(nf1,nflags1,nface2,flag2,neigh_flag2,trialfile)!!
            ! Compare with trial flag graph                                  !!!
            CALL check_equiv_fgraphs(nflags1,nf1,flag1,nface1,neigh_flag1, &
                                   & nflags1,nf1,flag2,nface2,neigh_flag2,same)
            DEALLOCATE(nface2,flag2,neigh_flag2,flag_color2)     !!!                                             
!             IF(.not.same) print*,TRIM(trialfile)
            IF(same) EXIT                                                    !!!
          ELSE                                                               !!!
            EXIT                                                             !!!
          END IF                                                             !!!
        END DO                                                               !!!
        ! New system?                                                        !!!
        IF(.not.same)THEN                                                    !!!
          IF(l.gt.1)THEN                                                     !!!
            tmpfile=TRIM(ADJUSTL(tmpfile))//'_'//TRIM(ADJUSTL(aux))          !!!
          END IF                                                             !!!          
          ! Updating database size                                           !!!
          nsys=nsys+1                                                        !!!
          OPEN(UNIT=1,FILE='gensize'//TRIM(ADJUSTL(gensize)))                !!!
          WRITE(1,*) nsys                                                    !!!
          CLOSE(UNIT=1)                                                      !!!
          ! Updating database by adding the new system                       !!!
          OPEN(UNIT=1,FILE='gensystems'//TRIM(ADJUSTL(gensize)),POSITION='APPEND')
          WRITE(1,*) tmpfile,nsys,f0,ntmaps                                  !!!
          CLOSE(UNIT=1)                                                      !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .b.flg                                                   !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          CALL write_flg_bin(nf1,ne1,nv1,nflags1,nface1,flag1,neigh_flag1,flag_color1,tmpfile)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .anc                                                     !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          CALL write_anc(f0,ie1,ie2,tmpfile,filename)                        !!!
        END IF                                                               !!!
        DEALLOCATE(nface1)                                                         !!!
        DEALLOCATE(flag1,neigh_flag1,flag_color1)                                  !!!          
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
  DEALLOCATE(init_flag0,uneq_face0,u_in_f0)                                                  !!!
!   DEALLOCATE(flag2,neigh_flag2,nface2)                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2gen                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



SUBROUTINE face_size_from_flag(nface,flag_0,nflags,neigh_flag)
IMPLICIT NONE
INTEGER:: nface,nflags,neigh_flag(nflags,3),flag_0,flag_i,i
LOGICAL:: closed
nface=1
flag_i=flag_0
closed=.false.
DO i=1,nflags
  flag_i=neigh_flag(flag_i,3)
  flag_i=neigh_flag(flag_i,2)
  IF(flag_i.eq.flag_0)THEN
    closed=.true.
    EXIT
  END IF
  nface=nface+1
END DO
IF(.not.closed) STOP 'Could not close a face.'

END SUBROUTINE face_size_from_flag


