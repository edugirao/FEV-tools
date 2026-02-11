!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fcs2kid                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_adim                                                               !!!
USE tools_conv                                                               !!!
USE tools_maps                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf,ne,nv,ie1,ie2                                             !!!
INTEGER:: f0,f1,f2,f3,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                       !!!
INTEGER:: nmax,nmaps,ntmaps,nsys,nsys_p,isys                                 !!!
INTEGER:: nflags1,fp                                                         !!!
INTEGER,ALLOCATABLE:: flag1(:,:),neigh_flag1(:,:),flag_color1(:)             !!!
INTEGER,ALLOCATABLE:: flag2(:,:),neigh_flag2(:,:),nface2(:)                  !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:),fp_gen(:)                     !!!
INTEGER,ALLOCATABLE:: nface0(:)                                              !!!
INTEGER,ALLOCATABLE:: f_in_f0(:,:),e_in_f0(:,:),v_in_f0(:,:)                 !!!
LOGICAL,ALLOCATABLE:: b_in_f0(:,:),u_in_f0(:,:),uneq_face0(:)                !!!
INTEGER,ALLOCATABLE:: nface1(:)                                              !!!
INTEGER,ALLOCATABLE:: f_in_f1(:,:),e_in_f1(:,:),v_in_f1(:,:)                 !!!
LOGICAL,ALLOCATABLE:: b_in_f1(:,:),u_in_f1(:,:),uneq_face1(:)                !!!
LOGICAL:: file_exist,same                                                    !!!
CHARACTER*100:: filename,tmpfile,gensize,aux,trialfile                       !!!
CHARACTER*100,ALLOCATABLE:: gen_filenames(:)                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying parent generation                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Structures' size                                                           !!!
CALL read_igen(nf,'igen')                                                    !!!
! Create gen-0 if the case                                                   !!!
IF(nf.eq.0) CALL write_graphene(1,1,1,1,1,1)                                 !!!
! Generation size                                                            !!!
CALL read_gensize(nf,nsys_p)                                                 !!!
! Reading generation filenames                                               !!!
CALL read_genfilenames(nf,nsys_p,gen_filenames,fp_gen)                       !!!
! Starting next generation size                                              !!!
WRITE(gensize,'(I0)') nf+1                                                   !!!
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
  fp=fp_gen(isys)                                                            !!!
  filename=gen_filenames(isys)                                               !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reading .fcs info for the parent system (allocations inside read_fcs)    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL read_fcs(nf,ne,nv,nmax,nface0,uneq_face0,f_in_f0,e_in_f0,v_in_f0, &   !!!
                                            b_in_f0,u_in_f0,filename)        !!!
  ! Updating nmax for the children                                           !!!
  nmax=nmax+3                                                                !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Allocating children data                                                 !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(nface1(nf+1),uneq_face1(nf+1))                                    !!!
  ALLOCATE(f_in_f1(nf+1,nmax),e_in_f1(nf+1,nmax))                            !!!
  ALLOCATE(v_in_f1(nf+1,nmax),b_in_f1(nf+1,nmax),u_in_f1(nf+1,nmax))         !!!
  nflags1=6*(nv+2)                                                           !!!
  ALLOCATE(flag1(nflags1,3),neigh_flag1(nflags1,3),flag_color1(nflags1))     !!!
  ALLOCATE(flag2(nflags1,3),neigh_flag2(nflags1,3),nface2(nf+1))             !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Running add dimer procedure for the parent system                        !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Who's F0?                                                                !!!
  DO f0=fp,nf!iif,iif                                                        !!!
    IF(.not.uneq_face0(f0)) CYCLE                                            !!!
    ! Who are E1/E2 around F0?                                               !!!
    DO ie1=1,nface0(f0) !iie1,iie1     ! ie1 gives E1                        !!!
      IF(.not.u_in_f0(f0,ie1)) CYCLE                                         !!!
      e1=e_in_f0(f0,ie1) ! Who's E1?                                         !!!
      v1=v_in_f0(f0,ie1) ! Who's V1? -> V1 is E1's starting vertex           !!!
      j=ie1+1 ! (Ej is the edge after E1) (V1-V4 make E1) (we go V1->V4)     !!!
      IF(ie1.eq.nface0(f0)) j=1 ! Correct loop limit                         !!!
      v4=v_in_f0(f0,j)   ! Who's V4? -> V4 is Ej's starting vertex           !!!
      f1=f_in_f0(f0,ie1) ! Who's F1? -> Face touching E1                     !!!
      DO ie2=ie1+1,nface0(f0) ! ie2 gives E2                                 !!!
        e2=e_in_f0(f0,ie2)  ! Who's E2?                                      !!!
        IF((e1.eq.e2).AND.(.not.b_in_f0(f0,ie1))) CYCLE                      !!!
        v3=v_in_f0(f0,ie2)  ! Who's V3? -> V3 is E2's starting vertex        !!!
        j=ie2+1 ! (Ej is the edge after E2) (V3-V2 make E1) (we go V3->V2)   !!!
        IF(ie2.eq.nface0(f0)) j=1 ! Correct loop limit                       !!!
        v2=v_in_f0(f0,j)    ! Who's V2? -> V2 is Ej's starting vertex        !!!
        f2=f_in_f0(f0,ie2)  ! Who's F2? ->  Face touching E2                 !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Repetition                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        nface1(1:nf)=nface0            ! uneq_face1,f_in_f1,                 !!!
        e_in_f1(1:nf,1:nmax-3)=e_in_f0 ! b_in_f1, and u_in_f1 later          !!!
        v_in_f1(1:nf,1:nmax-3)=v_in_f0                                       !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Expansion                                                          !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        f3=nf+1                                                              !!!
        e3=ne+1                                                              !!!
        e4=ne+2                                                              !!!
        e0=ne+3                                                              !!!
        v5=nv+1                                                              !!!
        v6=nv+2                                                              !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Creation                                                           !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Getting new F3                                                     !!!
        CALL create_f3(nf+1,nface1(f3),nmax,e_in_f1,v_in_f1,f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6,ie1,ie2)
        ! Getting new F0                                                     !!!
        CALL create_f0(nf+1,nface1(f0),nmax,e_in_f1,v_in_f1,f0,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6,ie1,ie2)
        ! Getting new F1                                                     !!!
        IF(f1.ne.f0)THEN                                                     !!!
          CALL create_f12(nf+1,nface1(f1),nmax,e_in_f1,v_in_f1,f1,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6)
        END IF                                                               !!!
        ! Getting new F2                                                     !!!
        IF((f2.ne.f0).AND.(f2.ne.f1))THEN                                    !!!
          CALL create_f12(nf+1,nface1(f2),nmax,e_in_f1,v_in_f1,f2,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6)
        END IF                                                               !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! fcs to flg                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        CALL fcs_to_flg(nf+1,nmax,nface1,e_in_f1,v_in_f1,nflags1,flag1,neigh_flag1,flag_color1)    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! flg to map                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        CALL flg2map(nflags1,flag1,neigh_flag1,flag_color1,nf+1,nface1,nmaps,maps,maps_nfixed)  ! alocs inside
        ntmaps=0                                                             !!!
        DO i=1,nmaps                                                         !!!
          IF(maps_nfixed(i).ne.0) CYCLE                                      !!!
          IF(flag_color1(1)*flag_color1(maps(i,1)).eq.1) ntmaps=ntmaps+1     !!!
        END DO                                                               !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Comparing child with previous database                             !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Define new system's name                                           !!!
        CALL get_lname(nf+1,nface1,tmpfile)                                  !!!
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
            ! Get previous flag graph                                        !!!          
            CALL read_flg4kid(nf+1,nflags1,nface2,flag2,neigh_flag2,trialfile)!!
            ! Compare with trial flag graph                                  !!!
            CALL check_equiv_fgraphs(nflags1,nf+1,flag1,nface1,neigh_flag1, &
                                   & nflags1,nf+1,flag2,nface2,neigh_flag2,same)
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
          CALL write_flg_bin(nf+1,ne+3,nv+2,nflags1,nface1,flag1,neigh_flag1,flag_color1,tmpfile)
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .b.fcs                                                   !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Getting uneq_face1 and u_in_f1                                   !!!
          CALL neq_fe(nflags1,flag1,nf+1,ne+3,nface1,nmax,e_in_f1,uneq_face1,u_in_f1,nmaps,maps)
          CALL f_in_faces(nflags1,flag1,neigh_flag1,nf+1,nface1,f_in_f1,b_in_f1,nmax)
          ! Writting .b.fcs                                                  !!!
          CALL write_fcs_bin(nf+1,ne+3,nv+2,MAXVAL(nface1),nface1,uneq_face1 &!!
                       & ,f_in_f1,e_in_f1,v_in_f1,b_in_f1,u_in_f1,tmpfile)   !!!          
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .anc                                                     !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          CALL write_anc(f0,ie1,ie2,tmpfile,filename)                        !!!
        END IF                                                               !!!
        DEALLOCATE(maps,maps_nfixed)                                         !!!        
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
  DEALLOCATE(f_in_f0,e_in_f0,v_in_f0)                                        !!!
  DEALLOCATE(b_in_f0,u_in_f0,nface0,uneq_face0)                              !!!
  DEALLOCATE(nface1,uneq_face1)                                              !!!
  DEALLOCATE(f_in_f1,e_in_f1)                                                !!!
  DEALLOCATE(v_in_f1,b_in_f1,u_in_f1)                                        !!!
  DEALLOCATE(flag1,neigh_flag1,flag_color1)                                  !!!
  DEALLOCATE(flag2,neigh_flag2,nface2)                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fcs2kid                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
