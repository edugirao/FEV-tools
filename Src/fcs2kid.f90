!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fev_search                                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,l,nf,ne,nv,ie1,ie2                                             !!!
INTEGER:: f0,f1,f2,f3,e0,e1,e2,e3,e4,v1,v2,v3,v4,v5,v6                       !!!
INTEGER:: nmax,nmap,nfixed,ntmaps,nsys,nsys_p,isys                           !!!
INTEGER:: nflags1,sys_p,fp                                                   !!!
INTEGER,ALLOCATABLE:: flag1(:,:),neigh_flag1(:,:),flag_color1(:)             !!!
INTEGER,ALLOCATABLE:: flag2(:,:),neigh_flag2(:,:),nface2(:)                  !!!
INTEGER,ALLOCATABLE:: map(:)                                                 !!!
INTEGER,ALLOCATABLE:: nface0(:)                                              !!!
INTEGER,ALLOCATABLE:: f_in_f0(:,:),e_in_f0(:,:),v_in_f0(:,:)                 !!!
LOGICAL,ALLOCATABLE:: b_in_f0(:,:),u_in_f0(:,:),uneq_face0(:)                !!!
INTEGER,ALLOCATABLE:: nface1(:)                                              !!!
INTEGER,ALLOCATABLE:: f_in_f1(:,:),e_in_f1(:,:),v_in_f1(:,:)                 !!!
LOGICAL,ALLOCATABLE:: b_in_f1(:,:),u_in_f1(:,:),uneq_face1(:)                !!!
LOGICAL:: file_exist,same                                                    !!!
CHARACTER*100:: filename,tmpfile,gensize,gensize_p,prevfile,aux              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying parent generation                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Structures' size                                                           !!!
OPEN(UNIT=1,FILE='igen')                                                     !!!
READ(1,*) nf                                                                 !!!
CLOSE(UNIT=1)                                                                !!!
IF(nf.eq.0)THEN                                                              !!!
  CALL make_gen1                                                             !!!
  STOP                                                                       !!!
END IF                                                                       !!!
! Generation size                                                            !!!
WRITE(gensize_p,'(I0)') nf                                                   !!!
OPEN(UNIT=1,FILE='gensize'//TRIM(ADJUSTL(gensize_p)))                        !!!
READ(1,*) nsys_p                                                             !!!
CLOSE(UNIT=1)                                                                !!!
! Starting next generation size                                              !!!
WRITE(gensize,'(I0)') nf+1                                                   !!!
OPEN(UNIT=1,FILE='gensize'//TRIM(ADJUSTL(gensize)))                          !!!
WRITE(1,*) 0                                                                 !!!
CLOSE(UNIT=1)                                                                !!!
nsys=0                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Running over all the structures of the parent database                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading parent structures filenames from gensystems file                   !!!
OPEN(UNIT=43,FILE='gensystems'//TRIM(ADJUSTL(gensize_p)))                    !!!
DO isys=1,nsys_p                                                             !!!
  ! Evolution bar                                                            !!!
  WRITE(*,'(I0,A,I0)') isys,'/',nsys_p                                       !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Getting parent system filelabel                                          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Getting parent structure's filename, ancestor, & ancestor's dimer added face
  READ(43,*) filename,sys_p,fp                                               !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reading .fcs info for the parent system                                  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')     !!!
  READ(3) nf,ne,nv,nmax                                                      !!!
  ALLOCATE(f_in_f0(nf,nmax),e_in_f0(nf,nmax),v_in_f0(nf,nmax))               !!!
  ALLOCATE(b_in_f0(nf,nmax),u_in_f0(nf,nmax),nface0(nf),uneq_face0(nf))      !!!
  DO i=1,nf                                                                  !!!
    READ(3) nface0(i),uneq_face0(i)                                          !!!
    READ(3) f_in_f0(i,1:nface0(i))                                           !!!
    READ(3) e_in_f0(i,1:nface0(i))                                           !!!
    READ(3) v_in_f0(i,1:nface0(i))                                           !!!
    READ(3) b_in_f0(i,1:nface0(i))                                           !!!
    READ(3) u_in_f0(i,1:nface0(i))                                           !!!
  END DO                                                                     !!!
  CLOSE(UNIT=3)                                                              !!!
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
  ALLOCATE(map(nflags1))                                                     !!!
  ALLOCATE(flag2(nflags1,3),neigh_flag2(nflags1,3),nface2(nf+1))             !!!
!   ALLOCATE(fev2(nf+1,ne+3,nv+2))                                           !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Running add dimer procedure for the parent system                        !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! Who's F0?                                                                !!!
  DO f0=fp,nf!iif,iif                                                        !!!
    IF(.not.uneq_face0(f0)) CYCLE                                            !!!
    ! Who are E1/E2 around F0?                                               !!!
    DO ie1=1,nface0(f0) !iie1,iie1     ! ie1 gives E1                        !!!
      IF(.not.u_in_f0(f0,ie1)) CYCLE                                         !!!
      ! Who's E1?                                                            !!!
      e1=e_in_f0(f0,ie1)                                                     !!!
      ! Who's V1?                                                            !!!
      v1=v_in_f0(f0,ie1) ! V1 is E1's starting vertex                        !!!
      ! Who's V4? (V1-V4 make E1) (we go V1->V4)                             !!!
      j=ie1+1 ! Ej is the edge after E1.                                     !!!
      IF(ie1.eq.nface0(f0)) j=1 ! Correct loop limit                         !!!
      v4=v_in_f0(f0,j)   ! V4 is Ej's starting vertex                        !!!
      ! Identifying F1                                                       !!!
      f1=f_in_f0(f0,ie1)      ! Face touching E1                             !!!
      DO ie2=ie1+1,nface0(f0)!iie2,iie2 ! ie2 gives E2                       !!!
        ! Who's E2?                                                          !!!
        e2=e_in_f0(f0,ie2)                                                   !!!
        IF((e1.eq.e2).AND.(.not.b_in_f0(f0,ie1))) CYCLE                      !!!
        ! Who's V3?                                                          !!!
        v3=v_in_f0(f0,ie2) ! V3 is E2's starting vertex                      !!!
        ! Who's V2? (V3-V2 make E1) (we go V3->V2)                           !!!
        j=ie2+1 ! Ej is the edge after E2?                                   !!!
        IF(ie2.eq.nface0(f0)) j=1 ! Correct loop limit                       !!!
        v2=v_in_f0(f0,j)   ! V2 is Ej's starting vertex                      !!!
        ! Identifying F2                                                     !!!
        f2=f_in_f0(f0,ie2)     ! Face touching E2                            !!!
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
        CALL fcs2flg(nf+1,nmax,nface1,e_in_f1,v_in_f1,nflags1,flag1,neigh_flag1,flag_color1)    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! flg to map                                                         !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        tmpfile='kid'                                                        !!!
        CALL flg2map(nflags1,flag1,neigh_flag1,flag_color1,nf+1,nface1,tmpfile)
        OPEN(UNIT=1,FILE='kid.map')                                          !!!
        READ(1,*) nmap                                                       !!!
        ntmaps=0                                                             !!!
        DO i=1,nmap                                                          !!!
          READ(1,*) nfixed                                                   !!!
          IF(nfixed.eq.0)THEN                                                !!!
            READ(1,*) map                                                    !!!
            IF(flag_color1(1)*flag_color1(map(1)).eq.1) ntmaps=ntmaps+1      !!!
          ELSE                                                               !!!
            READ(1,*)                                                        !!!
          END IF                                                             !!!
        END DO                                                               !!!
        CLOSE(UNIT=1)                                                        !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Comparing child with previous database                             !!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                             !!!
        ! Define new system's name                                           !!!
        CALL get_lname(nf+1,nface1,tmpfile)                                  !!!
        same=.false.                                                         !!!
        l=0                                                                  !!!
        DO                                                                   !!!
          l=l+1                                                              !!!
          ! If name has been taken before, add sufix                         !!!
          IF(l.gt.1)THEN                                                     !!!
            WRITE(aux,'(I0)') l                                              !!!
            INQUIRE(FILE=TRIM(ADJUSTL(tmpfile))//'_'//TRIM(ADJUSTL(aux))//'.b.flg',EXIST=file_exist)
          ELSE                                                               !!!
            INQUIRE(FILE=TRIM(ADJUSTL(tmpfile))//'.b.flg',EXIST=file_exist)  !!!
          END IF                                                             !!!
          ! If file exist, compare                                           !!!
          IF(file_exist)THEN                                                 !!!
            IF(l.gt.1)THEN                                                   !!!
              OPEN(UNIT=2,FILE=TRIM(ADJUSTL(tmpfile))//'_'//TRIM(ADJUSTL(aux))//'.b.flg',FORM='UNFORMATTED')
            ELSE
              OPEN(UNIT=2,FILE=TRIM(ADJUSTL(tmpfile))//'.b.flg',FORM='UNFORMATTED')
            END IF                                                           !!!
            ! Get previous flag graph                                        !!!
            READ(2)                                                          !!!
            READ(2) nface2                                                   !!!
            READ(2) flag2                                                    !!!
            READ(2) neigh_flag2                                              !!!
            CLOSE(UNIT=2)                                                    !!!
            ! Compare with trial flag graph                                  !!!
            CALL check_equiv_fgraphs(nflags1,nf+1,flag1,nface1,neigh_flag1, &
                                            & flag2,nface2,neigh_flag2,same)
            IF(same) EXIT                                                    !!!
          ELSE                                                               !!!
            EXIT                                                             !!!
          END IF                                                             !!!
        END DO                                                               !!!
        ! New system?                                                        !!!
        IF(.not.same)THEN                                                    !!!
          ! Define new system's name                                         !!!
          CALL get_lname(nf+1,nface1,tmpfile)                                !!!
          ! Check if a previous system has the same name (and how many)      !!!
          l=LEN_TRIM(tmpfile)                                                !!!
          OPEN(UNIT=1,FILE='gensystems'//TRIM(ADJUSTL(gensize)))             !!!
          j=1                                                                !!!
          DO i=1,nsys                                                        !!!
            READ(1,*) prevfile                                               !!!
            IF(TRIM(ADJUSTL(prevfile(1:l))).eq.TRIM(ADJUSTL(tmpfile)))THEN   !!!
              j=j+1                                                          !!!
            ENDIF                                                            !!!
          END DO                                                             !!!
          CLOSE(UNIT=1)                                                      !!!
          ! If name has been taken before, add sufix                         !!!
          IF(j.gt.1)THEN                                                     !!!
            WRITE(aux,'(I0)') j                                              !!!
            tmpfile=TRIM(ADJUSTL(tmpfile))//'_'//TRIM(ADJUSTL(aux))          !!!
          END IF                                                             !!!
          ! Updating database size                                           !!!
          nsys=nsys+1                                                        !!!
          OPEN(UNIT=1,FILE='gensize'//TRIM(ADJUSTL(gensize)))                !!!
          WRITE(1,*) nsys                                                    !!!
          CLOSE(UNIT=1)                                                      !!!
          ! Updating database adding the new system                          !!!
          OPEN(UNIT=1,FILE='gensystems'//TRIM(ADJUSTL(gensize)),POSITION='APPEND')
          WRITE(1,*) tmpfile,nsys,f0,ntmaps                                  !!!
          CLOSE(UNIT=1)                                                      !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .flg                                                     !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          OPEN(UNIT=3,FILE=TRIM(ADJUSTL(tmpfile))//'.b.flg',FORM='UNFORMATTED')
          WRITE(3) nflags1,nf+1,ne+3,nv+2                                    !!!
          WRITE(3) nface1                                                    !!!
          WRITE(3) flag1                                                     !!!
          WRITE(3) neigh_flag1                                               !!!
          WRITE(3) flag_color1                                               !!!
          CLOSE(UNIT=3)                                                      !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .fcs                                                     !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Getting uneq_face1 and u_in_f1                                   !!!
          aux='kid'                                                          !!!
          CALL neq_fe(nflags1,flag1,nf+1,ne+3,nface1,nmax,e_in_f1,uneq_face1,u_in_f1,aux)
          CALL f_in_faces(nflags1,flag1,neigh_flag1,nf+1,nface1,f_in_f1,b_in_f1,nmax)
          OPEN(UNIT=3,FILE=TRIM(ADJUSTL(tmpfile))//'.b.fcs',FORM='UNFORMATTED')
          WRITE(3) nf+1,ne+3,nv+2,MAXVAL(nface1)                             !!!
          DO i=1,nf+1                                                        !!!
            WRITE(3) nface1(i),uneq_face1(i)                                 !!!
            WRITE(3) f_in_f1(i,1:nface1(i))                                  !!!
            WRITE(3) e_in_f1(i,1:nface1(i))                                  !!!
            WRITE(3) v_in_f1(i,1:nface1(i))                                  !!!
            WRITE(3) b_in_f1(i,1:nface1(i))                                  !!!
            WRITE(3) u_in_f1(i,1:nface1(i))                                  !!!
          END DO                                                             !!!
          CLOSE(UNIT=3)                                                      !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Writing .flg                                                     !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          OPEN(UNIT=3,FILE=TRIM(ADJUSTL(tmpfile))//'.anc')                   !!!
          WRITE(3,*) TRIM(ADJUSTL(filename))                                 !!!
          WRITE(3,*) f0,ie1,ie2                                              !!!
          CLOSE(UNIT=3)                                                      !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          ! Renaming .map file                                               !!!
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                           !!!
          CALL RENAME('kid.map',TRIM(ADJUSTL(tmpfile))//'.map')              !!!
        END IF                                                               !!!
      END DO                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
  DEALLOCATE(f_in_f0,e_in_f0,v_in_f0)                                        !!!
  DEALLOCATE(b_in_f0,u_in_f0,nface0,uneq_face0)                              !!!
  DEALLOCATE(nface1,uneq_face1)                                              !!!
  DEALLOCATE(f_in_f1,e_in_f1)                                                !!!
  DEALLOCATE(v_in_f1,b_in_f1,u_in_f1)                                        !!!
  DEALLOCATE(flag1,neigh_flag1,flag_color1)                                  !!!
  DEALLOCATE(map)                                                            !!!
  DEALLOCATE(flag2,neigh_flag2,nface2)                                       !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fev_search                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
