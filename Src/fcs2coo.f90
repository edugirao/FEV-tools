!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fcs2coo                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool to create the generation of structures with N+1 faces from the gen    !!!
! of structures with N faces. Uses the .b.fcs format as basic instrument.    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_conv                                                               !!!
USE tools_maps                                                               !!!
USE tools_writ                                                               !!!
USE tools_adim                                                               !!!
USE tools_ddim                                                               !!!
USE tools_coor                                                               !!!
USE tools_relx                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,j,ii,u,nf,nf0,ne0,nv0,nf1,ne1,nv1,nmaps,isys,f0                  !!!
INTEGER:: nflags1,nflags0,nmax0,nmax1,ie1,ie2,inv                            !!!
INTEGER,ALLOCATABLE:: maps(:,:),maps_nfixed(:),v2v(:),f2f(:),nface2(:)       !!!
INTEGER,ALLOCATABLE:: flag0(:,:),neigh_flag0(:,:),flag_color0(:)             !!!
INTEGER,ALLOCATABLE:: flag1(:,:),neigh_flag1(:,:),flag_color1(:)             !!!
INTEGER,ALLOCATABLE:: nface0(:),f_in_f0(:,:),e_in_f0(:,:),v_in_f0(:,:)       !!!
LOGICAL,ALLOCATABLE:: b_in_f0(:,:),u_in_f0(:,:),uneq_face0(:)                !!!
LOGICAL,ALLOCATABLE:: b_in_f1(:,:),u_in_f1(:,:),uneq_face1(:)                !!!
INTEGER,ALLOCATABLE:: nface1(:),f_in_f1(:,:),e_in_f1(:,:),v_in_f1(:,:)       !!!
INTEGER,ALLOCATABLE:: x_in_f0(:,:),y_in_f0(:,:)                              !!!
INTEGER,ALLOCATABLE:: x_in_f1(:,:),y_in_f1(:,:)                              !!!
INTEGER,ALLOCATABLE:: x_in_f2(:,:),y_in_f2(:,:)                              !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*100,ALLOCATABLE:: fnames_a(:),fnames_b(:)                          !!!
REAL(KIND=8),ALLOCATABLE:: rp(:,:),rk(:,:),rk2(:,:)                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file for the target system                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
CALL read_fcs_only_nf(nf,filename)                                           !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Preparing filenames                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ALLOCATE(fnames_a(nf),fnames_b(nf))                                          !!!
DO i=1,nf                                                                    !!!
  ! From the nf-th to the 1st (graphene) structure, they will be called...   !!!
  !...structure-iib (unordered version) & structure-iia (reordered version). !!!
  WRITE(fnames_a(i),'(I0)') i                                                !!!
  IF(i.lt.10) fnames_a(i)='0'//TRIM(ADJUSTL(fnames_a(i)))                    !!!
  fnames_b(i)='structure-'//TRIM(ADJUSTL(fnames_a(i)))//'b'                  !!!
  fnames_a(i)='structure-'//TRIM(ADJUSTL(fnames_a(i)))//'a'                  !!!
END DO                                                                       !!!
! Target system is the (nf)th one in b version                               !!!
fnames_b(nf)=filename                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading .fcs info for the target system (allocations inside read_fcs)      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fcs_only_fev_in_f(nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1,filename)
! CALL print_ev_in_face(nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1)            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Building .fcs info files for the ancestors down the genealogic tree        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO isys=nf,2,-1                                                              !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Reordering child system to put elements to be extracted at the end       !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(v2v(nv1),f2f(nf1))                                                !!!
  CALL reordering_fcs(nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1,f2f,v2v,ie2,inv)
  OPEN(NEWUNIT=i,FILE=TRIM(ADJUSTL(fnames_a(isys)))//'.v2v')                 !!!
  WRITE(i,*) f2f                                                             !!!
  WRITE(i,*) v2v                                                             !!!
  WRITE(i,*) ie2,inv                                                         !!!
  CLOSE(UNIT=i)                                                              !!!
  DEALLOCATE(v2v,f2f)                                                        !!!
!   CALL print_ev_in_face(nf1,ne1,nv1,nmax1,nface1,e_in_f1,v_in_f1)          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Rest of reordered parent fcs info and writing fcs file                   !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! fcs to flg for the parent system                                         !!!
  CALL fcs_to_flg(nf1,nmax1,nface1,e_in_f1,v_in_f1,nflags1,flag1,neigh_flag1,flag_color1)    
  ! flg to map for the parent system ( alocs inside)                         !!!
  CALL flg2map(nflags1,flag1,neigh_flag1,flag_color1,nf1,nface1,nmaps,maps,maps_nfixed)
  ! Getting uneq_face1 and u_in_f1                                           !!!
  ALLOCATE(uneq_face1(nf1),b_in_f1(nf1,nmax1),u_in_f1(nf1,nmax1))            !!!
  CALL neq_fe(nflags1,flag1,nf1,ne1,nface1,nmax1,e_in_f1,uneq_face1,u_in_f1,nmaps,maps)
  ! Writing .b.fcs for the parent system                                     !!!
  CALL write_fcs_bin(nf1,ne1,nv1,nmax1,nface1,uneq_face1 &                   !!!
               & ,f_in_f1,e_in_f1,v_in_f1,b_in_f1,u_in_f1,fnames_a(isys))    !!!  
  ! Deallocations                                                            !!!
  DEALLOCATE(maps,maps_nfixed,uneq_face1,b_in_f1,u_in_f1)                    !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Creating the parent system (isys-1) from the child (isys) (allocs inside)!!!
  ! nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1-> child system  (isys)  !!!
  ! nf0,ne0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0-> parent system (isys-1)!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL make_prev(nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1,&          !!!
               & nf0,ne0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0,f0,ie1,ie2)!!!    
!   CALL print_ev_in_face(nf0,ne0,nv0,nmax0,nface0,e_in_f0,v_in_f0)          !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Rest of parent fcs info and writing (non-reordered) fcs file             !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  ! fcs to flg for the parent system                                         !!!
  CALL fcs_to_flg(nf0,nmax0,nface0,e_in_f0,v_in_f0,nflags0,flag0,neigh_flag0,flag_color0)    
  ! flg to map for the parent system ( alocs inside)                         !!!
  CALL flg2map(nflags0,flag0,neigh_flag0,flag_color0,nf0,nface0,nmaps,maps,maps_nfixed)
  ! Getting uneq_face0 and u_in_f0                                           !!!
  ALLOCATE(uneq_face0(nf0),b_in_f0(nf0,nmax0),u_in_f0(nf0,nmax0))            !!!
  CALL neq_fe(nflags0,flag0,nf0,ne0,nface0,nmax0,e_in_f0,uneq_face0,u_in_f0,nmaps,maps)
  ! Getting f_in_f0                                                          !!!
  CALL f_in_faces(nflags0,flag0,neigh_flag0,nf0,nface0,f_in_f0,b_in_f0,nmax0)!!!
  ! Writing (non-reordered) .b.fcs for the parent system                     !!!
  CALL write_fcs_bin(nf0,ne0,nv0,nmax0,nface0,uneq_face0 &                   !!!
               & ,f_in_f0,e_in_f0,v_in_f0,b_in_f0,u_in_f0,fnames_b(isys-1))  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Swapping to turn this parent into the child for the next step            !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Swapping sizes                                                           !!!
  nf1=nf0                                                                    !!!
  ne1=ne0                                                                    !!!
  nv1=nv0                                                                    !!!
  nmax1=nmax0                                                                !!!
  ! Swapping arrays                                                          !!!
  DEALLOCATE(nface1,f_in_f1,e_in_f1,v_in_f1)                                 !!!
  ALLOCATE(nface1(nf1),f_in_f1(nf1,nmax1),e_in_f1(nf1,nmax1),v_in_f1(nf1,nmax1))
  nface1=nface0                                                              !!!
  f_in_f1=f_in_f0(:,1:nmax1)                                                 !!!
  e_in_f1=e_in_f0(:,1:nmax1)                                                 !!!
  v_in_f1=v_in_f0(:,1:nmax1)                                                 !!!
  ! Deallocating data                                                        !!!
  DEALLOCATE(nface0,f_in_f0,e_in_f0,v_in_f0,uneq_face0,b_in_f0,u_in_f0)      !!!
  DEALLOCATE(maps,maps_nfixed)                                               !!!
  ! Writing .anc file                                                        !!!
  CALL write_anc(f0,ie1,ie2,fnames_a(isys),fnames_b(isys-1))                 !!!
END DO                                                                       !!!
! Creating graphene (structure-01)                                           !!!
CALL write_graphene(fnames_b(1),0,0,0,1,1,0)                                 !!!
CALL make_graphene(i,fnames_b(1))                                            !!!  
! Creating others                                                            !!!  
DO i=2,nf                                                                    !!!
  print*, 'Making ',fnames_a(i)                                              !!!
  ! Getting ancestor info                                                    !!!
  CALL read_anc(fnames_a(i),fnames_b(i-1),f0,ie1,ie2)                        !!!
  ! Reading parent .fcs (unordered - allocs inside                           !!!
  CALL read_fcs_only_fev_in_f(nf0,ne0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0,fnames_b(i-1))
  ! Reading child .fcs (reordered - allocs inside)                           !!!
  CALL read_fcs_only_fev_in_f(nf1,ne1,nv1,nmax1,nface1,f_in_f1,e_in_f1,v_in_f1,fnames_a(i))
  ! Reading parent .nxy (unordered - allocations inside)                     !!!
  CALL read_nxy(nf0,nmax0,nface0,x_in_f0,y_in_f0,fnames_b(i-1))              !!!
  ! Reading parent structure (unordered)                                     !!!
  CALL read_xydata(nv0,rp,fnames_b(i-1))                                     !!!  
  ! Optimizing the xyz                                                       !!!
  CALL add_a_dimer(f0,ie1,ie2,rp, &                                          !!!
          & nf0,nv0,nmax0,nface0,f_in_f0,e_in_f0,v_in_f0,x_in_f0,y_in_f0, &  !!!
          & nf1,nv1,nmax1,nface1,x_in_f1,y_in_f1,rk)                         !!!  
  ! Optimizing child structure (reordered)                                   !!!
  CALL xyz_optimization(nf1,nv1,nmax1,nface1,v_in_f1,x_in_f1,y_in_f1,rk,fnames_a(i))  
  !  Writing the xyz (reordered)                                             !!!
  CALL write_xyz(nv1,rk,fnames_a(i))                                         !!!
  !  Writing the nxy (reordered)                                             !!!
  CALL write_nxy(nf1,MAXVAL(nface1),nface1,x_in_f1,y_in_f1,fnames_a(i))      !!!
  ! Readig v to v map                                                        !!!
  ALLOCATE(v2v(nv1),f2f(nf1))                                                !!!
  OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(fnames_a(i)))//'.v2v')                    !!!
  READ(u,*) f2f                                                              !!!
  READ(u,*) v2v                                                              !!!
  READ(u,*) ie2,inv                                                          !!!
  CLOSE(UNIT=u)                                                              !!!
  ! Unordering vertices                                                      !!!
  ALLOCATE(rk2(nv1,2))                                                       !!!
  DO j=1,nv1                                                                 !!!
    rk2(j,:)=rk(v2v(j),:)                                                    !!!
  END DO                                                                     !!!
  rk=rk2                                                                     !!!
  DEALLOCATE(rk2,v2v)                                                        !!!
  !  Writing the xyz (unordered)                                             !!!
  CALL write_xyz(nv1,rk,fnames_b(i))                                         !!!    
  ! Unmaking F0 shift                                                        !!!
  ALLOCATE(x_in_f2(1,nmax1),y_in_f2(1,nmax1))                                !!!
  j=nface1(f0)-ie2                                                           !!! 
  x_in_f2=0 ; y_in_f2=0                                                      !!!
  DO ii=1,nface1(f0)                                                         !!!
    j=j+1                                                                    !!!
    IF(j.gt.nface1(f0)) j=j-nface1(f0)                                       !!!
    x_in_f2(1,ii)=x_in_f1(f0,j)                                              !!!
    y_in_f2(1,ii)=y_in_f1(f0,j)                                              !!!
  END DO                                                                     !!!
  x_in_f1(f0,:)=x_in_f2(1,:)                                                 !!!
  y_in_f1(f0,:)=y_in_f2(1,:)                                                 !!!
  DEALLOCATE(x_in_f2,y_in_f2)                                                !!!
  ! Unmaking F0 reverse                                                      !!!
  IF(inv.eq.-1)THEN                                                          !!!
    CALL reverse_f0xy(f0,nf1,nface1,nmax1,x_in_f1,y_in_f1)                   !!!  
  END IF                                                                     !!!
  ! Unordering faces in x_in_f/y_in_f                                        !!!
  ALLOCATE(x_in_f2(nf1,nmax1),y_in_f2(nf1,nmax1),nface2(nf1))                !!!
  x_in_f2=0 ; y_in_f2=0                                                      !!!
  DO ii=1,nf1                                                                !!!
    nface2(ii)=nface1(f2f(ii))                                               !!!
    x_in_f2(ii,1:nface2(ii))=x_in_f1(f2f(ii),1:nface2(ii))                   !!!
    y_in_f2(ii,1:nface2(ii))=y_in_f1(f2f(ii),1:nface2(ii))                   !!!
  END DO                                                                     !!!
  ! Passing the new order                                                    !!!
  nface1=nface2                                                              !!!
  x_in_f1=x_in_f2                                                            !!!
  y_in_f1=y_in_f2                                                            !!!
  DEALLOCATE(x_in_f2,y_in_f2,f2f,nface2)                                     !!!
  !  Writing the nxy (unordered)                                             !!!
  CALL write_nxy(nf1,MAXVAL(nface1),nface1,x_in_f1,y_in_f1,fnames_b(i))      !!!  
  DEALLOCATE(rp,f_in_f0,e_in_f0,v_in_f0,nface0,x_in_f0,y_in_f0)              !!!
  DEALLOCATE(rk,f_in_f1,e_in_f1,v_in_f1,nface1,x_in_f1,y_in_f1)              !!!
END DO                                                                       !!!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fcs2coo                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








SUBROUTINE print_ev_in_face(nf,ne,nv,nmax,nface,e_in_f,v_in_f)
IMPLICIT NONE
INTEGER:: nf,ne,nv,nmax,nface(nf),e_in_f(nf,nmax),v_in_f(nf,nmax),i
print*,'--------------------------------'
print*,nf,ne,nv,nmax
print*,nface
DO i=1,nf
  print*,e_in_f(i,1:nface(i))
  print*,v_in_f(i,1:nface(i))
END DO
END SUBROUTINE print_ev_in_face
