!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM flg2xyz                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for Faces-Info to Fractional-Atomic-Coordinates conversion.           !!!
! Converts a .flg or a .b.flg file to .xyz and .pdf formats.                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_relx                                                               !!!
USE tools_read                                                               !!!
USE tools_writ                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: i,nf,i0,f0,nf0,ne0,nv0,nf1,ne1,nv1,nflags0,nflags1             !!!
INTEGER,ALLOCATABLE:: nface0(:),flag0(:,:),A(:,:) !!!
INTEGER,ALLOCATABLE:: nface1(:),flag1(:,:) !!!
INTEGER,ALLOCATABLE:: cflag0(:,:),cflag1(:,:)                              !!!
INTEGER:: flag_a,flag_b,flag_c,v1,v2,v3,v4,vi,vj
INTEGER:: flag_1a,flag_1b,flag_2a,flag_2b,flag_3a,flag_3b,flag_4a,flag_4b
CHARACTER*100:: filename_target,filename_p                                   !!!
CHARACTER*100,ALLOCATABLE:: filename_list(:)                                 !!!
REAL(KIND=8):: a1(2),a2(2),dr(2),r1(2),r2(2),r3(2),r4(2),r5(2),r6(2),ra(2)                                                   !!!
REAL(KIND=8),ALLOCATABLE:: rp(:,:),rk(:,:)                                   !!!
CHARACTER*4:: group
LOGICAL:: have_file,leave                                                          !!!
a1(1)=1.0D0
a1(2)=0.0D0
a2(2)=0.0D0
a2(2)=1.0D0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting target information                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Getting target filename                                                    !!!
CALL read_init(filename_target,'inp')                                        !!!
! Early exit if xyz already exist                                            !!!
INQUIRE(FILE=TRIM(ADJUSTL(filename_target))//'.xyz',EXIST=have_file)         !!!
IF(have_file) STOP TRIM(ADJUSTL(filename_target))//'.xyz already exists.'    !!!
! Getting target's size                                                      !!!
CALL read_flg_only_nf(nf,filename_target)                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Ancestor list                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_anc_list(nf,filename_target,filename_list,i0)                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Taking care of graphene if the first in the ancestor list                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF(i0.eq.1)THEN                                                              !!!
  CALL write_graphene(filename_list(1),0,0,1,1,0)                          !!!
  CALL make_graphene(i0,filename_list(1))                                    !!!
END IF                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating strucuture(s) from ancestor list                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=i0,nf                                                                   !!!
  print*, 'Making ',filename_list(i)                                         !!!
  ! Reading child .flg (allocations in read_flg)               !!!
  CALL read_flg(nf1,ne1,nv1,nflags1,nface1,flag1,filename_list(i))  
  ! Getting ancestor info                                                    !!!
  CALL read_anc(filename_list(i),filename_p,f0,flag_a,flag_b)                      !!!
  ! Reading parent .flg (allocations in read_flg)              !!!
  CALL read_flg(nf0,ne0,nv0,nflags0,nface0,flag0,filename_p)

  ! Reading parent .cfl (allocations inside)                                 !!!
  CALL read_cfl(nflags0,cflag0,filename_p)

  ! Reading parent structure                                                 !!!
  CALL read_xydata(nv0,rp,filename_p)                                        !!!  
  
  
  ! Involved flags
  flag_1a=flag_a
  flag_1b=flag0(flag_a,-1)
  flag_4a=flag0(flag_a,-3)
  flag_4b=flag0(flag_4a,-1)
  flag_3a=flag_b
  flag_3b=flag0(flag_b,-1)
  flag_2a=flag0(flag_b,-3)
  flag_2b=flag0(flag_2a,-1)
  
  ! Involved vertices V1/V4-V3/V2
  v1=flag0(flag_a,3)
  v4=flag0(flag0(flag_a,-3),3)
  v3=flag0(flag_b,3)
  v2=flag0(flag0(flag_b,-3),3)  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Identifying real positions around the cycle
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Position for V1
  r1=rp(v1,:)
  ! Position for V4
  r4=rp(v4,:)+cflag0(flag_a,1)*a1+cflag0(flag_a,2)*a2
  ! Moving from V4 to V2
  flag_c=flag_a ! starting at E1/V1 
  flag_c=flag0(flag_c,-3) ! Moving to Ei/V4 ... 
  flag_c=flag0(flag_c,-2) ! ... with Ei after E1
  ra=r4
  leave=.false.
  DO
    ! Check if we reached E2
    IF(flag_c.eq.flag_b)THEN
      ! Position for V3
      r3=ra
      leave=.true.
    END IF  
    ! Vi vertex
    vi=flag0(flag_c,3)
    ! Displacement vector (part 1)
    dr=cflag0(flag_c,1)*a1+cflag0(flag_c,2)*a2-rp(vi,:)    
    ! Move one edge forward 
    flag_c=flag0(flag_c,-3) ! v-swap
    flag_c=flag0(flag_c,-2) ! e-swap
    ! Vj vertex
    vj=flag0(flag_c,3)
    ! Displacement vector  (part 2)
    dr=rp(vj,:)+dr
    ! Adding dr to ra
    ra=ra+dr
    ! Position for V2 if it is the case
    IF(leave)THEN
      r2=ra
      EXIT
    END IF
  END DO  
  ! Position for V5
  r5=(r1+r4)/2.0D0
  ! Position for V6
  r6=(r2+r3)/2.0D0

  
  
  ! Copying the parent part     !!!  
  ALLOCATE(rk(nv1,2),cflag1(nflags1,2))
  rk(1:nv0,:)=rp
  cflag1(1:nflags0,:)=cflag0
  ! Correcting V1 neighbor (V4 out, V5 in)
  dr=r5-r1 ! Displacement vector
  cflag1(flag_1a,:)=FLOOR(rp(v1,:)+dr(:))
  cflag1(flag_1b,:)=cflag1(flag_1a,:)
  ! V1 as one of the V5 neighbors
  cflag1(nflags0+1,:)=-cflag1(flag_1a,:)
  cflag1(nflags0+6,:)=-cflag1(flag_1a,:)
  ! Correcting V4 neighbor (V1 out, V5 in)
  dr=r5-r4 ! Displacement vector
  cflag1(flag_4a,:)=FLOOR(rp(v4,:)+dr(:))
  cflag1(flag_4b,:)=cflag1(flag_4a,:)
  ! V4 as one of the V5 neighbors
  cflag1(nflags0+4,:)=-cflag1(flag_4a,:)
  cflag1(nflags0+5,:)=-cflag1(flag_4a,:)
  ! Correcting V2 neighbor (V3 out, V6 in)
  dr=r6-r2 ! Displacement vector  
  cflag1(flag_2a,:)=FLOOR(rp(v2,:)+dr(:))
  cflag1(flag_2b,:)=cflag1(flag_2a,:)
  ! V2 as one of the V6 neighbors
  cflag1(nflags0+7,:)=-cflag1(flag_2a,:)
  cflag1(nflags0+12,:)=-cflag1(flag_2a,:)
  ! Correcting V3 neighbor (V2 out, V6 in)
  dr=r6-r3 ! Displacement vector
  cflag1(flag_3a,:)=FLOOR(rp(v3,:)+dr(:))
  cflag1(flag_3b,:)=cflag1(flag_3a,:)
  ! V3 as one of the V6 neighbors
  cflag1(nflags0+10,:)=-cflag1(flag_3a,:)
  cflag1(nflags0+11,:)=-cflag1(flag_3a,:)
  ! V5-V6 as neighbors of each other
  dr=r6-r5 ! Displacement vector
  r5(1)=r5(1)-FLOOR(r5(1)) ! Shifting...
  r5(2)=r5(2)-FLOOR(r5(2)) ! ...into...
  r6(1)=r6(1)-FLOOR(r6(1)) ! ...the...
  r6(2)=r6(2)-FLOOR(r6(2)) ! ...unit cell
  cflag1(nflags0+2,:)=FLOOR(r5+dr)
  cflag1(nflags0+3,:)=cflag1(nflags0+2,:)
  cflag1(nflags0+8,:)=-cflag1(nflags0+2,:)
  cflag1(nflags0+9,:)=-cflag1(nflags0+2,:)
  ! Creating new sites
  rk(nv0+1,:)=r5
  rk(nv0+2,:)=r6    
  
  


  
  
  
  
  ! Optimizing child structure                                               !!!
  CALL xyz_relaxation(nv1,nflags1,flag1,cflag1,rk,filename_list(i))  
  !  Writing the xyz                                                         !!!
  CALL write_xyz(nv1,rk,filename_list(i))                                    !!!
  !  Writing cfl                                                         !!!
  CALL write_cfl(nflags1,cflag1,filename_list(i))
  
  ! Writing Vincent format
  OPEN(NEWUNIT=v1,FILE=TRIM(ADJUSTL(filename_list(i)))//'.sym')
  READ(v1,*,IOSTAT=v2) group
  CLOSE(UNIT=v1)  
IF(v2.eq.0)THEN  
  OPEN(NEWUNIT=v1,FILE=TRIM(ADJUSTL(filename_list(i)))//'.txt')
  ALLOCATE(A(nv1,nv1))
  A=0
  DO vi=1,nflags1
    A(flag1(vi,3),flag1(flag1(vi,-3),3))=1
    A(flag1(flag1(vi,-3),3),flag1(vi,3))=1
  END DO
  DO vi=1,nv1
    DO vj=1,nv1
      IF(A(vi,vj).eq.1) WRITE(v1,'(I0,1X,I0)') vi-1,vj-1
    END DO
  END DO
  WRITE(v1,*)
  DO vi=1,nv1
    WRITE(v1,'(A,1X,F9.7,1X,F9.7,1X,F9.7)') 'C',rk(vi,:),0.0
  END DO
  WRITE(v1,*)   
  WRITE(v1,'(F9.7,1X,F9.7,1X,F9.7,1X,A)') 1.0D0,0.0D0,0.0D0,'True'
  WRITE(v1,'(F9.7,1X,F9.7,1X,F9.7,1X,A)') 0.0D0,1.0D0,0.0D0,'True'
  WRITE(v1,'(F9.7,1X,F9.7,1X,F9.7,1X,A)') 0.0D0,0.0D0,1.0D0,'False'
  WRITE(v1,'(A)') group
  WRITE(v1,*) 
  CLOSE(UNIT=vj)
  DEALLOCATE(A)
END IF  
  
  
  DEALLOCATE(rp,nface0,flag0,cflag0)              !!!
  DEALLOCATE(rk,nface1,flag1,cflag1)              !!!
END DO                                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM flg2xyz                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
