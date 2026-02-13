!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fcs2pov                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for Embedding-Tensor to PovRay illistration of the cuboid structure.  !!!
! Converts a .fev file to the .pov and .png file formats.                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
USE tools_conv                                                               !!!
USE tools_povr                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: u,nf,ne,nv,i,j,l,ivrot(3),nmax                                     !!!
INTEGER,ALLOCATABLE:: fev(:,:,:)                                             !!!
INTEGER,ALLOCATABLE:: nface(:),e_in_f(:,:),v_in_f(:,:)                       !!!
REAL(KIND=8):: vloc(3),v_at(3),v_up(3),v_rg(3),vl(4,3)                       !!!
REAL(KIND=8):: r(3),delta,blue(4),red(4),v(3)                                !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*20::pov_command                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying the input file                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading part of fcs info from an .fcs or .b.fcs file (allocations inside)  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fcs_only_ev_in_f(nf,ne,nv,nmax,nface,e_in_f,v_in_f,filename)       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Create fev from fcs (allocations inside fcs_to_fev)                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
CALL fcs_to_fev(nf,ne,nv,nmax,nface,e_in_f,v_in_f,fev)                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(NEWUNIT=u,FILE=TRIM(ADJUSTL(filename))//'.pov')                         !!!
vloc(1)=10+0.5*nf ; vloc(2)=10+0.5*nv ; vloc(3)=10+0.5*ne                    !!!
v_at(1)=0.5*nf ; v_at(2)=0.5*nv ; v_at(3)=0.5*ne                             !!!
v_up=0.0D0 ; v_up(2)=4.0*nf+1.0                                              !!!
v_rg=0.0D0 ; v_rg(1)=4.0*nf+1.0                                              !!!
vl(1,:)=10.0    ; vl(1,3)=-10.0                                              !!!
vl(2,:)=vl(1,:) ; vl(2,1)=-10.0                                              !!!
vl(3,:)=vl(2,:) ; vl(3,1)=10.0                                               !!!
vl(4,:)=10.0                                                                 !!!
CALL pov_header(vloc,v_at,v_up,v_rg,vl,u)                                    !!!
! FEV tensor                                                                 !!!
delta=0.35                                                                   !!!
blue=(/0.0,0.0,1.0,0.5/)                                                     !!!
red=(/1.0,0.0,0.0,0.5/)                                                      !!!
DO i=1,nf                                                                    !!!
  r(1)=i-0.5                                                                 !!!
  DO j=1,ne                                                                  !!!
    r(3)=j-0.5                                                               !!!
    DO l=1,nv                                                                !!!
      r(2)=l-0.5                                                             !!!
      IF(fev(i,j,l).eq.1)THEN                                                !!!
        CALL pov_box(r,delta,blue,u)                                         !!!
        v(1)=r(1)-0.5*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)-1.02*delta    !!!
        ivrot=0                                                              !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
        v(1)=r(1)+0.2*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)+1.02*delta    !!!
        ivrot=0 ; ivrot(2)=180                                               !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
        v(1)=r(1)+1.02*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)-0.4*delta    !!!
        ivrot=0 ; ivrot(2)=-90                                               !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
        v(1)=r(1)-1.02*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)+0.4*delta    !!!
        ivrot=0 ; ivrot(2)=90                                                !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
        v(1)=r(1)+0.3*delta ; v(2)=r(2)+1.02*delta ; v(3)=r(3)+0.7*delta     !!!
        ivrot=0 ; ivrot(1)=90; ivrot(2)=180                                  !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
        v(1)=r(1)-0.45*delta ; v(2)=r(2)-1.02*delta ; v(3)=r(3)+0.7*delta    !!!
        ivrot=0 ; ivrot(1)=-90                                               !!!
        CALL pov_bit(v,ivrot,1,u)                                            !!!
      ELSE IF(fev(i,j,l).eq.0)THEN                                           !!!
        CALL pov_box(r,delta,red,u)                                          !!!
        v(1)=r(1)-0.5*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)-1.02*delta    !!!
        ivrot=0                                                              !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
        v(1)=r(1)+0.2*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)+1.02*delta    !!!
        ivrot=0 ; ivrot(2)=180                                               !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
        v(1)=r(1)+1.02*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)-0.4*delta    !!!
        ivrot=0 ; ivrot(2)=-90                                               !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
        v(1)=r(1)-1.02*delta ; v(2)=r(2)-0.75*delta ; v(3)=r(3)+0.4*delta    !!!
        ivrot=0 ; ivrot(2)=90                                                !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
        v(1)=r(1)+0.3*delta ; v(2)=r(2)+1.02*delta ; v(3)=r(3)+0.7*delta     !!!
        ivrot=0 ; ivrot(1)=90; ivrot(2)=180                                  !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
        v(1)=r(1)-0.45*delta ; v(2)=r(2)-1.02*delta ; v(3)=r(3)+0.7*delta    !!!
        ivrot=0 ; ivrot(1)=-90                                               !!!
        CALL pov_bit(v,ivrot,0,u)                                            !!!
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Arrow                                                                      !!!
CALL pov_arrows(nf,1,u)                                                      !!!
! Arrow                                                                      !!!
CALL pov_arrows(nv,2,u)                                                      !!!
! Arrow                                                                      !!!
CALL pov_arrows(ne,3,u)                                                      !!!
CLOSE(UNIT=u)                                                                !!!
pov_command='povray +W1000 +H1000'                                           !!!
CALL SYSTEM(pov_command//' '//TRIM(ADJUSTL(filename))//'.pov')               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fcs2pov                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
