!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE tools_povr                                                            !!!
IMPLICIT NONE                                                                !!!
PUBLIC                                                                       !!!
CONTAINS                                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE pov_header(vloc,v_at,v_up,v_rg,vl,u)                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
REAL(KIND=8):: vloc(3),v_at(3),v_up(3),v_rg(3),vl(4,3)                       !!!
INTEGER:: i,u                                                                !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(u,*) '#include "transforms.inc"'                                       !!!
WRITE(u,*) 'camera {'                                                        !!!
WRITE(u,*) '  orthographic'                                                  !!!
WRITE(u,*) '  location <',vloc(1),',',vloc(2),',',vloc(3),'>'                !!!
WRITE(u,*) '  look_at <',v_at(1),',',v_at(2),',',v_at(3),'>'                 !!!
WRITE(u,*) '  up <',v_up(1),',',v_up(2),',',v_up(3),'>'                      !!!
WRITE(u,*) '  right <',v_rg(1),',',v_rg(2),',',v_rg(3),'>'                   !!!
WRITE(u,*) '}'                                                               !!!
DO i=1,4                                                                     !!!
  WRITE(u,*) 'light_source {'                                                !!!
  WRITE(u,*) '  <',vl(i,1),',',vl(i,2),',',vl(i,3),'>'                       !!!
  WRITE(u,*) '  color rgb<1.000, 1.000, 1.000>'                              !!!
  WRITE(u,*) '  parallel'                                                    !!!
  WRITE(u,*) '  point_at <0.0, 0.0, 0.0>'                                    !!!
  WRITE(u,*) '}'                                                             !!!
END DO                                                                       !!!
WRITE(u,*) 'background {'                                                    !!!
WRITE(u,*) '  color rgb<0,0,0>'                                              !!!
WRITE(u,*) '}'                                                               !!!
WRITE(u,*) '#default { texture {'                                            !!!
WRITE(u,*) ' finish { ambient 0.000 diffuse 0.650 phong', &                  !!!
&' 0.1 phong_size 40.000 specular 0.500 }'                                   !!!
WRITE(u,*) '} }'                                                             !!!
WRITE(u,*) '#include "textures.inc"'                                         !!!
WRITE(u,*) '#include "woods.inc"'                                            !!!
WRITE(u,*) '#include "glass.inc"'                                            !!!
WRITE(u,*) '#include "shapes3.inc"'                                          !!!
WRITE(u,*) '#declare em=chr(144)'                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE pov_header                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE pov_box(r,delta,color,u)                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
REAL(KIND=8):: r(3),delta,color(4)                                           !!!
INTEGER:: u                                                                  !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(u,*) 'box {<',r(1)-delta,',',r(2)-delta,',',r(3)-delta,'>,'            !!!
WRITE(u,*) '<',r(1)+delta,',',r(2)+delta,',',r(3)+delta,'>'                  !!!
WRITE(u,*)  '  texture { pigment { color rgbt <',color(1),',',color(2),',',color(3),',',color(4),'>}}'
WRITE(u,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(u,*) '}'                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE pov_box                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE pov_bit(v,ivrot,bit,u)                                            !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
REAL(KIND=8):: v(3)                                                          !!!
INTEGER:: bit,u,ivrot(3)                                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(u,*) 'text{'                                                           !!!
WRITE(u,*) '   ttf "arial.ttf",'                                             !!!
IF(bit.eq.1) WRITE(u,*) '   "1",0.02, 0'                                     !!!
IF(bit.eq.0) WRITE(u,*) '   "0",0.02, 0'                                     !!!
WRITE(u,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'     !!!
WRITE(u,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
WRITE(u,*) '   scale 0.7'                                                    !!!
WRITE(u,*) '   rotate<',ivrot(1),',',ivrot(2),',',ivrot(3),'> '              !!!
WRITE(u,*) '   translate<',v(1),',',v(2),',',v(3),'>}'                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE pov_bit                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE pov_arrows(n,i,u)                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This subroutine reads (from the inp file) the                              !!!
! filelabel of the filelabel.fev file to be read                             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE                                                                !!!
INTEGER:: n,i,u                                                              !!!
REAL(KIND=8):: ivec(3)                                                       !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ivec=0.0                                                                     !!!
ivec(i)=n                                                                    !!!
WRITE(u,*) 'cylinder {<',0.0,',',0.0,',',0.0,'>,'                            !!!
WRITE(u,*) '<',ivec(1),',',ivec(2),',',ivec(3),'>,', 0.05                    !!!
WRITE(u,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(u,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(u,*) '}'                                                               !!!
WRITE(u,*) 'cone {<',ivec(1),',',ivec(2),',',ivec(3),'>,',0.2                !!!
ivec(i)=ivec(i)+0.5                                                          !!!
WRITE(u,*) '<',ivec(1),',',ivec(2),',',ivec(3),'>,', 0.0                     !!!
WRITE(u,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(u,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(u,*) '}'                                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE pov_arrows                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE tools_povr                                                        !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
