!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM fev2pov                                                              !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tool for Embedding-Tensor to PovRay illistration of the cuboid structure.  !!!
! Converts a .fev file to the .pov and .png file formats.     s               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE tools_read                                                               !!!
IMPLICIT NONE                                                                !!!
INTEGER:: nf,ne,nv,i,j,l                                                     !!!
INTEGER,ALLOCATABLE:: fev(:,:,:)                                             !!!
REAL(KIND=8):: r(3),delta,v1(3)                                              !!!
CHARACTER*100:: filename                                                     !!!
CHARACTER*20::pov_command                                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying the input file                                                 !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename,'inp')                                               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading embedding tensor from .fev file (allocations inside)               !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_fev(nf,ne,nv,fev,filename)                                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.pov')                            !!!
WRITE(1,*) '#include "transforms.inc"'                                       !!!
WRITE(1,*) 'camera {'                                                        !!!
WRITE(1,*) '  orthographic'                                                  !!!
v1(1)=10+0.5*nf ; v1(2)=10+0.5*nv ; v1(3)=10+0.5*ne                          !!!
WRITE(1,*) '  location <',v1(1),',',v1(2),',',v1(3),'>'                      !!!
v1(1)=0.5*nf ; v1(2)=0.5*nv ; v1(3)=0.5*ne                                   !!!
WRITE(1,*) '  look_at <',v1(1),',',v1(2),',',v1(3),'>'                       !!!
v1=0.0D0 ; v1(2)=4.0*nf+1.0                                                  !!!
WRITE(1,*) '  up <',v1(1),',',v1(2),',',v1(3),'>'                            !!!
v1=0.0D0 ; v1(1)=4.0*nf+1.0                                                  !!!
WRITE(1,*) '  right <',v1(1),',',v1(2),',',v1(3),'>'                         !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'light_source {'                                                  !!!
WRITE(1,*) '  <10.0000, 10.0000, -10.0000>'                                  !!!
WRITE(1,*) '  color rgb<1.000, 1.000, 1.000>'                                !!!
WRITE(1,*) '  parallel'                                                      !!!
WRITE(1,*) '  point_at <0.0, 0.0, 0.0>'                                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'light_source {'                                                  !!!
WRITE(1,*) '  <-10.0000, 10.0000, -10.0000>'                                 !!!
WRITE(1,*) '  color rgb<1.000, 1.000, 1.000>'                                !!!
WRITE(1,*) '  parallel'                                                      !!!
WRITE(1,*) '  point_at <0.0, 0.0, 0.0>'                                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'light_source {'                                                  !!!
WRITE(1,*) '  <-10.0000, 10.0000, 10.0000>'                                  !!!
WRITE(1,*) '  color rgb<1.000, 1.000, 1.000>'                                !!!
WRITE(1,*) '  parallel'                                                      !!!
WRITE(1,*) '  point_at <0.0, 0.0, 0.0>'                                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'light_source {'                                                  !!!
WRITE(1,*) '  <10.0000, 10.0000, 10.0000>'                                   !!!
WRITE(1,*) '  color rgb<1.000, 1.000, 1.000>'                                !!!
WRITE(1,*) '  parallel'                                                      !!!
WRITE(1,*) '  point_at <0.0, 0.0, 0.0>'                                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'background {'                                                    !!!
WRITE(1,*) '  color rgb<0,0,0>'                                              !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) '#default { texture {'                                            !!!
WRITE(1,*) ' finish { ambient 0.000 diffuse 0.650 phong', &                  !!!
&' 0.1 phong_size 40.000 specular 0.500 }'                                   !!!
WRITE(1,*) '} }'                                                             !!!
WRITE(1,*) '#include "textures.inc"'                                         !!!
WRITE(1,*) '#include "woods.inc"'                                            !!!
WRITE(1,*) '#include "glass.inc"'                                            !!!
WRITE(1,*) '#include "shapes3.inc"'                                          !!!
WRITE(1,*) '#declare em=chr(144)'                                            !!!
! FEV tensor                                                                 !!!
delta=0.35                                                                   !!!
pov_command='povray +W1000 +H1000'                                           !!!
DO i=1,nf                                                                    !!!
  r(1)=i-0.5                                                                 !!!
  DO j=1,ne                                                                  !!!
    r(3)=j-0.5                                                               !!!
    DO l=1,nv                                                                !!!
      r(2)=l-0.5                                                             !!!
      IF(fev(i,j,l).eq.1)THEN                                                !!!
        WRITE(1,*) 'box {<',r(1)-delta,',',r(2)-delta,',',r(3)-delta,'>,'    !!!
        WRITE(1,*) '<',r(1)+delta,',',r(2)+delta,',',r(3)+delta,'>'          !!!
        WRITE(1,*)  '  texture { pigment { color rgbt <',0.0,',',0.0,',',1.0,',',0.5,'>}}'
        WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '              !!!
        WRITE(1,*) '}'                                                       !!!
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,0,0>           '                             !!!
        WRITE(1,*) '   translate<',r(1)-0.5*delta,',',r(2)-0.75*delta,',',r(3)-1.02*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,180,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)+0.2*delta,',',r(2)-0.75*delta,',',r(3)+1.02*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,-90,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)+1.02*delta,',',r(2)-0.75*delta,',',r(3)-0.4*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,90,0>           '                            !!!
        WRITE(1,*) '   translate<',r(1)-1.02*delta,',',r(2)-0.75*delta,',',r(3)+0.4*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<90,180,0>           '                          !!!
        WRITE(1,*) '   translate<',r(1)+0.3*delta,',',r(2)+1.02*delta,',',r(3)+0.7*delta,'>}'            
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "1",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<-90,0,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)-0.45*delta,',',r(2)-1.02*delta,',',r(3)+0.7*delta,'>}'                    
      ELSE IF(fev(i,j,l).eq.0)THEN                                           !!!
        WRITE(1,*) 'box {<',r(1)-delta,',',r(2)-delta,',',r(3)-delta,'>,'    !!!
        WRITE(1,*) '<',r(1)+delta,',',r(2)+delta,',',r(3)+delta,'>'          !!!
        WRITE(1,*)  '  texture { pigment { color rgbt <',1.0,',',0.0,',',0.0,',',0.5,'>}}'      
        WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '              !!!
        WRITE(1,*) '}'                                                       !!!
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,0,0>           '                             !!!
        WRITE(1,*) '   translate<',r(1)-0.5*delta,',',r(2)-0.75*delta,',',r(3)-1.02*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,180,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)+0.2*delta,',',r(2)-0.75*delta,',',r(3)+1.02*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,-90,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)+1.02*delta,',',r(2)-0.75*delta,',',r(3)-0.4*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<0,90,0>           '                            !!!
        WRITE(1,*) '   translate<',r(1)-1.02*delta,',',r(2)-0.75*delta,',',r(3)+0.4*delta,'>}'      
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<90,180,0>           '                          !!!
        WRITE(1,*) '   translate<',r(1)+0.3*delta,',',r(2)+1.02*delta,',',r(3)+0.7*delta,'>}'            
        WRITE(1,*) 'text{'                                                   !!!
        WRITE(1,*) '   ttf "arial.ttf",'                                     !!!
        WRITE(1,*) '   "0",0.02, 0'                                          !!!
        WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',0.0,',',0.0,'>}'
        WRITE(1,*)  '  finish { reflection <0,0,0> specular 0.0 ambient 0 diffuse 0.0 }}'
        WRITE(1,*) '   scale 0.7'                                            !!!
        WRITE(1,*) '   rotate<-90,0,0>           '                           !!!
        WRITE(1,*) '   translate<',r(1)-0.45*delta,',',r(2)-1.02*delta,',',r(3)+0.7*delta,'>}'                          
      END IF                                                                 !!!
    END DO                                                                   !!!
  END DO                                                                     !!!
END DO                                                                       !!!
! Arrow                                                                      !!!
WRITE(1,*) 'cylinder {<',0.0,',',0.0,',',0.0,'>,'                            !!!
WRITE(1,*) '<',nf,',',0,',',0,'>,', 0.05                                     !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'cone {<',nf,',',0.0,',',0.0,'>,',0.2                             !!!
WRITE(1,*) '<',nf+0.5,',',0,',',0,'>,', 0.0                                  !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
! Arrow                                                                      !!!
WRITE(1,*) 'cylinder {<',0.0,',',0.0,',',0.0,'>,'                            !!!
WRITE(1,*) '<',0,',',nv,',',0,'>,', 0.05                                     !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'cone {<',0.0,',',nv,',',0.0,'>,',0.2                             !!!
WRITE(1,*) '<',0.0,',',nv+0.5,',',0.0,'>,', 0.0                              !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
! Arrow                                                                      !!!
WRITE(1,*) 'cylinder {<',0.0,',',0.0,',',0.0,'>,'                            !!!
WRITE(1,*) '<',0.0,',',0.0,',',ne,'>,', 0.05                                 !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
WRITE(1,*) 'cone {<',0.0,',',0.0,',',ne,'>,',0.2                             !!!
WRITE(1,*) '<',0.0,',',0.0,',',ne+0.5,'>,', 0.0                              !!!
WRITE(1,*)  '  texture { pigment { color rgb <',0.0,',',1.0,',',0.0,'>}}'    !!!
WRITE(1,*) ' finish { reflection 0.03 specular 0.35 } '                      !!!
WRITE(1,*) '}'                                                               !!!
CLOSE(UNIT=1)                                                                !!!
! CALL SYSTEM(pov_command//' '//TRIM(ADJUSTL(filename))//'.pov')             !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END PROGRAM fev2pov                                                          !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
