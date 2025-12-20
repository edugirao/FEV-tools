!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION d_kro(i,j)
IMPLICIT NONE
INTEGER:: d_kro,i,j
IF(i.eq.j)THEN
  d_kro=1
ELSE
  d_kro=0
END IF
END FUNCTION d_kro

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION tri_d_kro(v1,v2)
IMPLICIT NONE
INTEGER:: tri_d_kro,d_kro,v1(3),v2(3)
tri_d_kro=d_kro(v1(1),v2(1))+ &
        & d_kro(v1(2),v2(2))+ &
        & d_kro(v1(3),v2(3))
END FUNCTION tri_d_kro

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
