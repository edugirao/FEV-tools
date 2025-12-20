PROGRAM faces_to_symbol
IMPLICIT NONE
INTEGER:: i,nf,npol(3:100)
INTEGER:: j,l,n,m,l1,l2,ne,nmax,nv
INTEGER,ALLOCATABLE:: Afn(:,:),fsize(:),av_n(:),tmp(:),is(:)
LOGICAL:: first
CHARACTER*1:: syst
CHARACTER*4:: group
CHARACTER*4:: aux,aux2,aux3
CHARACTER*100:: filename
CHARACTER*200:: string
CHARACTER*200,ALLOCATABLE:: string_s(:)
CHARACTER*4000:: string2
LOGICAL:: have_file_b,have_file_f
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Identifying input file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CALL read_init(filename)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reading faces/edges info on the .fcs file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.b.fcs',EXIST=have_file_b)
IF(have_file_b)THEN
  OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.b.fcs',FORM='UNFORMATTED')
  ! Reading tensor sizes
  READ(3) nf,ne,nv,nmax
  ! Reading faces 1) size,uneq_face, 2) edges, and 3) vertices
  ALLOCATE(fsize(nf),Afn(nf,nmax))
  Afn=0
  DO i=1,nf
    READ(3) fsize(i)
    READ(3) Afn(i,1:fsize(i))
    READ(3) 
    READ(3) 
    READ(3) 
    READ(3) 
  END DO
  CLOSE(UNIT=3)  
ELSE
  INQUIRE(FILE=TRIM(ADJUSTL(filename))//'.fcs',EXIST=have_file_f)
  IF(have_file_f)THEN
    OPEN(UNIT=3,FILE=TRIM(ADJUSTL(filename))//'.fcs')
    ! Reading tensor sizes
    READ(3,*) nf,ne,nv,nmax
    ! Reading faces 1) size,uneq_face, 2) edges, and 3) vertices
    ALLOCATE(fsize(nf),Afn(nf,nmax))
    Afn=0
    DO i=1,nf
      READ(3,*) fsize(i)
      READ(3,*) Afn(i,1:fsize(i))
      READ(3,*) 
      READ(3,*) 
      READ(3,*) 
      READ(3,*) 
    END DO
    CLOSE(UNIT=3)
  ELSE
    STOP 'Error: no .fcs or .b.fcs file found'
  END IF
END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Creating face neighbor size matrix and auxiliary symbols
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO i=1,nf
  Afn(i,1:fsize(i))=fsize(Afn(i,1:fsize(i)))
END DO
! Computing average neighbor size
ALLOCATE(av_n(nf))
av_n=0
DO j=1,nf
  DO i=1,fsize(j)
    av_n(j)=av_n(j)+Afn(j,i)
  END DO    
END DO
! Reordering level 1 - face size
ALLOCATE(tmp(nmax))
DO i=1,nf-1
  DO j=1,nf-i
    IF(fsize(j).gt.fsize(j+1))THEN
      m=fsize(j)
      fsize(j)=fsize(j+1)
      fsize(j+1)=m
      m=av_n(j)
      av_n(j)=av_n(j+1)
      av_n(j+1)=m      
      tmp=Afn(j,:)
      Afn(j,:)=Afn(j+1,:)
      Afn(j+1,:)=tmp
    END IF
  END DO
END DO
! Reordering level 2 - neighbor faces sizes
DO n=1,nf
  DO i=1,fsize(n)-1
    DO j=1,fsize(n)-i
      IF(Afn(n,j).gt.Afn(n,j+1))THEN
        m=Afn(n,j)
        Afn(n,j)=Afn(n,j+1)
        Afn(n,j+1)=m
      END IF
    END DO
  END DO
END DO
! Reordering level 3 - average neigh size
l1=1
l2=1
DO l=2,nf
  IF(fsize(l).eq.fsize(l-1))THEN 
    l2=l2+1
  END IF
  IF((fsize(l).ne.fsize(l-1)).OR.(l.eq.nf))THEN    
    ! Reorder the faces from l1 to l2
    IF(l2-l1.gt.0)THEN
      DO i=l1,l2-1
        DO j=l1,l2-(i-l1+1)
          IF(av_n(j).gt.av_n(j+1))THEN
            m=av_n(j)
            av_n(j)=av_n(j+1)
            av_n(j+1)=m          
            tmp=Afn(j,:)
            Afn(j,:)=Afn(j+1,:)
            Afn(j+1,:)=tmp
          END IF        
        END DO
      END DO
    END IF
    l1=l
    l2=l
  END IF
END DO  
! Reordering level 4 - smallest neigh size
n=1
l1=1
l2=1
DO l=2,nf
  IF((fsize(l).eq.fsize(l-1)).AND.(av_n(l).eq.av_n(l-1)))THEN 
    l2=l2+1
  END IF
  IF(((fsize(l).ne.fsize(l-1)).OR.(av_n(l).ne.av_n(l-1))).OR.(l.eq.nf))THEN    
    ! Reorder the faces from l1 to l2
    IF(l2-l1.gt.0)THEN
      DO i=l1,l2-1
        DO j=l1,l2-(i-l1+1)
          IF(Afn(j,n).ge.Afn(j+1,n))THEN
            tmp=Afn(j,:)
            Afn(j,:)=Afn(j+1,:)
            Afn(j+1,:)=tmp
          END IF        
        END DO
      END DO
    END IF
    l1=l
    l2=l
  END IF
END DO  
! S aux symbol for each face
ALLOCATE(string_s(nf))
DO j=1,nf
  npol=0
  DO i=1,fsize(j)
    npol(Afn(j,i))=npol(Afn(j,i))+1
  END DO
  first=.true.
  string_s(j)=''
  DO i=3,100
    IF(npol(i).gt.0)THEN
      WRITE(aux,'(I4)') i
      string_s(j)=TRIM(ADJUSTL(string_s(j)))//TRIM(ADJUSTL(aux))//'^'
      WRITE(aux,'(I4)') npol(i)
      string_s(j)=TRIM(ADJUSTL(string_s(j)))//'{'//TRIM(ADJUSTL(aux))//'}'
    END IF
  END DO
END DO
! Counting polygons for the whole structure
npol=0
DO i=1,nf
  npol(fsize(i))=npol(fsize(i))+1
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Simple group
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.sym')
READ(1,*) group
CLOSE(UNIT=1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Simple symbol
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=1,FILE=TRIM(ADJUSTL(filename))//'.smb')
WRITE(1,'(A)') '!*********** System Polygonal Symbol ************!'
! Getting the system
syst='x'
IF(TRIM(ADJUSTL(group)).eq.'p6m' ) syst='h'
IF(TRIM(ADJUSTL(group)).eq.'p6'  ) syst='h'
IF(TRIM(ADJUSTL(group)).eq.'p31m') syst='h'
IF(TRIM(ADJUSTL(group)).eq.'p3m1') syst='h'
IF(TRIM(ADJUSTL(group)).eq.'p3'  ) syst='h'
IF(TRIM(ADJUSTL(group)).eq.'p4m' ) syst='t'
IF(TRIM(ADJUSTL(group)).eq.'p4g' ) syst='t'
IF(TRIM(ADJUSTL(group)).eq.'p4'  ) syst='t'
IF(TRIM(ADJUSTL(group)).eq.'p2'  ) syst='o'
IF(TRIM(ADJUSTL(group)).eq.'cmm' ) syst='c'
IF(TRIM(ADJUSTL(group)).eq.'pmm' ) syst='r'
IF(TRIM(ADJUSTL(group)).eq.'pgg' ) syst='r'
IF(TRIM(ADJUSTL(group)).eq.'pmg' ) syst='r'
IF(TRIM(ADJUSTL(group)).eq.'pg'  ) syst='r'
IF(TRIM(ADJUSTL(group)).eq.'cm'  ) syst='c'
IF(TRIM(ADJUSTL(group)).eq.'pm'  ) syst='r'
IF(TRIM(ADJUSTL(group)).eq.'p1'  ) syst='o'
! Mounting the symbol
string=TRIM(ADJUSTL(syst))
first=.true.
DO i=3,100
  IF(npol(i).gt.0)THEN
    WRITE(aux,'(I4)') i
    string=TRIM(ADJUSTL(string))//TRIM(ADJUSTL(aux))//'^'
    WRITE(aux,'(I4)') npol(i)
    string=TRIM(ADJUSTL(string))//'{'//TRIM(ADJUSTL(aux))//'}'
  END IF
END DO
WRITE(1,'(A)') TRIM(ADJUSTL(string))
! Creating a latex-based pdf
OPEN(UNIT=4,FILE='tex.tex')
WRITE(4,'(A)') '\documentclass[crop]{standalone}'
WRITE(4,'(A)') '\begin{document}'
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string))//'$'
WRITE(4,'(A)') '\end{document}'
CLOSE(UNIT=4)
CALL SYSTEM('pdflatex tex.tex > z.txt')
CALL SYSTEM('rm z.txt')
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-1.pdf')
CALL SYSTEM('rm tex.*')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Semi-extended symbol
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(1,'(A)') '!*** System Polygonal Symbol (semi-extended) ****!'
! System letter
string2=TRIM(ADJUSTL(syst))
m=0  ! aux symbol counter
n=0  ! face counter
ALLOCATE(is(nf))
is=0
DO i=3,100
  IF(npol(i).gt.0)THEN
    WRITE(aux,'(I4)') i
    l1=1
    l2=1
    DO j=1,npol(i)
      n=n+1
      IF(j.eq.1) CYCLE
      IF(string_s(n).eq.string_s(n-1))THEN
        l2=l2+1
      END IF
      IF(string_s(n).ne.string_s(n-1))THEN
        m=m+1      
        is(m)=n-1
        WRITE(aux2,'(I4)') m
        WRITE(aux3,'(I4)') l2-l1+1
        string2=TRIM(ADJUSTL(string2))//TRIM(ADJUSTL(aux))//'^{'//TRIM(ADJUSTL(aux3))
        string2=TRIM(ADJUSTL(string2))//'}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'
        l1=j
        l2=j
      END IF  
    END DO  
    m=m+1      
    is(m)=n
    WRITE(aux2,'(I4)') m
    WRITE(aux3,'(I4)') l2-l1+1
    string2=TRIM(ADJUSTL(string2))//TRIM(ADJUSTL(aux))//'^{'//TRIM(ADJUSTL(aux3))
    string2=TRIM(ADJUSTL(string2))//'}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'
  END IF
END DO
! aux symbol
DO i=1,m
  WRITE(aux,'(I4)') i
  string2=TRIM(ADJUSTL(string2))//'-\{\mathcal{S}_{'//TRIM(ADJUSTL(aux))//'}='//TRIM(ADJUSTL(string_s(is(i))))//'\}'
END DO
WRITE(1,'(A)') TRIM(ADJUSTL(string2))
! Creating a latex-based pdf
OPEN(UNIT=4,FILE='tex.tex')
WRITE(4,'(A)') '\documentclass[crop]{standalone}'
WRITE(4,'(A)') '\begin{document}'
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string2))//'$'
WRITE(4,'(A)') '\end{document}'
CLOSE(UNIT=4)
CALL SYSTEM('pdflatex tex.tex > z.txt')
CALL SYSTEM('rm z.txt')
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-2.pdf')
CALL SYSTEM('rm tex.*')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Full-extended symbol
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(1,'(A)') '!*** System Polygonal Symbol (full-extended) ****!'
! System letter
string2=TRIM(ADJUSTL(syst))
m=0  ! aux symbol counter
DO i=3,100
  IF(npol(i).gt.0)THEN
    WRITE(aux,'(I4)') i      
    DO j=1,npol(i)
      m=m+1      
      WRITE(aux2,'(I4)') m
      string2=TRIM(ADJUSTL(string2))//TRIM(ADJUSTL(aux))//'^{1}_{\mathcal{S}_{'//TRIM(ADJUSTL(aux2))//'}}'
    END DO  
  END IF
END DO
! Aux symbol
m=0  ! aux symbol counter
DO i=3,100
  IF(npol(i).gt.0)THEN
    DO j=1,npol(i)
      m=m+1      
      WRITE(aux,'(I4)') m
      string2=TRIM(ADJUSTL(string2))//'-\{\mathcal{S}_{'//TRIM(ADJUSTL(aux))//'}='//TRIM(ADJUSTL(string_s(m)))//'\}'
    END DO
  END IF
END DO
WRITE(1,'(A)') TRIM(ADJUSTL(string2))
! Creating a latex-based pdf
OPEN(UNIT=4,FILE='tex.tex')
WRITE(4,'(A)') '\documentclass[crop]{standalone}'
WRITE(4,'(A)') '\begin{document}'
WRITE(4,'(A)') '$'//TRIM(ADJUSTL(string2))//'$'
WRITE(4,'(A)') '\end{document}'
CLOSE(UNIT=4)
CALL SYSTEM('pdflatex tex.tex > z.txt')
CALL SYSTEM('rm z.txt')
CALL SYSTEM('mv tex.pdf '//TRIM(ADJUSTL(filename))//'-3.pdf')
CALL SYSTEM('rm tex.*')

END PROGRAM faces_to_symbol
