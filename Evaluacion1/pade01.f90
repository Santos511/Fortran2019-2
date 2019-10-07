PROGRAM pade
IMPLICIT NONE

REAL(kind=8),external::SinP
REAL(kind=8)::SinX,y,x,b,E
INTEGER::i

 b=0

 OPEN (1,FILE='SinP.dat')
  DO i=-31415926,31415926,1000
    x=i*0.0000001
     SinX=Sin(x)
      WRITE(1,*) x,SinX,b
  END DO

 WRITE(1,*) ' '
 b=1

  DO i=-31415926,31415926,1000
    x=i*0.0000001
     y=SinP(x)
      WRITE(1,*) x,y,b
  END DO
 CLOSE (1)
 
 OPEN (2,FILE='ErrorSinP.dat')
  DO i=0,31415926,1000
    x=i*0.0000001
     SinX=Sin(x)
     y=SinP(x)
      E=SinX-(y/SinX)
       WRITE(2,*) x,E
  END DO
 CLOSE (2)

END PROGRAM pade

!===================
FUNCTION SinP(x)
!===================
IMPLICIT NONE

REAL(kind=8),intent(in)::x
REAL(kind=8)::SinP,a,b

a=x-(x**3)*(2363.0/18183.0)+(x**5)*(12671.0/4363920.0)

b=1+(x**2)*(445.0/12122.0)+(x**4)*(601.0/872784.0)+(x**6)*(121.0/16662240.0)

SinP=a/b

END FUNCTION SinP
