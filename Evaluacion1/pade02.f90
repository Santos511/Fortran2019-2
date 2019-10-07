PROGRAM pade
IMPLICIT NONE

REAL(kind=8),external::ExpPx,ExpPz,ExpPw
REAL(kind=8)::ExpX,y,x,E,z,w
INTEGER::i


 OPEN (1,FILE='ErrorExpP02.dat')
  DO i=-31415926,31415926,1000
    x=i*0.0000001
     ExpX=Exp(x)
     y=ExpPx(x)
      E=ExpX-(y/ExpX)
       WRITE(1,*) x,E
  END DO
 CLOSE (1)

 OPEN (2,FILE='ErrorExpP11.dat')
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpX=Exp(z)
     y=ExpPz(z)
      E=ExpX-(y/ExpX)
       WRITE(2,*) z,E
  END DO
 CLOSE (2)

 OPEN (3,FILE='ErrorExpP20.dat')
  DO i=-31415926,31415926,1000
    z=i*0.0000001
     ExpX=Exp(w)
     y=ExpPw(w)
      E=ExpX-(y/ExpX)
       WRITE(3,*) w,E
  END DO
 CLOSE (3)
 
END PROGRAM pade

!===================
FUNCTION ExpPx(x)
!===================
IMPLICIT NONE

REAL(kind=8),intent(in)::x
REAL(kind=8)::ExpPx,a,b

a=1.0

b=1-x+(x**2)*(1.0/2.0)

ExpPx=a/b

END FUNCTION ExpPx

!===================
FUNCTION ExpPz(z)
!===================
IMPLICIT NONE

REAL(kind=8),intent(in)::z
REAL(kind=8)::ExpPz,a,b

a=1+z*(1.0/2.0)

b=1-z*(1.0/2.0)

ExpPz=a/b

END FUNCTION ExpPz

!===================
FUNCTION ExpPw(w)
!===================
IMPLICIT NONE

REAL(kind=8),intent(in)::w
REAL(kind=8)::ExpPw,a,b

a=1+w+(w**2)*(1.0/2.0)

b=1.0

ExpPw=a/b

END FUNCTION ExpPw
