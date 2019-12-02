PROGRAM Paralelepipedo
IMPLICIT NONE

 REAL :: a, b, c, Volumen

  PRINT *, 'Favor de ingresar los tres lados distintos de su paralelepipedo par&
           &a realizar el calculo del volumen'
  READ *, a, b, c
  PRINT *, 'Volumen del paralelepipedo', Volumen(a,b,c)

END PROGRAM Paralelepipedo

FUNCTION Volumen(x,y,z)
IMPLICIT NONE

 REAL :: Volumen !Tipo Function
 REAL, INTENT( IN ) :: x, y, z
 
  Volumen= x*y*z

END FUNCTION Volumen
