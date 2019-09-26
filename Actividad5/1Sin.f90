PROGRAM Taylor
IMPLICIT NONE

 !Declaracion de variables necesarias
 REAL(kind=8),external:: SinTaylor  !Esta es la variable que viene desde fuera
 REAL(kind=8)::y,x
 INTEGER:: n,i,a

  a=0 !Variable integer para darle color a las curvas

   !Abrimos un documento para graficar las aproximaciones de taylor de SIN
   OPEN(1,FILE='Sin.dat')
    !Primer DO para el grado del polinomio, desde 1 hasta 11 de 2 en 2
    DO n=1,11,2
      a=a+1 !Cambiar color de la curva
       !Segundo DO para el valor de la x, yendo desde -10 hasta 10
       DO i=-100,100,1
         x=0.1*i          !Aqui convertimos a real el valor y lo reducimos
         y=SinTaylor(x,n) !Asigna a y la suma de Taylor de grado n y valor x
         WRITE(1,*) x,y,a !Se escribe en el documento los valores y el color
       END DO
        WRITE(1,*) ' '    !Escribimos un espacio para separar las curvas
    END DO
   CLOSE (1)

END PROGRAM Taylor

!==========================
FUNCTION SinTaylor(x,n)   
!==========================
IMPLICIT NONE

 !Se definen los valores externos para usar
 REAL(kind=8),intent(in):: x !Valor a valuar
 INTEGER,intent(in):: n      !Grado del polinomio
 REAL(kind=8):: SinTaylor    !Nombre con el que saldra el valor de la funcion
 !Variables que se usaran aqui y ya
 REAL(kind=8):: term,suma,a,b,c,d
 INTEGER:: j

  suma=0 !Comenzamos dando valor 0 a la suma

   !Primer DO para comenzar la suma desde grado 0 hasta n
   Taylor:DO j=0,n,1 
          a=(-1.0)**j !Habiendo separado la sumatoria en tres partes queda como
          b=2*j+1     !a=numerador, b=denominador, c=multiplicador, de una misma
          c=x**b      !operacion
           d=b         !Asignar el valor de b a d para calcular el factorial
           Factorial:DO    !Do para calcular el factorial de la siguiente forma:
                     d=d-1             !b!=b*(b-1)*(b-2)*...*(b-n)*1
                     b=b*d
                      IF(d==1) EXIT 
                      IF(d==0) THEN !En caso de que d sea 0 se le asigna el
                         b=1        !valor de 1 a b y sale del DO
                         EXIT
                      END IF   
           END DO Factorial

           term=(a/b)*c      !Se realiza la operacion de la sumatoria
            suma=suma+term   !Se suma al valor anterior
   END DO Taylor

    SinTaylor=suma !Se le asigna el valor de la sumatoria a la variable externa

END FUNCTION SinTaylor
