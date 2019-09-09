PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.80665, pi=3.1415927
 !Definimos variables
 REAL::a,t,u,x,y,ta
 INTEGER::n,i,b

  !Explicamos el programa y pedimos los valores requeridos para el calculo
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula la trayectoria de un proyectil partiendo de&
             & unas condiciones iniciales'
  WRITE(*,*) 'Para ello debes ingresar los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'Tiempo total de vuelo'
  READ*, t
  WRITE(*,*) ' '
  WRITE(*,*) 'Velocidad inicial dada en m/s'
  READ*, u
  WRITE(*,*) ' '

   !Calcular el angulo usando la velocidad inicial y el tiempo de vuelo
   a=asin((g*t)/(2*u))
  
    !Abrir archivo para guardar los datos que vendrán del DO
    OPEN(1,FILE='salida.dat')

     !Se abre un DO para calcular las distintas posiciones
     DO i=0,99999,1
      !Usamos variable real para hacer el contador de 0.1 en 0.1
      ta=i*0.1

      !Calculamos las posiciones
      x=u*ta*cos(a)
      y=u*ta*sin(a)-0.5*g*ta*ta

       !Condición de altura para salir del DO 
       IF (y<0) EXIT

      !Escribimos las posiciones en el documento 
      WRITE(1,*) x,y
     END DO
    ClOSE (1)

END PROGRAM proyectil
