PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.8, pi=3.1415927
 !Definimos las variables
 REAL:: a, t, u

  !Leer valores para el ángulo y la velocidad inicial u desde la
  !terminal
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula el tiempo de vuelo de un proyectil usando p&
             &ara ello caracteristicas iniciales'
  WRITE(*,*) 'Para ello necesito que ingreses los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'El angulo incial'
  READ*, a
  WRITE(*,*) ' '
  WRITE(*,*) 'La velocidad inicial dada en metros sobre segundos'
  READ*, u
  WRITE(*,*) ' '

   !Convirtiendo angulo a radianes
   a=a*pi/180.0
  
   !Calculamos el tiempo
   t=2*u*sin(a)/g

   !Regresamos las distancia a angular a grados para mostrarlo en pantalla
   a=a*180.0/pi
   
    !Escribiendo el resultado en la pantalla
    WRITE(*,*) 'Para un tiro parabolico de angulo',a
    WRITE(*,*) 'Y una velocidad inicial',u
    WRITE(*,*) 'El tiempo de vuelo es',t
    WRITE(*,*) ' '

END PROGRAM proyectil
