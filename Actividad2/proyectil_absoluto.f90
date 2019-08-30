PROGRAM proyectil
IMPLICIT NONE
 !Definimos constantes
 REAL,PARAMETER:: g=9.8, pi=3.1415927
 !Definimos variables
 REAL::a,t,u,x,y

  !Explicamos el programa y pedimos los valores requeridos para el calculo
  WRITE(*,*) ' '
  WRITE(*,*) 'Este programa calcula el tiempo de vuelo, la altura maxima y la d&
             &istancia maxima de un proyectil dadas unas condiciones iniciales'
  WRITE(*,*) 'Para ello debes ingresar los siguientes datos'
  WRITE(*,*) ' '
  WRITE(*,*) 'Angulo incial'
  READ*, a
  WRITE(*,*) ' '
  WRITE(*,*) 'Velocidad inicial dada en m/s'
  READ*, u
  WRITE(*,*) ' '

   !Convertir angulo a radianes para los calculos
   a=a*pi/180.0
  
   !Calculamos el tiempo de vuelo usando las ecuaciones de posición para una
   !y=0
   t=2*u*sin(a)/g

   !Usando el tiempo calculamos la distancia maxima usando ecuacion de posición
   x=u*cos(a)*t

   !Dividimos el tiempo entre 2 porque en ese momento la altura es maxima
   t=t/2

   !Usando el nuevo tiempo calculamos la altura maxima con ecuación de posición
   y=u*sin(a)*t-0.5*g*t*t

   !Regresamos la distancia angular a grados para mostrarla en pantalla
   a=a*180.0/pi

   !Regresamos el tiempo al completo para mostrarlo en pantalla
   t=t*2

    !Escribiendo el resultado en la pantalla
    WRITE(*,*) 'Para un tiro parabolico de angulo',a,'°'
    WRITE(*,*) 'Y una velocidad inicial de       ',u,'m/s'
    WRITE(*,*) 'El tiempo de vuelo es            ',t,'s'
    WRITE(*,*) 'La distancia maxima es           ',x,'m'
    WRITE(*,*) 'Y la altura maxima es            ',y,'m'
    WRITE(*,*) ' '

END PROGRAM proyectil
