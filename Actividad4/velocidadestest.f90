PROGRAM proyectil_con_resistencia_al_aire
IMPLICIT NONE

 !Constantes necesarias
 REAL,PARAMETER:: g=-9.81, pi=3.1415927
 !Variables necesarias
 REAL::v0x,v0y,k,x,y,ta,ax,ay,fx,fy,vx,vy
 REAL::m,v0,o,vt
 !Contadores
 INTEGER::i,b

  !Presentamos el programa y pedimos los datos
  WRITE(*,*) ' '
  WRITE(*,*) '     Proyectil con resistencia al aire'
  WRITE(*,*) 'Este programa grafica la trayectoria de una pelota de baseball co&
             &nsiderando la resistencia al aire'
  WRITE(*,*) 'Para ello es necesario que introduzcas las siguientes variables'
  WRITE(*,*) ' '
  WRITE(*,*) 'La masa de la pelota en kg'
  READ(*,*) m
  WRITE(*,*) ' '
  WRITE(*,*) 'La velocidad inicial en m/s'
  READ(*,*) v0
  WRITE(*,*) ' '
  WRITE(*,*) 'El angulo de tiro'
  READ(*,*) o
  WRITE(*,*) ' '
  WRITE(*,*) 'Y finalmente la velocidad terminal'
  READ(*,*)  vt
  WRITE(*,*) ' '

   !Convertimos el angulo a radianes
   o=(o*pi)/180.0

   !Calculamos las velocidades de v0x y v0y
   v0x=v0*cos(o)
   v0y=v0*sin(o)

   !Calculamos k
   k=(m*g)/vt

   b=0

    !Abrimos un OPEN para escribir los datos que conseguiremos
    OPEN(1,FILE='datos.dat')
     !Comenzamos el contador en 0
     i=0
     !Abrimos un DO para calcular las distintas posiciones del proyectil
     DO i=0,99999,1
       !Utilizamos una variable real para hacer el contador de 0.1 en 0.1
       ta=i*0.1

       vx=v0x*exp((-k*ta)/m)

       vy=(v0y-((m*g)/k))*exp((-k*ta)/m)+(m*g)/k

       IF (vy<vt) THEN
          vy=vt
       END IF

       
       WRITE(1,*) vx,vy
     END DO

    CLOSE (1)
END PROGRAM proyectil_con_resistencia_al_aire
