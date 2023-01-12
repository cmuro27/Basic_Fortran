program C012
!
!Este programa despliega la boleta de cuatro materias y da promedio
!
implicit none
character(7) :: s,apellido
real :: fisica,calculo,ecuaciones,laboratorio,promedio
print *, "escribir nombre y apellido"
read *, s,apellido
print *, "escriba calificaciones de física,cálculo,ecuaciones y laboratorio"
read(*,3)fisica,calculo,ecuaciones,laboratorio
3 format(f3.1,f3.1,f3.1,f3.1)
promedio=(fisica+calculo+ecuaciones+laboratorio)/4
print *, s,apellido
write(*,5)fisica,calculo,ecuaciones,laboratorio,promedio
5 format("física=",f3.1," ","cálculo=",f3.1," ","ecuaciones=",f3.1," ","laboratorio=",f3.1," ","promedio=",f3.1)
!
end program C012
