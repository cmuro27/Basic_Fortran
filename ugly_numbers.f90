program feos7
implicit none
integer :: a,b,i,aux1,aux2,aux3,aux4,aux5,aux6,z
a=1
print*,"Introduzca n£mero b que ser  el l¡mite superior de un intervalo a,b"
print*,"donde a=1 y se imprimiran los n£meros feos que existen entre 1 y b"
read(*,*)b
print*, a
ciclo1:do i=a+1,b
aux1=i
aux2=i
aux3=i
aux5=0
aux4=0
aux6=0
z=mod(i,2)
 33 cond1:if(mod(aux1,2)==0)then
z=aux1/2
aux4=aux4+1
cond1a:if(z==1)then
write(*,2)i,aux4,aux5,aux6
2 format(I4,"  ",I4," veces 2  ",I4," veces 3  ",I4," veces 5")
else
aux1=z
aux2=aux1
aux3=aux1
go to 33
end if cond1a
end if cond1
34 cond2:if(mod(aux2,3)==0)then
z=aux2/3
aux5=aux5+1
cond2a:if(z==1)then
write(*,3)i,aux4,aux5,aux6
3 format(I4,"  ",I4," veces 2  ",I4," veces 3  ",I4," veces 5")
else
aux2=z
aux3=aux2
go to 34
end if cond2a
end if cond2
35 cond3:if(mod(aux3,5)==0)then
z=aux3/5
aux6=aux6+1
cond3a:if(z==1)then
write(*,4)i,aux4,aux5,aux6
4 format(I4,"  ",I4," veces 2  ",I4," veces 3  ",I4," veces 5")
else
aux3=z
go to 35
end if cond3a
end if cond3
end do ciclo1
read(*,*)
end program feos7



