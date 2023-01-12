program capic7
integer ::  a,b,j,i,c,x,y
character(len=6) :: aux1,aux2
print*, "Defina intervalo de enteros positivos separados por espacio o coma"
read*, a,b
ciclo1: do i=a,b
write(aux1,"(I4)")i
ciclo2:do j=len_trim(aux1),1,-1
aux2(len_trim(aux1)+1-j:len_trim(aux1)+1-j)=aux1(j:j)
end do ciclo2
read(aux1,"(I4)")x
read(aux2,"(I4)")y
cond1:if(x==y)then
print*,x
end if cond1
end do ciclo1
write(*,*)
end program capic7



