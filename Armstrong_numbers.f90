program armstrong7
implicit none
integer :: a,b,n,i,j,k,suma
character(len=6):: aux1
integer, allocatable :: vector(:)
print*, "leer intervalo de n£meros enteros"
read(*,*)a,b
print*, "si quiere que sea a la potencia 3 escriba tres"
read(*,*)n
print*,"estos son los n£meros armstrong en dicho intervalo"
ciclo1:do i=a,b
write(aux1,"(I6)")i
allocate(vector(len_trim(aux1)))
ciclo2:do j=1,len_trim(aux1)
read(aux1(j:j),"(I6)")vector(j)
end do ciclo2
ciclo3:do k=1,len_trim(aux1)
vector(k)=((vector(k))**n)
end do ciclo3
suma=sum(vector)
cond1:if(suma==i)then
print*, i
end if cond1
deallocate(vector)
end do ciclo1
read(*,*)
end program armstrong7
