program C777
!
!Este programa te lee el n-ésimo tríangulo de pascal
!
implicit none
integer :: n,nfac,nmnosrfac,p,z,u,v,r,rfac
integer,allocatable :: x(:)
print *,"¿Qué renglón del triángulo de Pascal quiere,excepto el uno?"
read(*,*)n
allocate(x(n))
x(1)=1
x(n)=1
nfac=1
rfac=1
nmnosrfac=1
!
g:do p=1,n-1
nfac=nfac*p
end do g
!
j:do r=1,(n-1)
h:do u=1,(n-1-r)
nmnosrfac=nmnosrfac*u
end do h
i:do v=1,r
rfac=rfac*v
end do i
z=((nfac)/(nmnosrfac*rfac))
x(r+1)=z
rfac=1
nmnosrfac=1
end do j
!
print *,x
read(*,*)!
end program C777
