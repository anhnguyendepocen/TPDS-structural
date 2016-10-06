MODULE MINIMIZATION
IMPLICIT NONE
PUBLIC dfpmin 

CONTAINS
SUBROUTINE dfpmin(theta,n,gtol,func)
IMPLICIT NONE
REAL(8), INTENT(IN)    :: gtol
INTEGER, INTENT(IN)    :: n	        ! number of elements in the parameter vector
REAL(8), INTENT(INOUT) :: theta(n)	! parameter vector over which you which to maximize
INTERFACE	                        ! This lets the subroutine know that func is not a variable, it is a function
	REAL(8) FUNCTION func(theta,n)   ! This is not the likelihood itself, it just let the program know
    IMPLICIT NONE		  
    INTEGER, INTENT(IN) :: n
    REAL(8), INTENT(IN) :: theta(n)
    END FUNCTION func
END INTERFACE
INTEGER, PARAMETER :: ITMAX=20000	! Maximum number of iterations (in case it does not converge)
REAL(8), PARAMETER :: STPMX=10.0,EPS=2.220446049250313D-8,TOLD=4.0d0*EPS ! Tolerance criterions
INTEGER :: j,its ! indices
LOGICAL :: check 
REAL(8) :: den,fac,fad,fae,ftheta,sumdg,sumd ! working matrices
REAL(8) :: dg(n),g(n),hdg(n),thetanew(n),d(n)
REAL(8) :: B(n,n),fret,stpmax,tim
INTEGER :: tin,tout,hz
ftheta=func(theta,n)	        ! initial value of the function
g=gradient(theta,n,ftheta,func)	! initial value of the gradient, if analytical gradient known here
B=0.0d0				! initialize the matrix to a negative definite matrix
DO j = 1, n
	B(j,j)=1.0d0
    WRITE(*,*) g(j)
END DO
d=-MATMUL(B,g)	! initial line direction
stpmax=STPMX*MAX(vabs(theta),DBLE(SIZE(theta)))
DO its=1,ITMAX	! Main loop over iterations
    CALL SYSTEM_CLOCK(count_rate=hz) 
    CALL SYSTEM_CLOCK(count=tin) 
	CALL lnsrch(theta,ftheta,g,d,thetanew,fret,stpmax,check,func)
	ftheta=fret	! update the value of the fuction
    d=thetanew-theta	! update the direction of movement
    theta=thetanew	! update the point
    IF (MAXVAL(DABS(d)/MAX(DABS(theta),1.0D0)) < TOLD) THEN ! Check convergence in Delta-d
        WRITE(*,*) 'TOLD'
        RETURN     
    END IF        
    dg=g				! save the old gradient
    g=gradient(theta,n,ftheta,func)	! and get the new gradient
    den=MAX(ftheta,1.0d0)	
    IF (MAXVAL(DABS(g)*MAX(DABS(theta),1.0d0)/den) < gtol) THEN ! Check for convergence on zero gradient
        WRITE(*,*) 'GTOL'
        RETURN
    END IF     
    dg=g-dg			! compute difference in gradients
    hdg=MATMUL(B,dg)	        ! and difference times current matrix
    fac=DOT_PRODUCT(dg,d)	! Dot products for denominators
    fae=DOT_PRODUCT(dg,hdg)
    sumdg=DOT_PRODUCT(dg,dg)
    sumd=DOT_PRODUCT(d,d)
    IF (fac > DSQRT(EPS*sumdg*sumd)) THEN	! Skip update if fac not positive enough
		fac=1.0d0/fac
        fad=1.0d0/fae
        dg=fac*d-fad*hdg			! Vector that makes BFGS different from DFP
        ! BFGS updating formula
        B=B+fac*outerprod(d,d)-fad*outerprod(hdg,hdg)+fae*outerprod(dg,dg)
    END IF
    d=-MATMUL(B,g)	! Next direction to go
	OPEN(1,file='point.out')
	    DO j = 1, n
		    WRITE(1,'(F16.8)') theta(j)
        END DO
        WRITE(1,'(A,F16.8)') 'log likelihood=',-fret 
	CLOSE(1)		 
    CALL SYSTEM_CLOCK(count=tout) 
    tout=tout-tin
    tim=DBLE(tout)/DBLE(hz)
	write(*,'(A,I10,A,F16.6,A,F16.6)') 'iteration:',its,' value:',fret,' time:',tim
END DO		! Go back for another iteration
PAUSE 'dfpmin: too many iterations'
STOP
END SUBROUTINE dfpmin

FUNCTION outerprod(a,b) ! Gets the outerproduct of two vectors
IMPLICIT NONE	
REAL(8), DIMENSION(:), INTENT(IN) :: a(:),b(:)
REAL(8) :: outerprod(SIZE(a),SIZE(b))
outerprod = SPREAD(a,dim=2,ncopies=SIZE(b)) * &
SPREAD(b,dim=1,ncopies=SIZE(a))
END FUNCTION outerprod

FUNCTION GRADIENT(x0,k,f0,func)
! Routine to get the numerical gradient of a function using forward differences
IMPLICIT NONE
INTEGER, INTENT(IN) :: k
REAL(8), INTENT(IN) :: x0(k),f0
INTERFACE	                       ! This lets the subroutine know that func is not a variable, it is a function
	REAL(8) FUNCTION func(theta,k)  ! This is not the likelihood itself, it just let the program know
    IMPLICIT NONE		       ! the arguments and type of arguments it accepts
    INTEGER, INTENT(IN) :: k
    REAL(8), INTENT(IN) :: theta(k)
    END FUNCTION func
END INTERFACE
REAL(8) :: GRADIENT(k),grdd(k),dh(k),ax0(k),xdh(k)
REAL(8) :: arg(k,k),dax0(k),one
INTEGER :: i
one  = 1.0D0
grdd = 0.0D0
! Computation of stepsize (dh) for gradient
ax0  = DABS(x0)
dax0 = 1.0D0
WHERE (x0.ne.0.0D0) dax0 = x0/ax0
dh = (1.0D-6)*(1.0D-2)*one*dax0
WHERE (ax0.gt.(1.0D-2)*one) dh = (1.0D-8)*ax0*dax0
xdh = x0+dh
arg = SPREAD(x0,DIM=2,NCOPIES=k)
DO i = 1, k
	arg(i,i)=xdh(i)    
    grdd(i)=func(arg(:,i),k)
END DO
GRADIENT = (grdd-f0)/dh
END FUNCTION GRADIENT
  
SUBROUTINE lnsrch(xold,fold,g,p,x,f,stpmax,check,func)
! Given an N-dimensional point xold, the value of the function and gradient there, fold
! and g, and a direction p, finds a new point x along the direction p from xold where the
! function func has decreased sufficiently. xold, g, p, and x are all arrays of length N.
! The new function value is returned in f. stpmax is an input quantity that limits the length
! of the steps so that you do not try to evaluate the function in regions where it is undefined
! or subject to overflow. p is usually the Newton direction. The output quantity check is
! false on a normal exit. It is true when x is too close to xold. In a minimization algorithm,
! this usually signals convergence and can be ignored. However, in a zero-finding algorithm
! the calling program should check whether the convergence is spurious.
! Parameters: ALF ensures su.cient decrease in function value; TOLX is the convergence
! criterion on .x.
IMPLICIT NONE
REAL(8), DIMENSION(:), INTENT(IN) :: xold,g
REAL(8), DIMENSION(:), INTENT(INOUT) :: p(SIZE(xold))
REAL(8), INTENT(IN) :: fold,stpmax
REAL(8), INTENT(OUT) :: x(SIZE(xold))
REAL(8), INTENT(OUT) :: f
LOGICAL, INTENT(OUT) :: check
INTERFACE	! This lets the subroutine know that func is not a variable, it is a function
	REAL(8) FUNCTION func(theta,k)  ! This is not the likelihood itself, it just let the program know
    IMPLICIT NONE		
    INTEGER, INTENT(IN) :: k
    REAL(8), INTENT(IN) :: theta(k)
    END FUNCTION func
END INTERFACE
REAL(8), PARAMETER :: ALF=1.0e-4,TOLX=1.0d-16
INTEGER :: ndum
REAL(8) :: a,alam,alam2,alamin,b,disc,f2,pabs,rhs1,rhs2,slope,tmplam,zero
zero=0.0D0
ndum=SIZE(xold)
IF (SIZE(g).NE.SIZE(xold)) THEN
	PAUSE '***FATAL ERROR: Dimensions do not agree (LNSRCH)***'
    STOP
END IF
check=.false.
pabs=vabs(p(:))
IF (pabs > stpmax) p(:)=p(:)*stpmax/pabs ! Scale if attempted step is too big.
slope=DOT_PRODUCT(g,p)
IF (slope >= zero) THEN
	PAUSE '***FATAL ERROR: roundoff problem in lnsrch***'
    STOP
END IF
alamin=TOLX/MAXVAL(DABS(p(:))/MAX(DABS(xold(:)),1.0D0)) !Compute .min.
alam=1.0D0 ! Always try full Newton step first.
DO ! Start of iteration loop.
	x(:)=xold(:)+alam*p(:)
    F=FUNC(x,SIZE(x))
    IF (alam < alamin) THEN                 ! Convergence on .x. For zero finding,
		!the calling program should verify the convergence.
		x(:)=xold(:)
		check=.true.
		RETURN
    ELSE IF (f <= fold+ALF*alam*slope) THEN ! Sufficient function decrease.
		RETURN
	ELSE !Backtrack.
		IF (alam == 1.0D0) THEN !First time.
			tmplam=-slope/(2.0D0*(f-fold-slope))
        ELSE !Subsequent backtracks.
			rhs1=f-fold-alam*slope
            rhs2=f2-fold-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/&
              (alam-alam2)
            IF (a == 0.0D0) THEN
				tmplam=-slope/(2.0D0*b)
            ELSE
				disc=b*b-3.0*a*slope
                IF (disc < 0.0D0) THEN
					tmplam=0.5D0*alam
                ELSE IF (b <= 0.0D0) THEN
					tmplam=(-b+DSQRT(disc))/(3.0D0*a)
                ELSE
					tmplam=-slope/(b+DSQRT(disc))
                END IF
             END IF
             IF (tmplam > 0.5D0*alam) tmplam=0.5D0*alam
		END IF
	END IF
    alam2=alam
    f2=f
    alam=MAX(tmplam,0.1D0*alam) 
END DO !Try again.
END SUBROUTINE lnsrch

FUNCTION vabs(v)
! Return the length (ordinary L2 norm) of a vector.
REAL(8), DIMENSION(:), INTENT(IN) :: v
REAL(8) :: vabs
vabs=DSQRT(DOT_PRODUCT(v,v))
END FUNCTION vabs

RECURSIVE REAL(8) FUNCTION brent(ax,bx,cx,func,tol,xmin,arg_1,arg_2,arg_3,arg_4)
! Given a function f, and given a bracketing triplet of abscissas ax, bx, cx 
! (such that bx is between ax and cx, and f(bx) is less than both f(ax) and f(cx)), 
! this routine isolates the minimum to a fractional precision of about tol using 
! Brent’s method. The abscissa of the minimum is returned as xmin, and the minimum 
! function value is returned as brent, the returned function value.
! Parameters: Maximum allowed number of iterations; golden ratio; and a small number that
! protects against trying to achieve fractional accuracy for a minimum that happens to be
! exactly zero.
IMPLICIT NONE
REAL(8), INTENT(IN) :: ax,bx,cx,tol,arg_1,arg_2
INTEGER, INTENT(IN) :: arg_3,arg_4
REAL(8), INTENT(OUT) :: xmin
INTERFACE
    REAL(8) FUNCTION func(p,y,a,s,g)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: p,y,a
    INTEGER, INTENT(IN) :: s,g
    END FUNCTION func
END INTERFACE
INTEGER, PARAMETER :: ITMAX=1000
REAL(8), PARAMETER :: CGOLD=0.3819660d0,ZEPS=2.220446049250313D-16
INTEGER :: iter
REAL(8) :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
a=MIN(ax,cx)
b=MAX(ax,cx)
v=bx
w=v
x=v
e=0.0d0
fx=func(x,arg_1,arg_2,arg_3,arg_4)
fv=fx
fw=fx
DO iter = 1, ITMAX
    xm=0.5d0*(a+b)
    tol1=tol*DABS(x)+ZEPS
    tol2=2.0d0*tol1
    IF (DABS(x-xm) <= (tol2-0.5d0*(b-a))) THEN
        xmin=x
        brent=fx
        RETURN
    END IF
    IF (DABS(e) > tol1) THEN
        r=(x-w)*(fx-fv)
        q=(x-v)*(fx-fw)
        p=(x-v)*q-(x-w)*r
        q=2.0d0*(q-r)
        IF (q > 0.0d0) p=-p
        q=DABS(q)
        etemp=e
        e=d
        IF (DABS(p) >= DABS(0.5d0*q*etemp) .or. p <= q*(a-x) .or. p >= q*(b-x)) THEN
            e=MERGE(a-x,b-x, x >= xm )
            d=CGOLD*e
        ELSE
            d=p/q
            u=x+d
            IF (u-a < tol2 .or. b-u < tol2) d=SIGN(tol1,xm-x)
        END IF
    ELSE
        e=MERGE(a-x,b-x, x >= xm )
        d=CGOLD*e
    END IF
    u=MERGE(x+d,x+SIGN(tol1,d), DABS(d) >= tol1 )
    fu=func(u,arg_1,arg_2,arg_3,arg_4)
    IF (fu <= fx) THEN
        IF (u >= x) THEN
            a=x
        ELSE
            b=x
        END IF
        CALL shft(v,w,x,u)
        CALL shft(fv,fw,fx,fu)
    ELSE
        IF (u < x) THEN
            a=u
        ELSE
            b=u
        END IF
        IF (fu <= fw .or. w == x) THEN
            v=w
            fv=fw
            w=u
            fw=fu
        ELSE IF (fu <= fv .or. v == x .or. v == w) THEN
            v=u
            fv=fu
        END IF
    END IF
END DO
PAUSE 'Exceeded maximum iterations'
STOP
CONTAINS
SUBROUTINE shft(a,b,c,d)
REAL(8), INTENT(OUT) :: a
REAL(8), INTENT(INOUT) :: b,c
REAL(8), INTENT(IN) :: d
a=b
b=c
c=d
END SUBROUTINE shft
END FUNCTION brent

END MODULE MINIMIZATION
