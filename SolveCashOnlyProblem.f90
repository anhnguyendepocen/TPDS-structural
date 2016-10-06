SUBROUTINE SolveCashOnlyProblem (alpha,xi,pm,tm,te,tb,mv,x,util)
USE globvar
IMPLICIT NONE
REAL(8), INTENT(IN) :: alpha(ncomm),xi(ncomm),pm(ncomm), tm, te, tb, mv
REAL(8), INTENT(OUT) :: x(ncomm,nloc), util(nloc)
INTEGER :: loci, i
REAL(8) :: numeraire, tcoupon


		tcoupon = te + tb

		loci = 0

		! No purchase
		loci = loci + 1
		x(:,loci) = 0.0d0
		numeraire = mv-tcoupon
		util(loci) = DOT_PRODUCT(alpha,DLOG(x(:,loci) + xi)) + numeraire

		! Market only
		loci = loci + 1
		DO i = 1, ncomm
			x(i,loci) = MAX(0.0d0,alpha(i)/pm(i) - xi(i))
		END DO
		numeraire = mv-DOT_PRODUCT(x(:,loci),pm) - tm -tcoupon
		util(loci) = DOT_PRODUCT(alpha,DLOG(x(:,loci) + xi)) + numeraire


END SUBROUTINE SolveCashOnlyProblem