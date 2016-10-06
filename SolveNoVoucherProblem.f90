SUBROUTINE SolveNoVoucherProblem (alpha,xi,pf,pm,tf,tm,faultyweight,quota,x,util)
USE globvar
IMPLICIT NONE
REAL(8), INTENT(IN) :: alpha(ncomm),xi(ncomm), pf(ncomm),pm(ncomm), tf, tm, faultyweight, quota(ncomm)
REAL(8), INTENT(OUT) :: x(ncomm,nloc), util(nloc)
INTEGER :: loci, i
REAL(8) :: numeraire, x_fps(ncomm), x_market(ncomm)

	loci = 0

	! No purchase
	loci = loci + 1
	x (:,loci) = 0.0d0
	numeraire = 0.0d0

	util(loci) = DOT_PRODUCT(alpha,DLOG(x(:,loci) + xi)) + numeraire

	! Market only
	loci = loci + 1
	DO i = 1, ncomm
		x(i,loci) = MAX(0.0d0,alpha(i)/pm(i) - xi(i))
	END DO
	numeraire = -DOT_PRODUCT(x(:,loci),pm) - tm
	util(loci) = DOT_PRODUCT(alpha,DLOG(x(:,loci) + xi)) + numeraire

	! FPS only
	loci = loci + 1
	DO i = 1, ncomm
		x(i,loci) = MIN(quota(i), MAX(0.0d0,alpha(i)/pf(i) - xi(i)))
	END DO
	numeraire = -DOT_PRODUCT(x(:,loci),pf) - tf
	util(loci) = DOT_PRODUCT(alpha,DLOG(x(:,loci)*(1.0d0-faultyweight) + xi)) + numeraire

	! Both market and FPS
	loci = loci + 1
	DO i = 1, ncomm
		x_fps(i) = MIN(quota(i), MAX(0.0d0,alpha(i)/pf(i) - xi(i)))
		x_market(i) = MAX(0.0d0,alpha(i)/pm(i) - xi(i) - quota(i)*(1.0d0-faultyweight))
        
        x(i,loci ) = x_fps(i) + x_market(i)
					
	END DO

	numeraire = -DOT_PRODUCT(x_fps,pf) -DOT_PRODUCT(x_market,pm) - tm - tf
	util(loci) = DOT_PRODUCT(alpha,DLOG(x_fps*(1.0d0-faultyweight) +x_market + xi)) + numeraire

END SUBROUTINE SolveNoVoucherProblem