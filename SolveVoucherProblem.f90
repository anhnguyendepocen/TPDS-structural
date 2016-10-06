SUBROUTINE SolveVoucherProblem (alpha,xi,pf,pm,tf,tm,te,tb,faultyweight,quota,mv,x,util)
USE globvar
IMPLICIT NONE
REAL(8), INTENT(IN) :: alpha(ncomm),xi(ncomm),pf(ncomm),pm(ncomm), tf, tm, te, tb, faultyweight, quota(ncomm,ncoupon),mv(ncoupon)
REAL(8), INTENT(OUT) :: x(ncomm,nloc,ncoupon), util(nloc,ncoupon)
INTEGER :: loci, couponi, i
REAL(8) :: numeraire, quota0(ncomm), x_fps(ncomm), x_market(ncomm), tcoupon


	DO couponi = 1, ncoupon

		quota0 = quota(:,couponi)

		IF (couponi==1) THEN
			tcoupon = 0
		ELSE 
			tcoupon = te + tb
		END IF


		loci = 0

		! No purchase
		loci = loci + 1
		x(:,loci,couponi) = 0.0d0
		numeraire = mv(couponi)-tcoupon
		util(loci,couponi) = DOT_PRODUCT(alpha,DLOG(x(:,loci,couponi) + xi)) + numeraire

		! Market only
		loci = loci + 1
		DO i = 1, ncomm
			x(i,loci,couponi) = MAX(0.0d0,alpha(i)/pm(i) - xi(i))
		END DO
		numeraire = mv(couponi)-DOT_PRODUCT(x(:,loci,couponi),pm) - tm -tcoupon
		util(loci,couponi) = DOT_PRODUCT(alpha,DLOG(x(:,loci,couponi) + xi)) + numeraire

		! FPS only
		loci = loci + 1
		DO i = 1, ncomm
			x(i,loci,couponi) = MIN(quota0(i), MAX(0.0d0,alpha(i)/pf(i) - xi(i)))
		END DO
		numeraire = mv(couponi)-DOT_PRODUCT(x(:,loci,couponi),pf) - tf -tcoupon
		util(loci,couponi) = DOT_PRODUCT(alpha,DLOG(x(:,loci,couponi)*(1.0d0-faultyweight) + xi)) + numeraire

		! Both market and FPS
		loci = loci + 1
		DO i = 1, ncomm
			x_fps(i) = MIN(quota0(i), MAX(0.0d0,alpha(i)/pf(i) - xi(i)))
			x_market(i) = MAX(0.0d0,alpha(i)/pm(i) - xi(i) - quota0(i)*(1.0d0-faultyweight))
            x(i,loci,couponi) = x_fps(i) + x_market(i)
		END DO
		
		numeraire = mv(couponi)-DOT_PRODUCT(x_fps,pf) -DOT_PRODUCT(x_market,pm)- tm - tf -tcoupon
		util(loci,couponi) = DOT_PRODUCT(alpha,DLOG(x_fps*(1.0d0-faultyweight) +x_market + xi)) + numeraire


	END DO ! couponi

END SUBROUTINE SolveVoucherProblem