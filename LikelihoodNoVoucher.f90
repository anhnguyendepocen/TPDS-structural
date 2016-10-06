SUBROUTINE LikelihoodNoVoucher (indi,mui,probi)
USE globvar
USE probability
USE mappings

IMPLICIT NONE
INTEGER, INTENT(IN) :: indi, mui
REAL(8), INTENT(OUT) :: probi
REAL(8) :: alpha(ncomm),alphai(ncomm),ximean(ncomm),xi(ncomm),pf(ncomm),logpm(ncomm),pm(ncomm),faultyweight,tf,tm,tb,te,quota(ncomm),x(ncomm,nloc), &
            util(nloc),opt_x(ncomm),obs_x(ncomm),prob_x(ncomm), prob_xi
INTEGER :: opt_loc(1), obs_loc, i, monthi, loci, xii, pii
REAL(8), PARAMETER :: smallNumber = 1.0d-15
! Compute the likeilhood value without the voucher

    CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,logpm,tf,tm,tb,te)
    
	quota = quotamat (:,1,indi)
    
	probi = 1.0d0
	
	DO monthi = july,sept
				
		obs_loc = locchoice (indi,monthi)
		obs_x  = commchoice (:,indi,monthi) !observed quantity purchased
        
        prob_xi = 0.0d0
        
		DO xii = 1, nxi
            
		    xi = DEXP(ximean + ximat(:,monthi,xii)) !storage shock. varies over time.
            pm = DEXP(logpm + pmat(:,monthi,xii))
            IF ( pii==1 .AND. xii == 1 .AND. mui==1) THEN
                xi = DEXP(ximean + ximat(:,monthi,indi))
                pm = DEXP(logpm + pmat(:,monthi,indi))
            END IF
            
		    CALL SolveNoVoucherProblem(alphai,xi,pf,pm,tf,tm,faultyweight,quota,x,util) !in a given month, given xi
	
		    !optimal choice
		    opt_loc = MAXLOC(util)
		    opt_x = x(:,opt_loc(1)) !optimal quantity purchased
        
            IF ( xii == 1 ) THEN
                !simulated choices
                simulComm(:,indi,monthi) = opt_x
                simulLoc(indi,monthi) = opt_loc(1)
            END IF            
        
            IF (obs_loc == opt_loc(1)) THEN
                        
                ! probability of observing the reported quantity 
		        DO i = 1, ncomm
			        IF ( obs_x(i) > 0.0d0 .AND. opt_x(i) > 0.0d0 ) THEN  !no categorical error
				        prob_x(i) = normpdf((DLOG(obs_x(i))-DLOG(opt_x(i))),0.0d0,sigeps(i))*(1.0d0-pi)
			        ELSE IF ( obs_x(i)==0.0d0 .AND. opt_x(i)==0.0d0) THEN  !no categorical error
				        prob_x(i) = 1.0d0-pi
			        ELSE !categorical error
				        prob_x(i) = pi
			        END IF
		        END DO
				        
		        ! likelihood in the given month
		        prob_xi = prob_xi + MAX(smallNumber,PRODUCT(prob_x))
            ELSE
                prob_xi = prob_xi + smallNumber
            END IF
            
            EXIT
            
        END DO ! xii    
        
        probi = probi * prob_xi
        
    END DO	!monthi

	
END SUBROUTINE