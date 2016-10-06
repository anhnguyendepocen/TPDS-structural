SUBROUTINE LikelihoodVoucher (indi,mui,probi)
USE globvar
USE probability
USE mappings

IMPLICIT NONE
INTEGER, INTENT(IN) :: indi,mui
REAL(8), INTENT(OUT) :: probi
REAL(8) :: alpha(ncomm),alphai(ncomm),ximean(ncomm),xi(ncomm),faultyweight,pf(ncomm),logpm(ncomm),pm(ncomm),tf,tm,tb,te,mv(ncoupon),quota(ncomm,ncoupon),x(ncomm,nloc,ncoupon),util(nloc,ncoupon)
INTEGER :: ind(2), opt_loc, opt_coupon, obs_loc, obs_coupon, i, monthi,  couponi, loci, xii
REAL(8) :: obs_x(ncomm), opt_x(ncomm), prob_x(ncomm), prob_xi
REAL(8), PARAMETER :: smallNumber = 1.0d-15

! Compute the likeilhood value without the voucher
    CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,logpm,tf,tm,tb,te)
    
	quota = quotamat (:,:,indi)
	mv = mvmat(:,indi)
    
	probi = 1.0d0
	
	DO monthi = may, dec
        
	    prob_xi = 0.0d0
    
        DO xii = 1, nxi    	
        
		    xi = DEXP(ximean + ximat(:,monthi,xii))
            pm = DEXP(logpm + pmat(:,monthi,xii))
            
            IF ( xii == 1 .AND. mui==1) THEN
                xi = DEXP(ximean + ximat(:,monthi,indi))
                pm = DEXP(logpm + pmat(:,monthi,indi))
            END IF
            
		    CALL SolveVoucherProblem(alphai,xi,pf,pm,tf,tm,te,tb,faultyweight,quota,mv,x,util) ! solve for the optimal solution 
        
            !punish ineligible options
            DO couponi = 1, ncoupon
                IF ( eligibility (couponi,indi)==0 ) util(:,couponi) = -1.0d+20
            END DO
        
                
		    !optimal choice
		    ind = MAXLOC(util)
		    opt_loc = ind(1)
		    opt_coupon = ind(2)
		    opt_x = x(:,opt_loc,opt_coupon) !optimal quantity purchased
 

DO couponi = 1, ncoupon
    PRINT '(2I4,f6.2,I4,<nloc>f12.4)', indi, couponi, faultyweight, eligibility (couponi,indi), util(:,couponi)
END DO
PAUSE
            IF ( mui==1 .AND. xii==1) simulCoupon(indi,monthi) = opt_coupon
        
            ! observed choice
            obs_coupon = couponchoice(indi,monthi)
			
		    IF ( monthi>=july .AND. monthi<=sept  ) THEN	!has both coupon choice and purchase data
            
                IF ( mui==1 .AND. xii == 1 ) THEN
                    !simulated choices
                    simulComm(:,indi,monthi) = opt_x
                    simulLoc(indi,monthi) = opt_loc
                END IF            
				
		        obs_loc = locchoice (indi,monthi)
			    obs_x  = commchoice (:,indi,monthi) !observed quantity purchased
			
                IF ( opt_loc==obs_loc .AND. opt_coupon==obs_coupon) THEN
                
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
			        prob_xi = prob_xi + MAX(1.0d-15,PRODUCT(prob_x))
                
                ELSE
                    prob_xi = prob_xi + smallNumber
                END IF
            
		    ELSE ! coupon choice only
            
                IF ( opt_coupon==obs_coupon ) THEN	
                    prob_xi = prob_xi + 1.0d0
                ELSE
                    prob_xi = prob_xi + smallNumber
                END IF  
			
            END IF	!purchase data available?
            
            EXIT
        END DO  !xi	
    
        probi = probi * (prob_xi / nxi)
    
    END DO	!monthi

    
END SUBROUTINE LikelihoodVoucher
