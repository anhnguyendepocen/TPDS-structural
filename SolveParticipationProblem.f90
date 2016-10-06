SUBROUTINE SolveParticipationProblem (indi,mui,participationprobi)
USE globvar
USE probability
USE mappings
IMPLICIT NONE
INTEGER, INTENT(IN) :: indi, mui
REAL(8), INTENT(OUT) :: participationprobi
INTEGER :: i, opt_loc(1), ind(2), monthi, xii, tei, couponi
REAL(8) :: alpha(ncomm),alphai(ncomm),ximean(ncomm),xi(ncomm),pf(ncomm),logpm(ncomm),pm(ncomm),faultyweight,tf,tm,tb,te,mv(ncoupon),quota(ncomm,ncoupon), &
        x0(ncomm,nloc),util0(nloc),x1(ncomm,nloc,ncoupon),util1(nloc,ncoupon), U0, U1, V0, V1

    CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,logpm,tf,tm,tb,te)
	
	mv = mvmat(:,indi)
	quota = quotamat (:,:,indi)
    
    
      
    IF ( data_te(indi) <0.1d0) THEN
        tei = 1
    ELSE IF ( data_te(indi) >0.6d0 .AND. data_te(indi) <0.7d0) THEN
        tei = 2
    ELSE IF ( data_te(indi) >0.8d0 .AND. data_te(indi) <2.0d0) THEN
        tei = 3
    ELSE IF ( data_te(indi) >2.0d0 .AND. data_te(indi) <2.7d0) THEN
        tei = 4
    ELSE IF ( data_te(indi) >2.7d0 .AND. data_te(indi) <4.0d0) THEN
        tei = 5
    ELSE IF ( data_te(indi) >4.0d0) THEN
        tei = 6
    END IF
	
	U0 = 0.0d0
	U1 = 0.0d0
    
	DO monthi = may, dec
    DO xii = 1, nxi
	    xi = DEXP(ximean + ximat(:,monthi,xii))
        pm = DEXP(logpm + pmat(:,monthi,xii))
	    ! Not participate
	    CALL SolveNoVoucherProblem (alphai,xi,pf,pm,tf,tm,faultyweight,quota(:,1),x0,util0)
	    U0 = U0 + discount**(monthi-may)*MAXVAL(util0) 

	    ! Participate
	    CALL SolveVoucherProblem (alphai,xi,pf,pm,tf,tm,te,tb,faultyweight,quota,mv,x1,util1)
        !punish ineligible options
        DO couponi = 1, ncoupon
            IF ( eligibility (couponi,indi)==0 ) util1(:,couponi) = -1.0d+20
        END DO
        
	    U1 = U1 + discount**(monthi-may)*MAXVAL(util1)  
        EXIT
    END DO !xii	
    END DO !monthi
 !   U0 = U0 / nxi
	!U1 = U1 / nxi
 !   
	!V0 = 1.0d0 / (1.0d0 - discount) * U0 !expected lifetime utility from not participating in the experiment
	!
	!
	!V1 = 0.0d0
	!
 !   i = 0
	!DO monthi = may, dec
 !       i = i + 1
	!	V1 = V1 + (discount**(i*1.0d0))*U1 
	!END DO
	!
	!V1 = V1 + (discount**(nmonth*1.0d0+1.0d0))*V0  !expected utility from participating in the experiment for nmonth
	!
 !   V0 = discount * V0
    
	! Probability to participate: V1-fc>V0.. fc<V1-V0
	!participationprobi = normcdf(log(max(1.0d-10,V1-V0)),mufc,sigfc)
	!participationprobi = normcdf(log(max(1.0d-10,U1-U0)),mufc,sigfc)	
    
    IF ( U1*mufc>U0) THEN
        participationprobi = 1.0d0
    ELSE
        participationprobi = 0.0d0
    END IF
U0_te(tei) = U0_te(tei)+ U0
U1_te(tei) = U1_te(tei)+ U1
nhh_te(tei) = nhh_te(tei)+1
	
END SUBROUTINE SolveParticipationProblem