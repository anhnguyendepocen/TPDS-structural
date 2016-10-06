SUBROUTINE Simulation(theta,ntheta)
!Compute cash equivalent value of in-kind transfers for 1 month of switch to cash only and for permanent switch to cash only
!Compute welfare gain from having 5 e-Mitra kiosks every villages + switching to cash only permanently
!Compute welfare gain from correcting all scales used by FPS dealers and enforcing longer than 75 hours of FPS open hours.
USE globvar
USE mappings
IMPLICIT NONE
INTEGER, INTENT(IN) :: ntheta
REAL(8), INTENT(IN) :: theta(ntheta)

INTEGER :: indi, mui, xii, monthi, couponi, simi
REAL(8) :: alpha(ncomm),alphai(ncomm),ximean(ncomm),xi(ncomm),pf(ncomm),pm(ncomm),faultyweight,tf,tm,tb,te,mv,quota(ncomm), &
        util(nloc),x(ncomm,nloc),U0, U1, mv0, mv_hi, mv_lo, gap
REAL(8), PARAMETER :: tol = 1.0d-6
CHARACTER(*), PARAMETER :: fileplace = "D:\Dropbox\TPDS\Choice Experiment Paper\Output\Structural\"

! Map the paramters
CALL theta_to_param(ntheta,theta)

!---------------------------------------------
! Cash Equivalent Value of in-Kind Transfers
!=============================================
monthi = may
simi = 0

OPEN(1,FILE=fileplace//'CEV.txt')

DO indi = 1, nhh
    
	quota = quotamat (:,1,indi)
    mv0 = mvpds(indi)
    
    DO mui = 1, nmu
        
        simi = simi + 1 !count the number of simulated households
        
        
        CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,pm,tf,tm,tb,te)
        
        ! Initialize
        U0 = 0.0d0
        U1 = 0.0d0
	    mv_hi = 100.0d0 * mv0 ! initial guess
        mv_lo = 0.0d0
        mv = 0.5d0 * ( mv_lo + mv_hi )
        
        DO ! iterate over mv until we fine CEV
        
            DO xii = 1, nxi    
                        
		        xi = DEXP(ximean + ximat(:,monthi,xii)) !storage shock. varies over time.
            
		        CALL SolveNoVoucherProblem(alphai,xi,pf,pm,tf,tm,faultyweight,quota,x,util) !in a given month, given xi
                U0 = U0 + MAXVAL(util) 
            
                util = -1.0d+20 !some location choice options are not relavent under Cash-only 
		        CALL SolveCashOnlyProblem(alphai,xi,pm,tm,te,tb,mv,x,util) ! solve for the optimal solution 
	            U1 = U1 + MAXVAL(util)  
                
            END DO !xii
        
            U0 = U0 / nxi
            U1 = U1 / nxi
        
            gap = DABS(U0-U1)
        
            IF ( gap < U0*tol ) EXIT
        
            IF ( U0 > U1 ) THEN ! mv is too small
                mv_lo = mv
            ELSE IF ( U0 < U1 ) THEN !mv is too high
                mv_hi = mv
            END IF
        
            IF (mv_lo >= mv_hi ) THEN
                PRINT*, 'mv_lo is equal to or greater than mv_hi', indi, mui
            END IF
        
            IF ( DABS(mv_lo - mv_hi) < tol ) EXIT
            
            mv = 0.5d0 * ( mv_lo + mv_hi )
            
        END DO ! iterate over mv        
        
        WRITE(1,'(1f8.2)') mv - mv0
            
    END DO  !mui
    print *, 'Simul 1', indi
END DO  !indi

CLOSE(1)


!-----------------------------------------------
!Compute welfare gain from having 5 e-Mitra kiosks every villages + switching to cash only permanently
!Compute welfare gain from correcting all scales used by FPS dealers and enforcing longer than 75 hours of FPS open hours.
!===============================================
OPEN(1,FILE=fileplace//'CEV_5eMitra.txt')

DO indi = 1, nhh
    
	quota = quotamat (:,1,indi)
    mv0 = mvpds(indi)
    
    DO mui = 1, nmu        
        CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,pm,tf,tm,tb,te)
 
        ! CHANGE BY COUNTERFACTUAL POLICY    
	    te = DEXP(param_te_const + 5.0d0*param_te) !number of e-Mitra kiosks increases to 5
        
        ! Initialize
        U0 = 0.0d0
        U1 = 0.0d0
	    mv_hi = 100.0d0 * mv0 ! initial guess
        mv_lo = 0.0d0
        mv = 0.5d0 * ( mv_lo + mv_hi )
        
        DO ! iterate over mv until we fine CEV
        
            DO xii = 1, nxi    
                        
		        xi = DEXP(ximean + ximat(:,monthi,xii)) !storage shock. varies over time.
            
		        CALL SolveNoVoucherProblem(alphai,xi,pf,pm,tf,tm,faultyweight,quota,x,util) !in a given month, given xi
                U0 = U0 + MAXVAL(util) 
            
                util = -1.0d+20 !some location choice options are not relavent under Cash-only 
		        CALL SolveCashOnlyProblem(alphai,xi,pm,tm,te,tb,mv,x,util) ! solve for the optimal solution 
	            U1 = U1 + MAXVAL(util)  
                
            END DO !xii
        
            U0 = U0 / nxi
            U1 = U1 / nxi
        
            gap = DABS(U0-U1)
        
            IF ( gap < U0*tol ) EXIT
        
            IF ( U0 > U1 ) THEN ! mv is too small
                mv_lo = mv
            ELSE IF ( U0 < U1 ) THEN !mv is too high
                mv_hi = mv
            END IF
        
            IF (mv_lo >= mv_hi ) THEN
                PRINT*, 'mv_lo is equal to or greater than mv_hi', indi, mui
            END IF
        
            IF ( DABS(mv_lo - mv_hi) < tol ) EXIT
            
            mv = 0.5d0 * ( mv_lo + mv_hi )
            
        END DO ! iterate over mv        
        
        WRITE(1,'(1f8.2)') mv - mv0
            
    END DO  !mui
    print *, 'Simul 2', indi
END DO  !indi

CLOSE(1)
    
!===============================================
!Compute welfare gain from correcting all scales used by FPS dealers and enforcing longer than 75 hours of FPS open hours.
!===============================================
OPEN(2,FILE=fileplace//'CEV_betterFPS.txt')

DO indi = 1, nhh
    
	quota = quotamat (:,1,indi)
    mv0 = mvpds(indi)
    
    DO mui = 1, nmu
                
        CALL get_param (indi,mui,alphai,ximean,faultyweight,pf,pm,tf,tm,tb,te)
 
        ! CHANGE BY COUNTERFACTUAL POLICY    
	    faultyweight = 0.0d0 ! no dishonest behavior by FPS dealers
	    tf = DEXP(param_tf_const + data_tf(indi)*param_tf ) !longer FPS hours
        
        ! Initialize
        U0 = 0.0d0
        U1 = 0.0d0
	    mv_hi = 100.0d0 * mv0 ! initial guess
        mv_lo = 0.0d0
        mv = 0.5d0 * ( mv_lo + mv_hi )
        
        DO ! iterate over mv until we fine CEV
        
            DO xii = 1, nxi    
                        
		        xi = DEXP(ximean + ximat(:,monthi,xii)) !storage shock. varies over time.
            
		        CALL SolveNoVoucherProblem(alphai,xi,pf,pm,tf,tm,faultyweight,quota,x,util) !in a given month, given xi
                U0 = U0 + MAXVAL(util) 
            
                util = -1.0d+20 !some location choice options are not relavent under Cash-only 
		        CALL SolveCashOnlyProblem(alphai,xi,pm,tm,te,tb,mv,x,util) ! solve for the optimal solution 
	            U1 = U1 + MAXVAL(util)  
                
            END DO !xii
        
            U0 = U0 / nxi
            U1 = U1 / nxi
        
            gap = DABS(U0-U1)
        
            IF ( gap < U0*tol ) EXIT
        
            IF ( U0 > U1 ) THEN ! mv is too small
                mv_lo = mv
            ELSE IF ( U0 < U1 ) THEN !mv is too high
                mv_hi = mv
            END IF
        
            IF (mv_lo >= mv_hi ) THEN
                PRINT*, 'mv_lo is equal to or greater than mv_hi', indi, mui
            END IF
        
            IF ( DABS(mv_lo - mv_hi) < tol ) EXIT
            
            mv = 0.5d0 * ( mv_lo + mv_hi )
            
        END DO ! iterate over mv        
        
        WRITE(2,'(1f8.2)') mv - mv0
            
    END DO  !mui
    print *, 'Simul 3', indi
END DO  !indi

CLOSE(2)
    
    

END SUBROUTINE Simulation