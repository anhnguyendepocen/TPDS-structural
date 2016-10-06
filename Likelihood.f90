REAL(8) FUNCTION Likelihood(theta,ntheta)
USE globvar
USE mappings
USE csv_file
IMPLICIT NONE
INTEGER, INTENT(IN) :: ntheta
REAL(8), INTENT(IN) :: theta(ntheta)
INTEGER :: indi, monthi,mui,i
REAL(8) :: participationprobi, probi, prob_out, prob_mui, lf
CHARACTER(*), PARAMETER :: fileplace = "D:\Dropbox\TPDS\Choice Experiment Paper\Output\Structural\"

CALL theta_to_param(ntheta,theta)

simulCoupon = 0
simulPart = 0.0d0
lf = 0.0d0

U1_te=0.0d0
U0_te=0.0d0

DO indi = 1, nhh
    
    probi = 0.0d0
    
    DO mui = 1, nmu

        prob_mui = 1.0d0
        
	    IF (treatment (indi)==1) THEN !Treatment households
	
		    CALL SolveParticipationProblem (indi,mui,prob_out)
        
            prob_mui = prob_mui * prob_out
            simulPart(indi) = simulPart(indi) + prob_out
	
		    !IF (participate(indi)==1) THEN
            IF (prob_out>0.0d0) THEN
			    CALL LikelihoodVoucher (indi,mui,prob_out)
                prob_mui = prob_mui * prob_out
			
		    ELSE
		
			    CALL LikelihoodNoVoucher (indi,mui,prob_out)
                prob_mui = prob_mui * prob_out
		    END IF
		
	    ELSE !Control households
	
		    CALL LikelihoodNoVoucher (indi,mui,prob_out)
            prob_mui = prob_mui * prob_out
		
        END IF
    
        !print '(I6,3f)', indi, probi, participationprobi, lf
        probi = probi + prob_mui
        
        EXIT
    END DO !mui

	lf = lf + DLOG(probi/nmu)
        

END DO	! indi

U1_te = U1_te/nhh_te
U0_te = U0_te/nhh_te

do i=1, 6
    PRINT *, U1_te(i), U0_te(i)
end do 

Likelihood = -lf

!simulPart = simulPart / nmu

IF ( estimation == .false. ) THEN

    OPEN(1,FILE=fileplace//'simulPart.txt')
        DO indi = 1, nhh
            WRITE(1,'(1f)') simulPart(indi)
        END DO
    CLOSE(1)

    OPEN(1,FILE=fileplace//'simulLoc.txt')
        DO indi = 1, nhh
        DO monthi = july,sept
            WRITE(1,'(1I)') simulLoc(indi,monthi)
        END DO
        END DO
    CLOSE(1)
    

    OPEN(1,FILE=fileplace//'simulComm.txt')
        DO indi = 1, nhh
        DO monthi = july,sept
            call csv_write_real_1d(1,simulComm(:,indi,monthi),.true.) 
        END DO
        END DO
    CLOSE(1)    


    OPEN(1,FILE=fileplace//'simulCoupon.txt')
        DO indi = 1, nhh
        DO monthi = may,dec
            WRITE(1,'(1I)') simulCoupon(indi,monthi) 
        END DO
        END DO
    CLOSE(1)

END IF


END FUNCTION Likelihood