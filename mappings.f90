MODULE mappings
IMPLICIT NONE

	CONTAINS
	
	
	!!!!!!!!!
	SUBROUTINE get_ntheta (ntheta)
    USE globvar
	IMPLICIT NONE
	INTEGER, INTENT(OUT) :: ntheta
	
		ntheta = 0
		! alpha constant
		ntheta = ntheta + ncomm
		
		! alpha hhsize
		ntheta = ntheta + ncomm
		
		! produced wheat
		ntheta = ntheta + 1
        
		! gas connectnthetaon
		ntheta = ntheta + 1

		! lpg
		ntheta = ntheta + nlpg

		! distrust
		ntheta = ntheta + 1
		
		! fps open hrs
		ntheta = ntheta + nhrs

		! travel time intercept
		ntheta = ntheta + 4

		! travel time slope
		ntheta = ntheta + 4

		! mean of participation cost
		ntheta = ntheta + 1
		
		!std of participation cost
		ntheta = ntheta + 1
		
        !alpha (MU of consumption) variance
        ntheta = ntheta + ncomm
        
		!storage shock variance
		ntheta = ntheta + ncomm  
		!storage shock covariance
		ntheta = ntheta + 1 !wheat and sugar
		ntheta = ntheta + 1 !wheat and kero

        
		!std of classical measurement error
		ntheta = ntheta + ncomm

		!categorical error probability
		ntheta = ntheta + 1
		
	
	END SUBROUTINE get_ntheta
	
	!!!!!!!!!
	
	SUBROUTINE theta_to_param (ntheta,theta)
	USE globvar
	IMPLICIT NONE
	INTEGER, INTENT(IN) :: ntheta
	REAL(8), INTENT(IN) :: theta(ntheta) 
	INTEGER :: i, j, monthi, xii
    REAL(8) :: cvar(ncomm,ncomm)
	
		i=0
		
        i = i + 1
		param_alpha (1) = 900.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !wheat 1000
        i = i + 1
		param_alpha (2) = 120.0d0 * DEXP(theta(i))/(1+DEXP(theta(i)))   !sugar 200
        i = i + 1
		param_alpha (3) = 80.0d0 * DEXP(theta(i))/(1+DEXP(theta(i)))   !kerosene 200
		
        i = i + 1
		param_hhsize (1) = 150.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !wheat
        i = i + 1
		param_hhsize (2) = 90.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !sugar
        i = i + 1
		param_hhsize (3) = 30.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !kerosene
		
		i = i + 1
		param_wheat = 10.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !wheat production enters wheat consumption -> shifts xi mean for wheat
		i = i + 1
		param_gas = 2.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !shifts xi mean for kero
		
        DO j = 1, nlpg
    		i = i + 1
    		param_lpg (j) = 2.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) ! shifts xi mean for kero. no. lpg owned.
		END DO
		
		i = i + 1
		param_distrust = 0.5d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !faulty weights..
        
        DO j = 1, nhrs
            i = i + 1
    		param_hrs (j) = 4.0d0*DEXP(theta(i))/(1+DEXP(theta(i))) !FPS hours open per month less than 24hrs -> affects traveling cost
		END DO
		
		i = i + 1
		param_tf_const = theta(i) !fixed travel cost to fps
        i = i + 1
		param_tm_const = theta(i) !fixed travel cost to market
		i = i + 1
		param_te_const = theta(i) !fixed travel cost to emitra
		i = i + 1
		param_tb_const = theta(i) !fixed travel cost to bank
        
		i = i + 1
		param_tf = 4*DEXP(theta(i))/(1+DEXP(theta(i))) !walking distance to fps
		i = i + 1
		param_tm = DEXP(theta(i))/(1+DEXP(theta(i))) !walking distance to market
        i = i + 1
		param_te = -2*DEXP(theta(i))/(1+DEXP(theta(i))) !density of e-mitra
		i = i + 1
		param_tb = DEXP(theta(i))/(1+DEXP(theta(i))) !waking distance to bank
        
		i = i + 1
		!mufc = 10.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) ! mean of participation cost
        mufc = theta(i)
		i = i + 1
		sigfc = 15.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !std of participation cost
		
        DO j = 1, ncomm
            i = i + 1
            sigmu (j) = DEXP(theta(i))/(1+DEXP(theta(i))) !MU variance for wheat, sugar, kerosene
        END DO
         
        sigxi = 0.0d0
        i = i + 1
		sigxi (1,1) = (5.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))))**2.0d0 !storage shock variance for wheat
        i = i + 1
		sigxi (2,2) = (2.5d0 * DEXP(theta(i))/(1+DEXP(theta(i))))**2.0d0 !storage shock variance for sugar
        i = i + 1
		sigxi (3,3) = (2.5d0 * DEXP(theta(i))/(1+DEXP(theta(i))))**2.0d0 !storage shock variance for kerosene
        i = i + 1
		sigxi (1,2) = DEXP(theta(i))/(1+DEXP(theta(i))) * SQRT(sigxi(1,1)*sigxi(2,2)) !covar wheat,sugar storage shock
		sigxi (2,1) = sigxi (1,2)
        i = i + 1
		sigxi (1,3) = DEXP(theta(i))/(1+DEXP(theta(i))) * SQRT(sigxi(1,1)*sigxi(3,3)) !covar wheat,kero storage shock
		sigxi (3,1) = sigxi (1,3)
        

        
        ! classical measurement error
        i = i + 1
		sigeps (1) = 2.0d0 * DEXP(theta(i))/(1+DEXP(theta(i))) !std of measurement error for wheat
        i = i + 1
		sigeps (2) = DEXP(theta(i))/(1+DEXP(theta(i))) !std of measurement error for sugar
        i = i + 1
		sigeps (3) = DEXP(theta(i))/(1+DEXP(theta(i))) !std of measurement error for kerosene
        
		
		!categorical error probability
        i = i + 1
		pi = 0.5d0*DEXP(theta(i))/(1+DEXP(theta(i)))	!bound between 0 and 0.5
        
        ! combine read-in xi matrix (ximat0) and sigxi
        DO i = 1, ncomm
            mumat(i,:) = mumat0(i,:)*sigmu(i)
        END DO
        
        ! storage shock draw
        cvar = CHOL(sigxi)
        DO monthi = may, dec
        DO xii = 1, nxi
            ! transform independent normal variables to correlated variables
            ximat(:,monthi,xii) = MATMUL(cvar,ximat0(:,monthi,xii))
        END DO
        END DO

		
    END SUBROUTINE theta_to_param
!!!!!!!!!!
    
    FUNCTION CHOL(AIN)
IMPLICIT NONE
REAL(8), DIMENSION(:,:), INTENT(IN) :: AIN
REAL(8) :: A(SIZE(AIN,1),SIZE(AIN,2)),p(SIZE(AIN,1)),CHOL(SIZE(AIN,1),SIZE(AIN,2))
INTEGER :: i,j,n
REAL(8) :: summ
IF (SIZE(AIN,1).NE.SIZE(AIN,2)) THEN
    PAUSE 'FATAL ERROR: matrix is not square (CHOL)***'
    STOP
END IF    
A=AIN
CHOL=0.0d0
n=SIZE(A,1)
DO i=1,n
    summ=A(i,i)-DOT_PRODUCT(A(i,1:i-1),A(i,1:i-1))
    IF (summ <= 0.0) THEN
        PAUSE 'CHOL failed'
    END IF      
    p(i)=DSQRT(summ)
    A(i+1:n,i)=(A(i,i+1:n)-matmul(A(i+1:n,1:i-1),A(i,1:i-1)))/p(i)
    A(i,i)=p(i)
END DO
FORALL (i=1:n,j=1:n,j<=i) CHOL(i,j)=A(i,j)
END FUNCTION CHOL

!!!!!!!!!!
    SUBROUTINE get_param (indi,mui,alphai,ximean,faultyweight,pf,pm,tf,tm,tb,te)
    USE globvar
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: indi, mui
    REAL(8), INTENT(OUT) :: alphai(ncomm),ximean(ncomm),faultyweight,pf(ncomm),pm(ncomm),tf,tm,tb,te
    REAL(8) :: alpha(ncomm)
    INTEGER :: i
        ! The larger alpha is, the higher the demand for the commodity is
	    DO i = 1, ncomm
		    alpha(i) = param_alpha(i) + data_hhsize(indi)*param_hhsize(i)
        END DO
    
    
        alphai = alpha*DEXP(mumat(:,mui)) !household type. fixed over time.
	
        !IF ( mui ==1 ) alphai = alpha*DEXP(mumat(:,indi)) 
        ! The larger ximean is, the less the demand for the commodity is
	    ximean(1) = data_wheat(indi)*param_wheat
	    ximean(3) = DOT_PRODUCT(data_lpg(:,indi),param_lpg) + data_gas(indi)*param_gas
	
	    faultyweight = data_distrust(indi)*param_distrust
	
    
        pf = data_pf(:,indi)
        pm = data_pm(:,indi)
        
	    tf = DEXP(param_tf_const + data_tf(indi)*param_tf + DOT_PRODUCT(data_hrs(:,indi),param_hrs))
	    tm = DEXP(param_tm_const + data_tm(indi)*param_tm)
	    te = DEXP(param_te_const + data_te(indi)*param_te)
	    tb = DEXP(param_tb_const + data_tb(indi)*param_tb)
    
    
    END SUBROUTINE

END MODULE mappings