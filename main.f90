PROGRAM main
USE globvar
USE mappings
USE minimization
IMPLICIT NONE

INTEGER :: ntheta, i, xii, dim_output, numberparameters
REAL(8), ALLOCATABLE :: theta(:)
REAL(8) :: objective
CHARACTER(LEN=20) :: inputfilename, outputfilename, ufo
REAL(8), PARAMETER :: gtol = 1.0d-6
REAL(8) :: Likelihood
    hopspack =.false.
    estimation = .false.

	CALL get_ntheta(ntheta)
	
	ALLOCATE(theta(ntheta))
    
    IF ( hopspack == .TRUE. ) THEN

        !----------------
        ! hopspack input
        !================
        !READ-in name of input file
        CALL getarg(1, inputfilename)  
    
        !READ-in name of output file
        CALL getarg(2, outputfilename)  
    
        !Read parameters
        OPEN(1, file = inputfilename, access='sequential', form='formatted',status='old') 
            READ(1,*) ufo
            READ(1,*) numberparameters
            DO i=1, numberparameters
                READ(1,*) theta(i)
            END DO
        CLOSE(1)
    
        IF ( ntheta .ne. numberparameters ) THEN
            print *, 'ntheta .ne. numberparameters'
            print *, 'ntheta', ntheta
            print *, 'numberparam', numberparameters
            pause
        END IF

    ELSE 

        ! READ parameters
	    OPEN(1,FILE='initialguess.txt')
		    DO i = 1, ntheta
			    READ(1,*) theta(i)
		    END DO
	    CLOSE(1)

    END IF
	
	
	
	CALL ReadData
    
    	
    
   IF ( hopspack ==.TRUE. ) THEN
       
        objective = Likelihood(theta,ntheta)
        !-----------------
        ! hopspack output
        !=================
        dim_output = 1

        OPEN (unit = 1, file = outputfilename)  
	        WRITE(1,*) dim_output
	        WRITE(1,*) objective
        CLOSE(1)    
        
        print *, objective
   ELSE
    
        IF (estimation==.true.) THEN
            CALL dfpmin(theta,ntheta,gtol,Likelihood)	
        ELSE
            objective = Likelihood(theta,ntheta)
            print *, objective
            !CALL Simulation(theta,ntheta)
        END IF
        
        PAUSE
    END IF
    
END PROGRAM main