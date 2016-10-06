MODULE globvar
IMPLICIT NONE

INTEGER, PARAMETER :: ncomm=3, nmonth=8, nloc=4, ncoupon=8, nmu=450, nxi=450, nhrs=3, nlpg=2, may=5, july=7, sept=9, dec=12
INTEGER :: nhh

REAL(8), PARAMETER :: discount = 0.99d0

REAL(8) :: param_alpha(ncomm), param_hhsize(ncomm), param_wheat, param_lpg(nlpg), param_gas, param_tf_const, param_tm_const, param_te_const, param_tb_const, &
			param_tf, param_tm, param_te, param_tb, param_hrs(nhrs), param_distrust, sigmu(ncomm), sigxi(ncomm,ncomm), sigp(ncomm,ncomm), &
            mufc, sigfc, pi,sigeps(ncomm)
REAL(8) :: ximat(ncomm,may:dec,nxi),ximat0(ncomm,may:dec,nxi)

REAL(8), ALLOCATABLE :: data_tf(:), data_tm(:), data_tb(:), data_te(:), data_pf(:,:), data_pm(:,:), &
						 quotamat(:,:,:), mvmat(:,:), mvpds(:), commchoice(:,:,:),mumat(:,:),mumat0(:,:)
						
INTEGER, ALLOCATABLE :: locchoice(:,:), couponchoice(:,:), treatment(:), participate(:), eligibility(:,:), &
						data_hhsize(:), data_gas(:), data_lpg(:,:), data_wheat(:), data_distrust(:), data_hrs(:,:)

!! for simulation
REAL, ALLOCATABLE :: simulPart(:), simulComm(:,:,:)
INTEGER, ALLOCATABLE :: simulLoc(:,:), simulCoupon(:,:)

LOGICAL :: hopspack, estimation

REAL(8) :: U0_te(6), U1_te(6)
INTEGER :: nhh_te(6)

END MODULE globvar