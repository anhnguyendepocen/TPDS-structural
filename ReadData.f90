SUBROUTINE ReadData
USE globvar
IMPLICIT NONE

INTEGER :: indi, monthi, xii, pii
CHARACTER(*), PARAMETER :: fileplace = "D:\Dropbox\TPDS\Choice Experiment Paper\Data\Matrices\Rajasthan\"

! Number of households
OPEN(1,FILE=fileplace//'nhh.txt')
	READ(1,*) nhh
CLOSE(1)


ALLOCATE(data_tf(nhh), data_tm(nhh), data_te(nhh), data_tb(nhh), data_distrust(nhh), data_hrs(nhrs,nhh), &
		data_pf(ncomm,nhh), data_pm(ncomm,nhh), quotamat(ncomm,ncoupon,nhh), mvmat(ncoupon,nhh), &
		mvpds(nhh), data_hhsize(nhh), data_wheat(nhh), data_lpg(nlpg,nhh), data_gas(nhh), &
		couponchoice(nhh,may:dec), commchoice(ncomm,nhh,july:sept), locchoice(nhh,july:sept), &
		treatment(nhh), participate(nhh), eligibility(ncoupon,nhh),mumat(ncomm,nmu),mumat0(ncomm,nmu))

ALLOCATE (simulLoc(nhh,july:sept), simulCoupon(nhh,may:dec),simulComm(ncomm,nhh,july:sept),simulPart(nhh))

! Distance
OPEN(1,FILE=fileplace//'normfpsdistance.txt')
	READ(1,*) data_tf(:)
CLOSE(1)

OPEN(1,FILE=fileplace//'normmarketdistance.txt')
	READ(1,*) data_tm(:)
CLOSE(1)

OPEN(1,FILE=fileplace//'nemitrakiosks.txt')
	READ(1,*) data_te(:)
CLOSE(1)

OPEN(1,FILE=fileplace//'normbankdistance.txt')
	READ(1,*) data_tb(:)
CLOSE(1)

! FPS experience
OPEN(1,FILE=fileplace//'distrust.txt')
	READ(1,*) data_distrust(:)
CLOSE(1)

OPEN(1,FILE=fileplace//'hrsopen.txt')
	DO indi = 1, nhh
		READ(1,*) data_hrs(:,indi)
	END DO
CLOSE(1)

! Unit price
OPEN(1,FILE=fileplace//'fpsprice.txt')
	DO indi = 1, nhh
		READ(1,*) data_pf(:,indi)
	END DO
CLOSE(1)

OPEN(1,FILE=fileplace//'marketprice.txt')
	DO indi = 1, nhh
		READ(1,*) data_pm(:,indi)
	END DO
CLOSE(1)
data_pm = DLOG(data_pm)

! Quota
OPEN(1,FILE=fileplace//'quota1.txt')
	DO indi = 1, nhh
		READ(1,*) quotamat(1,:,indi)
	END DO
CLOSE(1)

OPEN(1,FILE=fileplace//'quota2.txt')
	DO indi = 1, nhh
		READ(1,*) quotamat(2,:,indi)
	END DO
CLOSE(1)

OPEN(1,FILE=fileplace//'quota3.txt')
	DO indi = 1, nhh
		READ(1,*) quotamat(3,:,indi)
	END DO
CLOSE(1)

! Money value of coupons
OPEN(1,FILE=fileplace//'mv.txt')
	DO indi = 1, nhh
		READ(1,*) mvmat(:,indi)
	END DO
CLOSE(1)

! Money value of ALL in-kind transfers
OPEN(1,FILE=fileplace//'mvpds.txt')
	READ(1,*) mvpds(:)
CLOSE(1)


! Household size
OPEN(1,FILE=fileplace//'hhsize.txt')
	READ(1,*) data_hhsize(:)
CLOSE(1)

! Household wheat production
OPEN(1,FILE=fileplace//'hh_produced_wheat.txt')
	READ(1,*) data_wheat(:)
CLOSE(1)

! Number of LPGs owned
OPEN(1,FILE=fileplace//'dlpg.txt')
	DO indi = 1, nhh
		READ(1,*) data_lpg(:,indi)
	END DO
CLOSE(1)

! Gas connection 
OPEN(1,FILE=fileplace//'gasconnection.txt')
	READ(1,*) data_gas(:)
CLOSE(1)

! Quantity purchased
OPEN(1,FILE=fileplace//'commchoice.txt')
	DO indi = 1, nhh
	DO monthi = july, sept
		READ(1,*) commchoice(:,indi,monthi)
	END DO
	END DO
CLOSE(1)


! Location choice
OPEN(1,FILE=fileplace//'locchoice2.txt')
	DO indi = 1, nhh
	DO monthi = july, sept
		READ(1,*) locchoice(indi,monthi)
	END DO
	END DO
CLOSE(1)


! Coupon choice
OPEN(1,FILE=fileplace//'couponchoice2.txt')
	DO indi = 1, nhh
	DO monthi = may, dec
		READ(1,*) couponchoice(indi,monthi)
	END DO
	END DO
CLOSE(1)


! Treatment 
OPEN(1,FILE=fileplace//'treatment.txt')
	READ(1,*) treatment(:)
CLOSE(1)

! Participate
OPEN(1,FILE=fileplace//'participate.txt')
	READ(1,*) participate(:)
CLOSE(1)

! Coupon eligibility
OPEN(1,FILE=fileplace//'eligibility.txt')
    DO indi = 1, nhh
    	READ(1,*) eligibility(:,indi)
    END DO
CLOSE(1)

! Xi
OPEN(1,FILE=fileplace//'xi.txt')
    DO xii = 1, nxi
    DO monthi = may, dec
    	READ(1,*) ximat0(:,monthi,xii)
    END DO
    END DO
CLOSE(1)

! Mu
OPEN(1,FILE=fileplace//'mu.txt')
    DO xii = 1, nxi
    	READ(1,*) mumat0(:,xii)
    END DO
CLOSE(1)

! up
OPEN(1,FILE=fileplace//'pmat.txt')
    DO pii = 1, np
    DO monthi = may, dec
    	READ(1,*) pmat0(:,monthi,pii)
    END DO
    END DO
CLOSE(1)

END SUBROUTINE ReadData