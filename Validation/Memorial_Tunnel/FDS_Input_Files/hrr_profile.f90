PROGRAM HRR_PROFILE

! Read experimental HRR file and produce RAMP lines

CHARACTER(50) :: EXP_FILE

HRR_0 = 50000.
EXP_FILE = '../../../../exp/Memorial_Tunnel/HRR-611.csv'
OPEN(10,FILE=EXP_FILE,FORM='FORMATTED')

READ(10,*)
READ(10,*)
DO 
   READ(10,*,END=20) T,HRR
   WRITE(6,'(A,F5.0,A,F6.3,A)') "&RAMP ID='RAMP_FIRE', T=",T,", F=",HRR/HRR_0," /"
ENDDO

20 CONTINUE

END PROGRAM HRR_PROFILE
