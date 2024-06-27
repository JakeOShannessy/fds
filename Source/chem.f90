! CALL CVODE FOR SOLVING CHEMISTRY 
! THE DERIVATIVE AND JACOBIAN FUNCTION TRACK N_TRACKED_SPECIES+2
! VARIABLES. THE +2 IS TEMPERATURE AND PRESSURE.


#ifdef WITH_SUNDIALS

MODULE CVODE_INTERFACE

USE PRECISION_PARAMETERS
USE GLOBAL_CONSTANTS   
USE TYPES
USE CHEMCONS
USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

LOGICAL :: DEBUG=.FALSE.
REAL(EB), PARAMETER :: MIN_CHEM_TMP=200._EB

PUBLIC CVODE_SERIAL

CONTAINS

  
!> \brief The right hand side function of the ode d[c]/dt = wdot (=f).
!> \param TN_C is the current time
!> \param SUNVEC_Y is the current array of molar concentrations, temperature and pressure.
!> \param SUNVEC_F is the array of derivatives returned

INTEGER(C_INT) FUNCTION RHSFN(TN_C, SUNVEC_Y, SUNVEC_F, USER_DATA) &
    RESULT(IERR) BIND(C,NAME='RHSFN')

USE, INTRINSIC :: ISO_C_BINDING
USE FSUNDIALS_NVECTOR_MOD

! CALLING VARIABLES
REAL(C_DOUBLE), VALUE :: TN_C      ! CURRENT TIME
TYPE(N_VECTOR)        :: SUNVEC_Y  ! SOLUTION N_VECTOR
TYPE(N_VECTOR)        :: SUNVEC_F  ! RHS N_VECTOR
TYPE(C_PTR),    VALUE :: USER_DATA ! USER-DEFINED DATA

! POINTERS TO DATA IN SUNDIALS VECTORS
REAL(EB), POINTER :: CVEC(:)
REAL(EB), POINTER :: FVEC(:)

! TO PREVENT UNUSED VARIABLE WARNING
IF (DEBUG) THEN
   WRITE(LU_ERR,*)"TIME = ",TN_C
   IF (.NOT. C_ASSOCIATED(USER_DATA)) WRITE(LU_ERR,*)" NO USER_DATA IS PROVIDED"
ENDIF

! GET DATA ARRAYS FROM SUNDIALS VECTORS
CVEC => FN_VGETARRAYPOINTER(SUNVEC_Y)
FVEC => FN_VGETARRAYPOINTER(SUNVEC_F)

FVEC = 0._EB
CALL DERIVATIVE(CVEC, FVEC)


IERR = 0
RETURN

END FUNCTION RHSFN



!> \brief Calculate derivative (fvec) for a given cvec(n_tracked_species+2)
!> \param CVEC is the current array of molar concentrations, temperature and pressure.
!> \param FVEC is the array of derivatives returned

SUBROUTINE DERIVATIVE(CVEC,FVEC)
USE PHYSICAL_FUNCTIONS, ONLY : GET_SPECIFIC_HEAT_INTERP, GET_ENTHALPY_Z, MOLAR_CONC_TO_MASS_FRAC

REAL(EB), POINTER, INTENT(INOUT)  :: CVEC(:)
REAL(EB), POINTER, INTENT(INOUT) :: FVEC(:)
REAL(EB) :: R_F,MIN_SPEC(N_TRACKED_SPECIES), KG,  TMP, RHO, &
          K_0, K_INF, P_RI, FCENT, C_I, RRTMP, THIRD_BODY_ENHANCEMENT
INTEGER :: I,NS, ITMP
REAL(EB) :: ZZ(N_TRACKED_SPECIES), CP, HS_I, DG 
TYPE(REACTION_TYPE), POINTER :: RN=>NULL()

TMP = MAX(CVEC(N_TRACKED_SPECIES+1), MIN_CHEM_TMP)
PR = CVEC(N_TRACKED_SPECIES+2) ! PA
RRTMP = 1._EB/(R0*TMP)

RHO = DOT_PRODUCT(SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%MW,CVEC(1:N_TRACKED_SPECIES))
MIN_SPEC = SPECIES_MIXTURE%ODE_REL_ERROR*ZZ_MIN_GLOBAL

REACTION_LOOP: DO I=1,N_REACTIONS
   RN => REACTION(I)
   ! CHECK FOR CONSUMED SPECIES
   DO NS=1,RN%N_SMIX_FR
      IF (RN%NU_MW_O_MW_F_FR(NS) < 0._EB .AND. CVEC(RN%NU_INDEX(NS)) < MIN_SPEC(NS)) CYCLE REACTION_LOOP
   ENDDO

   ! CALCULATE FORWARD RATE COEFF.
   K_INF = RN%A_SI*TMP**RN%N_T*EXP(-RN%E*RRTMP) !A_SI IS IN [KMOL/M3]^() UNIT.
   R_F = K_INF
   IF(RN%REVERSE) THEN ! COMPUTE EQUILIBRIUM CONSTANT
      ITMP = MIN(I_MAX_TEMP-1,INT(TMP))
      DG = RN%DELTA_G(ITMP) + (TMP-REAL(ITMP,EB))*(RN%DELTA_G(ITMP+1)-RN%DELTA_G(ITMP))
      KG = EXP(DG/TMP)*(P_STP*RRTMP)**RN%C0_EXP
      !KG = EXP(RN%DELTA_G(MIN(I_MAX_TEMP,NINT(TMP)))/TMP)*(P_STP*RRTMP)**RN%C0_EXP
      R_F = R_F*KG ! KG IS THE CONCENTRATION EQUILIBRIUM CONSTANT OF REVERSE REACTION. 
   ENDIF

   ! MULTIPLY WITH MOLAR_CONCENTRATION ^ STOICHIOMETRIC_COEFF
   DO NS=1,RN%N_SPEC
      IF (CVEC(YP2ZZ(RN%N_S_INDEX(NS))) < MIN_SPEC(YP2ZZ(RN%N_S_INDEX(NS)))) CYCLE REACTION_LOOP
      R_F = R_F*(CVEC(YP2ZZ(RN%N_S_INDEX(NS))))**RN%N_S(NS) 
   ENDDO

   ! CALCULATE CI BASED ON TYPE OF REACTION
   IF (RN%THIRD_BODY) THEN

      IF (RN%N_THIRD > 0) THEN
         THIRD_BODY_ENHANCEMENT = DOT_PRODUCT(CVEC(1:N_SPECIES),RN%THIRD_EFF(1:N_SPECIES))
      ELSE
         THIRD_BODY_ENHANCEMENT = PR*RRTMP
      ENDIF

      IF (RN%REACTYPE==THREE_BODY_ARRHENIUS_TYPE) THEN
         R_F = R_F * THIRD_BODY_ENHANCEMENT
      ELSEIF(RN%REACTYPE==FALLOFF_LINDEMANN_TYPE .OR. RN%REACTYPE==FALLOFF_TROE_TYPE ) THEN
         K_0 = RN%A_LOW_PR*TMP**(RN%N_T_LOW_PR)*EXP(-RN%E_LOW_PR*RRTMP)
         P_RI = K_0/K_INF*THIRD_BODY_ENHANCEMENT
         FCENT = CALCFCENT(TMP,P_RI, RN)
         C_I = P_RI/(1._EB+P_RI)*FCENT
         R_F = R_F * C_I
      ENDIF 
   ENDIF

   IF (DEBUG) WRITE(LU_ERR,*)"RN%ID, RATE-CONTRIB=", RN%ID, R_F

   ! ADD CONTRIBUTION OF THE REACTION TO THE FVEC
   DO NS=1,RN%N_SMIX_FR
     FVEC(RN%NU_INDEX(NS)) = FVEC(RN%NU_INDEX(NS)) + RN%NU_NN(RN%NU_INDEX(NS)) * R_F
   ENDDO

ENDDO REACTION_LOOP

! CALCULATE TEMPERATURE DERIVATIVE
CALL MOLAR_CONC_TO_MASS_FRAC(CVEC(1:N_TRACKED_SPECIES), ZZ(1:N_TRACKED_SPECIES))
CALL GET_SPECIFIC_HEAT_INTERP(ZZ(1:N_TRACKED_SPECIES),CP,TMP)
DO NS=1,N_TRACKED_SPECIES
   CALL GET_ENTHALPY_Z(NS, TMP,HS_I)
   HS_I = HS_I * SPECIES_MIXTURE(NS)%MW ! To convert J/kg to J/kmol (absolute enthalpy includes entahlpy of formation)
   FVEC(N_TRACKED_SPECIES+1)= FVEC(N_TRACKED_SPECIES+1) + HS_I*FVEC(NS)
ENDDO
FVEC(N_TRACKED_SPECIES+1)=-FVEC(N_TRACKED_SPECIES+1)/RHO/CP

 
! PRESSURE DERIVATIVE (CONSTANT PRESSURE ASSUMPTION)
FVEC(N_TRACKED_SPECIES+2) = 0._EB


END SUBROUTINE DERIVATIVE


!> \brief Calculate fall-off function 
!> \param TMP is the current temperature.
!> \param P_RI is the reduced pressure
!> \param RN is the reaction

REAL(EB) FUNCTION CALCFCENT(TMP, P_RI, RN)
REAL(EB), INTENT(IN) :: TMP, P_RI
TYPE(REACTION_TYPE), POINTER, INTENT(IN) :: RN
REAL(EB) :: LOGFCENT, C, N, LOGPRC
REAL(EB), PARAMETER :: D=0.14_EB

IF(RN%REACTYPE==FALLOFF_TROE_TYPE) THEN
   IF (RN%T2_TROE <-1.E20_EB) THEN
      LOGFCENT = LOG10(MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
                 RN%A_TROE*EXP(-TMP*RN%RT1_TROE),TWO_EPSILON_EB))
   ELSE
      LOGFCENT = LOG10(MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
                 RN%A_TROE*EXP(-TMP*RN%RT1_TROE) + EXP(-RN%T2_TROE/TMP),TWO_EPSILON_EB))
   ENDIF
   C = -0.4_EB - 0.67_EB*LOGFCENT
   N = 0.75_EB - 1.27_EB*LOGFCENT
   LOGPRC = LOG10(MAX(P_RI, TWO_EPSILON_EB)) + C
   CALCFCENT = 10._EB**(LOGFCENT/(1._EB + (LOGPRC/(N - D*LOGPRC))**2))
ELSE
   CALCFCENT = 1._EB  !FALLOFF-LINDEMANNN
ENDIF

RETURN

END FUNCTION CALCFCENT




!> \the jacobian of the ode right hand side function j = df/dy
!> \param TN_C is the current time
!> \param SUNVEC_Y is the current array of molar concentrations, temperature and pressure.
!> \param SUNVEC_F is the array of derivatives returned

INTEGER(C_INT) FUNCTION JACFN(TN_C, SUNVEC_Y, SUNVEC_F, SUNMAT_J, &
 USER_DATA, TMP1, TMP2, TMP3) &
 RESULT(IERR) BIND(C,NAME='JACFN')

USE, INTRINSIC :: ISO_C_BINDING
USE FSUNDIALS_NVECTOR_MOD
USE FSUNMATRIX_DENSE_MOD
USE FSUNDIALS_MATRIX_MOD

! CALLING VARIABLES
REAL(C_DOUBLE), VALUE :: TN_C             ! CURRENT TIME
TYPE(N_VECTOR)        :: SUNVEC_Y         ! CURRENT SOLUTION N_VECTOR
TYPE(N_VECTOR)        :: SUNVEC_F         ! CURRENT RHS N_VECTOR
TYPE(SUNMATRIX)       :: SUNMAT_J         ! JACOBIAN SUNMATRIX
TYPE(C_PTR), VALUE    :: USER_DATA        ! USER-DEFINED DATA
TYPE(N_VECTOR)        :: TMP1, TMP2, TMP3 ! WORKSPACE N_VECTORS

! POINTER TO DATA IN SUNDIALS MATRIX
REAL(EB), POINTER :: CVEC(:)
REAL(EB), POINTER :: FVEC(:)
REAL(EB), POINTER :: JMAT1D(:)
REAL(EB) :: JMAT(N_TRACKED_SPECIES+2, N_TRACKED_SPECIES+2)
REAL(EB), POINTER :: DUMMY(:)

INTEGER :: NS1, NS2, NEQ

! TO PREVENT UNUSED VARIABLE WARNING
IF(DEBUG) THEN
   WRITE(LU_ERR,*)" TIME= ",TN_C
   DUMMY => FN_VGETARRAYPOINTER(TMP1)
   DUMMY => FN_VGETARRAYPOINTER(TMP2)
   DUMMY => FN_VGETARRAYPOINTER(TMP3)
   IF (.NOT. C_ASSOCIATED(USER_DATA)) WRITE(LU_ERR,*)" NO USER_DATA IS PROVIDED"
ENDIF


! GET DATA ARRAYS FROM SUNDIALS VECTORS
CVEC => FN_VGETARRAYPOINTER(SUNVEC_Y)
FVEC => FN_VGETARRAYPOINTER(SUNVEC_F)
JMAT = 0._EB
CALL JACOBIAN(CVEC, FVEC, JMAT)

NEQ = N_TRACKED_SPECIES +2
JMAT1D => FSUNDENSEMATRIX_DATA(SUNMAT_J)
DO NS1=1, NEQ
   DO NS2=1, NEQ
      JMAT1D((NS1-1)*NEQ+NS2)=JMAT(NS1,NS2)
   ENDDO
ENDDO

! RETURN SUCCESS
IERR = 0
RETURN

END FUNCTION JACFN


!> \brief Calculate the jacobian matrix (jmat[n_tracked_species+2,
!> \param CVEC is the current array of molar concentrations, temperature and pressure.
!> \param FVEC is the array of derivatives passed as input
!> \param JMAT is the jacobian matrix returned

SUBROUTINE JACOBIAN(CVEC,FVEC, JMAT)

USE PHYSICAL_FUNCTIONS, ONLY : GET_SPECIFIC_HEAT_INTERP, GET_SPECIFIC_HEAT_Z,  & 
             GET_ENTHALPY_Z, GET_SPECIFIC_HEAT_TMP_DERIVATIVE, MOLAR_CONC_TO_MASS_FRAC

REAL(EB), POINTER, INTENT(INOUT) :: CVEC(:)
REAL(EB), POINTER, INTENT(INOUT) :: FVEC(:)
REAL(EB), INTENT(INOUT) :: JMAT(N_TRACKED_SPECIES+2, N_TRACKED_SPECIES+2)

REAL(EB) :: R_F,DCVEC1,DCVEC2, MIN_SPEC(N_TRACKED_SPECIES), KG,  TMP, RHO, &
            K_0, K_INF, P_RI, FCENT, C_I, RRTMP, THIRD_BODY_ENHANCEMENT
REAL(EB) :: ZZ(N_TRACKED_SPECIES), CP_I(N_TRACKED_SPECIES), HS_I(N_TRACKED_SPECIES)    
REAL(EB) :: DKCDTBYKC, DCIDC(N_TRACKED_SPECIES), DCIDT, CP, DCPDT, DKINFDTMPBYKINF, DTMPDT, DG, TMPI   
INTEGER :: I,NS, NS1, NS2, ITMP
TYPE(REACTION_TYPE), POINTER :: RN=>NULL()

TMP = MAX(CVEC(N_TRACKED_SPECIES+1), MIN_CHEM_TMP)
TMPI = 1._EB/TMP
PR = CVEC(N_TRACKED_SPECIES+2) ! PA
RRTMP = 1._EB/(R0*TMP)

RHO = DOT_PRODUCT(SPECIES_MIXTURE(1:N_TRACKED_SPECIES)%MW,CVEC(1:N_TRACKED_SPECIES))
MIN_SPEC = SPECIES_MIXTURE%ODE_REL_ERROR*ZZ_MIN_GLOBAL

! TO PREVENT UNUSED VARIABLE WARNING
IF(DEBUG) WRITE(LU_ERR,*)" SIZE OF FVEC ARRAY=", SIZE(FVEC)

! CALCULATE D[X]DOT/D[X] AND D[X]DOT/DT - Take contributions from every reactions 
REACTION_LOOP: DO I=1,N_REACTIONS
   DCIDC = 0._EB
   DCIDT = 0._EB

   RN => REACTION(I)
   ! CHECK FOR CONSUMED SPECIES
   DO NS=1,RN%N_SMIX_FR
      IF (RN%NU_MW_O_MW_F_FR(NS) < 0._EB .AND. CVEC(RN%NU_INDEX(NS)) < MIN_SPEC(NS)) CYCLE REACTION_LOOP
   ENDDO

   ! CALCULATE FORWARD RATE COEFF.
   K_INF = RN%A_SI*TMP**RN%N_T*EXP(-RN%E*RRTMP) !A_SI IS IN [KMOL/M3]^() UNIT.
   R_F = K_INF
   DKINFDTMPBYKINF = (RN%N_T+RN%E*RRTMP)*TMPI
   IF(RN%REVERSE) THEN ! COMPUTE EQUILIBRIUM CONSTANT
      ITMP = MIN(I_MAX_TEMP-1,INT(TMP))
      DG = RN%DELTA_G(ITMP) + (TMP-REAL(ITMP,EB))*(RN%DELTA_G(ITMP+1)-RN%DELTA_G(ITMP))
      KG = EXP(DG*TMPI)*(P_STP*RRTMP)**RN%C0_EXP
      R_F = R_F*KG ! KG IS THE CONCENTRATION EQUILIBRIUM CONSTANT OF REVERSE REACTION. 
      DKCDTBYKC = ((RN%DELTA_G(MIN(I_MAX_TEMP,NINT(TMP)))*TMPI + RN%DELTA_S(MIN(I_MAX_TEMP,NINT(TMP))))+RN%C0_EXP)*TMPI 
   ENDIF

   ! MULTIPLY WITH MOLAR_CONCENTRATION ^ STOICHIOMETRIC_COEFF
   DO NS=1,RN%N_SPEC
      IF (CVEC(YP2ZZ(RN%N_S_INDEX(NS))) < MIN_SPEC(YP2ZZ(RN%N_S_INDEX(NS)))) CYCLE REACTION_LOOP
      R_F = R_F*(CVEC(YP2ZZ(RN%N_S_INDEX(NS))))**RN%N_S(NS)
   ENDDO

   

   ! CALCULATE CI BASED ON TYPE OF REACTION
   IF (RN%THIRD_BODY) THEN
      IF (RN%N_THIRD > 0) THEN
         THIRD_BODY_ENHANCEMENT =  DOT_PRODUCT(CVEC(1:N_SPECIES),RN%THIRD_EFF(1:N_SPECIES))
      ELSE
         THIRD_BODY_ENHANCEMENT = PR*RRTMP
      ENDIF
      IF (RN%REACTYPE==THREE_BODY_ARRHENIUS_TYPE) THEN
         R_F = R_F * THIRD_BODY_ENHANCEMENT
         IF (THIRD_BODY_ENHANCEMENT > 0) THEN
            DO NS=1,N_TRACKED_SPECIES
               IF (RN%N_THIRD > 0) THEN
                  DCIDC(NS) = RN%THIRD_EFF(NS)/THIRD_BODY_ENHANCEMENT
               ELSE
                  DCIDC(NS) = 1._EB/THIRD_BODY_ENHANCEMENT
               ENDIF   
            ENDDO
         ENDIF   
         DCIDT = -TMPI
      ELSEIF(RN%REACTYPE==FALLOFF_LINDEMANN_TYPE .OR. RN%REACTYPE==FALLOFF_TROE_TYPE) THEN
         K_0 = RN%A_LOW_PR*TMP**(RN%N_T_LOW_PR)*EXP(-RN%E_LOW_PR*RRTMP)
         P_RI = K_0/K_INF*THIRD_BODY_ENHANCEMENT
         FCENT = CALCFCENT(TMP,P_RI, RN)
         C_I = P_RI/(1._EB+P_RI)*FCENT
         R_F = R_F * C_I
         IF (THIRD_BODY_ENHANCEMENT > TWO_EPSILON_EB) THEN
            CALL CALC_FALLOFF_DCIDC_AND_DCIDT(TMP, RN, K_0, K_INF, P_RI, FCENT, DCIDC, DCIDT)
         ENDIF 
      ENDIF 
   ENDIF

   !Contribution of qi
   DO NS1 = 1, RN%N_SPEC
      DO NS=1,RN%N_SMIX_FR
         DCVEC1 = R_F*RN%NU_NN(RN%NU_INDEX(NS))/CVEC(YP2ZZ(RN%N_S_INDEX(NS1)))
         JMAT((YP2ZZ(RN%N_S_INDEX(NS1))),RN%NU_INDEX(NS)) = &
         JMAT((YP2ZZ(RN%N_S_INDEX(NS1))),RN%NU_INDEX(NS))+ DCVEC1 
      ENDDO   
   ENDDO 

   ! Add contribution of CI
   IF (RN%THIRD_BODY) THEN
      DO NS = 1,N_TRACKED_SPECIES
         IF (abs(DCIDC(NS)) > 0._EB) THEN
            DO NS1=1,RN%N_SMIX_FR
               DCVEC2 = R_F*RN%NU_NN(RN%NU_INDEX(NS1))
               JMAT(NS,(YP2ZZ(RN%NU_INDEX(NS1)))) = &
                   JMAT(NS,(YP2ZZ(RN%NU_INDEX(NS1))))+ DCVEC2 * DCIDC(NS) 
            ENDDO
         ENDIF      
      ENDDO   
   ENDIF   

   ! Populate D[X]DOT/DT
   DO NS=1,RN%N_SMIX_FR
      DCVEC2 = R_F*RN%NU_NN(RN%NU_INDEX(NS))
      JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) = JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) - &
         DCVEC2*TMPI*(RN%RHO_EXPONENT+1) + DCVEC2 * DCIDT
      IF(RN%REVERSE) THEN
         JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) = JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) + &
            DCVEC2*(DKINFDTMPBYKINF - DKCDTBYKC)
      ELSE
         JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) = JMAT(N_TRACKED_SPECIES+1,RN%NU_INDEX(NS)) + &
            DCVEC2*DKINFDTMPBYKINF  
      ENDIF      

   ENDDO

ENDDO REACTION_LOOP

! CALCULATE DTDOT/D[X]
DTMPDT = FVEC(N_TRACKED_SPECIES+1)
CALL MOLAR_CONC_TO_MASS_FRAC(CVEC(1:N_TRACKED_SPECIES), ZZ(1:N_TRACKED_SPECIES))
CALL GET_SPECIFIC_HEAT_INTERP(ZZ(1:N_TRACKED_SPECIES),CP,TMP)
CALL GET_SPECIFIC_HEAT_TMP_DERIVATIVE(ZZ(1:N_TRACKED_SPECIES),DCPDT,TMP)
DO NS=1,N_TRACKED_SPECIES ! Store the CP and HS 
   CALL GET_SPECIFIC_HEAT_Z(NS, CP_I(NS), TMP)
   CALL GET_ENTHALPY_Z(NS, TMP,HS_I(NS))
   CP_I(NS)=CP_I(NS)*SPECIES_MIXTURE(NS)%MW
   HS_I(NS)=HS_I(NS)*SPECIES_MIXTURE(NS)%MW
ENDDO   

DO NS=1,N_TRACKED_SPECIES
   DO NS2=1,N_TRACKED_SPECIES
      JMAT(NS, N_TRACKED_SPECIES+1) = JMAT(NS, N_TRACKED_SPECIES+1)  + &
         HS_I(NS2)*JMAT(NS,NS2)
   ENDDO
   JMAT(NS, N_TRACKED_SPECIES+1) = JMAT(NS, N_TRACKED_SPECIES+1)  + CP_I(NS)*DTMPDT
   JMAT(NS, N_TRACKED_SPECIES+1) = - JMAT(NS, N_TRACKED_SPECIES+1) / RHO/CP   
ENDDO
      

 
! CALCULATE DTDOT/DT
DO NS=1,N_TRACKED_SPECIES
   JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) = JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) + &
      CP_I(NS)*FVEC(NS) + HS_I(NS)*JMAT(N_TRACKED_SPECIES+1,NS)   
ENDDO   
JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) = JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) + &
   RHO*DCPDT*DTMPDT
JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) = -JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1)/RHO/CP
JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) = JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1) + DTMPDT*TMPI


! CALL PRINT_JMAT(JMAT)


END SUBROUTINE JACOBIAN


!> \brief Print the component of jacobian matrix
SUBROUTINE PRINT_JMAT(JMAT)
REAL(EB), INTENT(IN) :: JMAT(N_TRACKED_SPECIES+2, N_TRACKED_SPECIES+2)
INTEGER :: NS, NS2

WRITE(LU_ERR,*)"------------------------------------------------------"
WRITE(LU_ERR,*)" Printing D[X]DOT/D[X] Jacobian elements...."
WRITE(LU_ERR,*)"------------------------------------------------------"
DO NS=1,N_TRACKED_SPECIES
   DO NS2=1,N_TRACKED_SPECIES
      IF (ABS(JMAT(NS,NS2)) > 0._EB) THEN
         WRITE(LU_ERR,'(A,2I4,5A,ES18.6)') "I,J=",NS, NS2, &
            " D[",TRIM(SPECIES_MIXTURE(NS2)%ID),"]DOT/D[",TRIM(SPECIES_MIXTURE(NS)%ID),"]=",JMAT(NS,NS2)
      ENDIF   
   ENDDO   
ENDDO

WRITE(LU_ERR,*)"------------------------------------------------------"
WRITE(LU_ERR,*)" Printing D[X]DOT/DT Jacobian elements...."
WRITE(LU_ERR,*)"------------------------------------------------------"
DO NS=1,N_TRACKED_SPECIES
   IF (ABS(JMAT(N_TRACKED_SPECIES+1,NS)) > 0._EB) THEN
      WRITE(LU_ERR,'(A,2I4,3A,ES18.6)') "I,J=",N_TRACKED_SPECIES+1, NS, &
         " D[",TRIM(SPECIES_MIXTURE(NS)%ID),"]DOT/D[T]=",JMAT(N_TRACKED_SPECIES+1,NS)
   ENDIF   
ENDDO

WRITE(LU_ERR,*)"------------------------------------------------------"
WRITE(LU_ERR,*)" Printing DTDOT/D[X] Jacobian elements...."
WRITE(LU_ERR,*)"------------------------------------------------------"
DO NS=1,N_TRACKED_SPECIES
   IF (ABS(JMAT(NS, N_TRACKED_SPECIES+1)) > 0._EB) THEN
      WRITE(LU_ERR,'(A,2I4,3A,ES18.6)') "I,J=",NS, N_TRACKED_SPECIES+1, &
         " D[T]DOT/D[",TRIM(SPECIES_MIXTURE(NS)%ID),"]=",JMAT(NS, N_TRACKED_SPECIES+1)
   ENDIF   
ENDDO

WRITE(LU_ERR,*)"------------------------------------------------------"
WRITE(LU_ERR,*)" Printing D[T]DOT/DT Jacobian elements...."
WRITE(LU_ERR,*)"------------------------------------------------------"
IF (ABS(JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1)) > 0._EB) THEN
   WRITE(LU_ERR,'(A,2I4,A,ES18.6)') "I,J=",N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1, &
      " D[T]DOT/D[T]=",JMAT(N_TRACKED_SPECIES+1, N_TRACKED_SPECIES+1)
ENDIF 
END SUBROUTINE PRINT_JMAT


!> \brief Calculate DCIDC of reactions 
!> \param TMP is the current temperature.
!> \param RN is the reaction
!> \param CVEC is the array of concentrations
!> \param PR is the PRESSURE RATIO.
!> \param F is the FALLOFF FUNCTION VALUE.

SUBROUTINE CALC_FALLOFF_DCIDC_AND_DCIDT(TMP, RN, K0, KINF, PR, F, DCIDC, DCIDT)
REAL(EB), INTENT(IN) :: TMP, PR, K0, KINF, F
TYPE(REACTION_TYPE), POINTER, INTENT(IN) :: RN
REAL(EB), INTENT(INOUT) :: DCIDC(N_TRACKED_SPECIES)
REAL(EB), INTENT(INOUT) :: DCIDT
REAL(EB) :: DPRDCI, DFDCI, DPRDT, DFDT, RRTMP
INTEGER :: NS

RRTMP = 1._EB/(R0*TMP)

DO NS=1,N_TRACKED_SPECIES
   DPRDCI = -RN%THIRD_EFF(NS     )*K0/KINF
   DFDCI = 0._EB
   IF (RN%REACTYPE==FALLOFF_TROE_TYPE) THEN
      DFDCI = DDC_TROE(PR, F, DPRDCI, TMP, RN)
   ENDIF   
   DCIDC(NS) = (DPRDCI/(PR*(1 + PR)) + DFDCI/F)
ENDDO


DPRDT = PR/TMP*( RN%N_T_LOW_PR + RN%E_LOW_PR*RRTMP - RN%N_T - RN%E*RRTMP - 1)
DFDT = 0._EB
IF (RN%REACTYPE==FALLOFF_TROE_TYPE) THEN
   DFDT = DDTMP_TROE(PR, F, DPRDT, TMP, RN)
ENDIF   
DCIDT = (DPRDT/(PR*(1 + PR)) + DFDT/F)

RETURN
END SUBROUTINE CALC_FALLOFF_DCIDC_AND_DCIDT

!> \brief Calculate derivative of TROE function w.r.t concentration  
REAL(EB) FUNCTION DDC_TROE(PR, F, DPRDC, TMP, RN) 
REAL(EB), INTENT(IN) :: PR, F, DPRDC, TMP
TYPE(REACTION_TYPE), POINTER, INTENT(IN) :: RN
REAL(EB) :: LOGPR, LOGTEN, LOGFCENT, C, N, DLOGPRDC, DPARENTDC
REAL(EB), PARAMETER :: D=0.14_EB

LOGPR = LOG10(MAX(PR,  TWO_EPSILON_EB))
LOGTEN = LOG(10.0)
IF (RN%T2_TROE <-1.E20_EB) THEN
   LOGFCENT = LOG10(MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE),TWO_EPSILON_EB))
ELSE
   LOGFCENT = LOG10(MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE) + EXP(-RN%T2_TROE/TMP),TWO_EPSILON_EB))
ENDIF

DLOGPRDC = DPRDC/PR/LOGTEN;
C = -0.4_EB - 0.67_EB*LOGFCENT
N = 0.75_EB - 1.27_EB*LOGFCENT

DPARENTDC = 2.0*(LOGPR + C)/((N - D*(LOGPR + C))**2)*((DLOGPRDC)-(LOGPR + C)*(-D*(DLOGPRDC))/(N - D*(LOGPR + C)))

DDC_TROE = F*LOGTEN * (- LOGFCENT*DPARENTDC/(1.0 + ((LOGPR + C)/(N - D*(LOGPR + C)))**2)**2)

END FUNCTION DDC_TROE




!> \brief Calculate derivative of TROE function w.r.t temperature  
REAL(EB) FUNCTION DDTMP_TROE(PR, F, DPRDT, TMP, RN) 
REAL(EB), INTENT(IN) :: PR, F, DPRDT, TMP
TYPE(REACTION_TYPE), POINTER, INTENT(IN) :: RN
REAL(EB) :: FCENT, LOGPR, LOGTEN, LOGFCENT, DFCENTDT, C, N, DCDT, DNDT, DPARENTDT, DLOGFCENTDT, DLOGPRDT
REAL(EB), PARAMETER :: D=0.14_EB

LOGPR = LOG10(MAX(PR, TWO_EPSILON_EB));
LOGTEN = LOG(10.0);
IF (RN%T2_TROE <-1.E20_EB) THEN
   FCENT = MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE),TWO_EPSILON_EB)
   DFCENTDT = (RN%A_TROE - 1)*EXP(-TMP*RN%RT3_TROE)*RN%RT3_TROE - &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE)*RN%RT1_TROE 
ELSE
   FCENT = MAX((1 - RN%A_TROE)*EXP(-TMP*RN%RT3_TROE) + &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE) + EXP(-RN%T2_TROE/TMP),TWO_EPSILON_EB)
   DFCENTDT = (RN%A_TROE - 1)*EXP(-TMP*RN%RT3_TROE)*RN%RT3_TROE - &
              RN%A_TROE*EXP(-TMP*RN%RT1_TROE)*RN%RT1_TROE + RN%T2_TROE*EXP(-RN%T2_TROE/TMP)/TMP**2
ENDIF
LOGFCENT = LOG10(FCENT)
DLOGFCENTDT = DFCENTDT/FCENT/LOGTEN;
C = -0.4_EB - 0.67_EB*LOGFCENT
N = 0.75_EB - 1.27_EB*LOGFCENT
DCDT = -0.67*DLOGFCENTDT
DNDT = -1.27*DLOGFCENTDT
DLOGPRDT = DPRDT/PR/LOGTEN

DPARENTDT = 2.0*(LOGPR + C)/((N - D*(LOGPR + C))**2)* &
   ((DLOGPRDT + DCDT) - (LOGPR + C)*(DNDT - D*(DLOGPRDT + DCDT))/(N - D*(LOGPR + C)))

DDTMP_TROE = F*LOGTEN*(DLOGFCENTDT/(1.0 + ((LOGPR + C)/(N - D*(LOGPR + C)))**2) &
          - LOGFCENT*DPARENTDT/(1.0 + ((LOGPR + C)/(N - D*(LOGPR + C)))**2)**2)

END FUNCTION DDTMP_TROE



!> \cvode interface for ODE integrator 
!> \Call sundials cvode in serial mode.
!> \param ZZ species mass fraction array
!> \param TMP_IN is the temperature
!> \param PR_IN is the pressure
!> \param TCUR is the start time in seconds
!> \param TEND is the end time in seconds
!> \param RTOL is the relative error for all the species (REAL_EB)
!> \param ATOL is the absolute error tolerance array for the species (REAL_EB)

SUBROUTINE CVODE_SERIAL(CC,TMP_IN, PR_IN, TCUR,TEND, RTOL, ATOL)
USE, INTRINSIC :: ISO_C_BINDING
USE PHYSICAL_FUNCTIONS, ONLY : MOLAR_CONC_TO_MASS_FRAC, CALC_EQUIV_RATIO
USE GLOBAL_CONSTANTS
USE FCVODE_MOD                 ! FORTRAN INTERFACE TO CVODE
USE FSUNDIALS_CONTEXT_MOD      ! FORTRAN INTERFACE TO SUNCONTEXT
USE FNVECTOR_SERIAL_MOD        ! FORTRAN INTERFACE TO SERIAL N_VECTOR
USE FSUNMATRIX_DENSE_MOD       ! FORTRAN INTERFACE TO DENSE SUNMATRIX
USE FSUNLINSOL_DENSE_MOD       ! FORTRAN INTERFACE TO DENSE SUNLINEARSOLVER
USE FSUNDIALS_LINEARSOLVER_MOD ! FORTRAN INTERFACE TO GENERIC SUNLINEARSOLVER
USE FSUNDIALS_MATRIX_MOD       ! FORTRAN INTERFACE TO GENERIC SUNMATRIX
USE FSUNDIALS_NVECTOR_MOD      ! FORTRAN INTERFACE TO GENERIC N_VECTOR

REAL(EB), INTENT(INOUT) :: CC(N_TRACKED_SPECIES)
REAL(EB), INTENT(IN)    :: TMP_IN, PR_IN,  TCUR, TEND
REAL(EB), INTENT(IN)    :: ATOL(N_TRACKED_SPECIES)
REAL(EB), INTENT(IN)    :: RTOL

! C-INTERFACE VARIABLES
REAL(C_DOUBLE)                 :: TCUR_C(1)      ! CURRENT TIME
INTEGER(C_INT)                 :: IERR_C         ! ERROR FLAG FROM C FUNCTIONS
REAL(C_DOUBLE)                 :: RTOL_C         ! RELATIVE AND ABSOLUTE TOLERANCE
REAL(C_DOUBLE)                 :: CVEC_C(N_TRACKED_SPECIES+2)    ! N_SP + 2 (FOR TEMPERATURE AND PRESSURE)
REAL(C_DOUBLE)                 :: ATOLVEC_C(N_TRACKED_SPECIES+2) ! N_SP + 2 
INTEGER(C_LONG)                :: MAXSTEPS_C     ! MAXIMUM NUMBER OF INTERNAL STEPS
INTEGER(C_LONG)                :: NEQ

! SUNDIALS RELATED VARIABLES
TYPE(C_PTR)                    :: SUNCTX       ! SUNDIALS SIMULATION CONTEXT
TYPE(C_PTR)                    :: CVODE_MEM    ! CVODE MEMORY
TYPE(N_VECTOR),        POINTER :: SUNVEC_Y     ! SUNDIALS VECTOR
TYPE(SUNMATRIX),       POINTER :: SUNMAT_A     ! SUNDIALS MATRIX
TYPE(SUNLINEARSOLVER), POINTER :: SUNLINSOL_LS ! SUNDIALS LINEAR SOLVER
TYPE(N_VECTOR),        POINTER :: SUNATOL      ! SUNDIALS VECTOR FOR ABSOLUTE TOLERANCE


REAL(EB) :: ZZ(N_TRACKED_SPECIES), EQUIV
INTEGER :: NS

!======= INTERNALS ============
NEQ = INT(N_TRACKED_SPECIES+2, KIND(8))
TCUR_C = TCUR
CVEC_C(1:N_TRACKED_SPECIES) = CC
CVEC_C(N_TRACKED_SPECIES+1) = TMP_IN
CVEC_C(N_TRACKED_SPECIES+2) = PR_IN
ATOLVEC_C(1:N_TRACKED_SPECIES) = ATOL
ATOLVEC_C(N_TRACKED_SPECIES+1) = 0.001_EB
ATOLVEC_C(N_TRACKED_SPECIES+2) = 0.001_EB

! CREATE SUNDIALS CONTEXT
IERR_C = FSUNCONTEXT_CREATE(C_NULL_PTR, SUNCTX)

! CREATE SUNDIALS N_VECTOR
SUNVEC_Y => FN_VMAKE_SERIAL(NEQ, CVEC_C, SUNCTX)
IF (.NOT. ASSOCIATED(SUNVEC_Y)) THEN
   WRITE(LU_ERR,*) 'ERROR: SUNVEC = NULL'
   STOP 1
END IF

! CREATE A DENSE MATRIX
SUNMAT_A => FSUNDENSEMATRIX(NEQ, NEQ, SUNCTX)
IF (.NOT. ASSOCIATED(SUNMAT_A)) THEN
   WRITE(LU_ERR,*)'ERROR: SUNMAT = NULL'
   STOP 1
END IF

! CREATE A DENSE LINEAR SOLVER
SUNLINSOL_LS => FSUNLINSOL_DENSE(SUNVEC_Y, SUNMAT_A, SUNCTX)
IF (.NOT. ASSOCIATED(SUNLINSOL_LS)) THEN
   WRITE(LU_ERR,*) 'ERROR: SUNLINSOL = NULL'
   STOP 1
END IF

! CREATE CVODE MEMORY
CVODE_MEM = FCVODECREATE(CV_BDF, SUNCTX)
IF (.NOT. C_ASSOCIATED(CVODE_MEM)) THEN
   WRITE(LU_ERR,*) 'ERROR: CVODE_MEM = NULL'
   STOP 1
END IF

! INITIALIZE CVODE
IERR_C = FCVODEINIT(CVODE_MEM, C_FUNLOC(RHSFN), TCUR_C(1), SUNVEC_Y)
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODEINIT, IERR = ', IERR_C, '; HALTING'
   STOP 1
END IF

! SET RELATIVE AND ABSOLUTE TOLERANCES
RTOL_C = RTOL
SUNATOL => FN_VMAKE_SERIAL(NEQ, ATOLVEC_C, SUNCTX)
IERR_C = FCVODESVTOLERANCES(CVODE_MEM, RTOL_C, SUNATOL)
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODESSTOLERANCES, IERR = ', IERR_C, '; HALTING'
   STOP 1
END IF

! ATTACH LINEAR SOLVER
IERR_C = FCVODESETLINEARSOLVER(CVODE_MEM, SUNLINSOL_LS, SUNMAT_A);
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODESETLINEARSOLVER, IERR = ', IERR_C, '; HALTING'
   STOP 1
END IF

! SET JACOBIAN ROUTINE
IERR_C = FCVODESETJACFN(CVODE_MEM, C_FUNLOC(JACFN))
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODESETJACFN, IERR = ', IERR_C, '; HALTING'
   STOP 1
END IF

! SET MAX NUM OF STEPS
MAXSTEPS_C = 100000
IERR_C = FCVODESETMAXNUMSTEPS(CVODE_MEM, MAXSTEPS_C)
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODESETMAXNUMSTEPS, IERR = ', IERR_C, '; HALTING'
   STOP 1
END IF

!CALL CVODE
IERR_C = FCVODE(CVODE_MEM, TEND, SUNVEC_Y, TCUR_C, CV_NORMAL)
IF (IERR_C /= 0) THEN
   WRITE(LU_ERR,*)" WARN: CVODE took all internal substeps. TSTART, TEND, DT=", TCUR, TEND, (TEND-TCUR)
   IF (DEBUG) THEN
      CALL MOLAR_CONC_TO_MASS_FRAC(CC(1:N_TRACKED_SPECIES), ZZ(1:N_TRACKED_SPECIES))
      CALL CALC_EQUIV_RATIO(ZZ(1:N_TRACKED_SPECIES), EQUIV)
      DO NS = 1, N_TRACKED_SPECIES
         WRITE(LU_ERR,*)" ID, Y=",SPECIES_MIXTURE(NS)%ID, ZZ(NS)
      ENDDO 
      WRITE(LU_ERR,*)" EQUIVALENCE RATIO, TMP=", EQUIV,TMP_IN
      CALL CVODESTATS(CVODE_MEM) ! DIAGNOSTICS OUTPUT
   ENDIF   
ENDIF

CC = CVEC_C(1:N_TRACKED_SPECIES) !DISCARD THE TEMPERATURE.

! CLEAN UP
CALL FCVODEFREE(CVODE_MEM)
IERR_C = FSUNLINSOLFREE(SUNLINSOL_LS)
CALL FSUNMATDESTROY(SUNMAT_A)
CALL FN_VDESTROY(SUNVEC_Y)
CALL FN_VDESTROY(SUNATOL)
IERR_C = FSUNCONTEXT_FREE(SUNCTX)


END SUBROUTINE CVODE_SERIAL


!> \brief Print cvode statstics to standard out

SUBROUTINE CVODESTATS(CVODE_MEM)

!======= INCLUSIONS ===========
USE ISO_C_BINDING
USE FCVODE_MOD
USE GLOBAL_CONSTANTS

TYPE(C_PTR), INTENT(IN) :: CVODE_MEM ! SOLVER MEMORY STRUCTURE

INTEGER(C_INT)  :: IERR          ! ERROR FLAG

INTEGER(C_LONG) :: NSTEPS(1)     ! NUM STEPS
INTEGER(C_LONG) :: NFEVALS(1)    ! NUM FUNCTION EVALS
INTEGER(C_LONG) :: NLINSETUPS(1) ! NUM LINEAR SOLVER SETUPS
INTEGER(C_LONG) :: NETFAILS(1)   ! NUM ERROR TEST FAILS

INTEGER(C_INT)  :: QLAST(1)      ! METHOD ORDER IN LAST STEP
INTEGER(C_INT)  :: QCUR(1)       ! METHOD ORDER FOR NEXT STEP

REAL(C_DOUBLE)  :: HINUSED(1)    ! INITIAL STEP SIZE
REAL(C_DOUBLE)  :: HLAST(1)      ! LAST STEP SIZE
REAL(C_DOUBLE)  :: HCUR(1)       ! STEP SIZE FOR NEXT STEP
REAL(C_DOUBLE)  :: TCUR(1)       ! INTERNAL TIME REACHED

INTEGER(C_LONG) :: NNITERS(1)    ! NONLINEAR SOLVER ITERATIONS
INTEGER(C_LONG) :: NNCFAILS(1)   ! NONLINEAR SOLVER FAILS

INTEGER(C_LONG) :: NJEVALS(1)    ! NUM JACOBIAN EVALUATIONS

!======= INTERNALS ============

! GENERAL SOLVER STATISTICS
IERR = FCVODEGETINTEGRATORSTATS(CVODE_MEM, NSTEPS, NFEVALS, NLINSETUPS, &
    NETFAILS, QLAST, QCUR, HINUSED, HLAST, HCUR, TCUR)
IF (IERR /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODEGETINTEGRATORSTATS, IERR = ', IERR, '; HALTING'
   STOP 1
END IF

! NONLINEAR SOLVER STATISTICS
IERR = FCVODEGETNONLINSOLVSTATS(CVODE_MEM, NNITERS, NNCFAILS)
IF (IERR /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODEGETNONLINSOLVSTATS, IERR = ', IERR, '; HALTING'
   STOP 1
END IF

! NUMBER OF JACOBIAN EVALUATIONS
IERR = FCVODEGETNUMJACEVALS(CVODE_MEM, NJEVALS)
IF (IERR /= 0) THEN
   WRITE(LU_ERR,*) 'ERROR IN FCVODEGETNUMJACEVALS, IERR = ', IERR, '; HALTING'
   STOP 1
END IF

WRITE(LU_ERR,*) ' '
WRITE(LU_ERR,*) ' GENERAL SOLVER STATS:'
WRITE(LU_ERR,'(4X,A,I9)')    'TOTAL INTERNAL STEPS TAKEN =',NSTEPS
WRITE(LU_ERR,'(4X,A,I9)')    'TOTAL RHS FUNCTION CALLS   =',NFEVALS
WRITE(LU_ERR,'(4X,A,I9)')    'NUM LIN SOLVER SETUP CALLS =',NLINSETUPS
WRITE(LU_ERR,'(4X,A,I9)')    'NUM ERROR TEST FAILURES    =',NETFAILS
WRITE(LU_ERR,'(4X,A,I9)')    'LAST METHOD ORDER          =',QLAST
WRITE(LU_ERR,'(4X,A,I9)')    'NEXT METHOD ORDER          =',QCUR
WRITE(LU_ERR,'(4X,A,ES12.5)')'FIRST INTERNAL STEP SIZE   =',HINUSED
WRITE(LU_ERR,'(4X,A,ES12.5)')'LAST INTERNAL STEP SIZE    =',HLAST
WRITE(LU_ERR,'(4X,A,ES12.5)')'NEXT INTERNAL STEP SIZE    =',HCUR
WRITE(LU_ERR,'(4X,A,ES12.5)')'CURRENT INTERNAL TIME      =',TCUR
WRITE(LU_ERR,'(4X,A,I9)')    'NUM NONLINEAR SOLVER ITERS =',NNITERS
WRITE(LU_ERR,'(4X,A,I9)')    'NUM NONLINEAR SOLVER FAILS =',NNCFAILS
WRITE(LU_ERR,'(4X,A,I9)')    'NUM JACOBIAN EVALUATIONS   =',NJEVALS
WRITE(LU_ERR,*) ' '

RETURN

END SUBROUTINE CVODESTATS

END MODULE CVODE_INTERFACE
#endif
