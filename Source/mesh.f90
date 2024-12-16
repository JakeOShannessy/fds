!> \brief Variables that are defined on each mesh.

MODULE MESH_VARIABLES

USE PRECISION_PARAMETERS
USE TYPES
IMPLICIT NONE (TYPE,EXTERNAL)

!> \brief Derived type containing the bulk of the variables defined on each mesh.
!>
!> \details The variables listed in this module are defined on each mesh that is
!> owned by a given MPI process. For example, MESHES(NM)%U(I,J,K) is the \f$u\f$
!> component of velocity at the forward \f$x\f$ face of grid cell (I,J,K) and
!> mesh NM.

TYPE MESH_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: U  !< Velocity component at current time step, \f$u_{ijk}^n\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: V  !< Velocity component at current time step, \f$v_{ijk}^n\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: W  !< Velocity component at current time step, \f$w_{ijk}^n\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: US !< Velocity component estimated at next time step, \f$u_{ijk}^*\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: VS !< Velocity component estimated at next time step, \f$v_{ijk}^*\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: WS !< Velocity component estimated at next time step, \f$w_{ijk}^*\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: DDDT    !< \f$(\partial D/\partial t)_{ijk}\f$ where \f$D_{ijk}\f$ is the divergence
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: D       !< Divergence at current time step, \f$D_{ijk}^n\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: DS      !< Divergence estimate next time step, \f$D_{ijk}^*\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: H       !< \f$ \tilde{p}_{ijk}/\rho_{ijk} + |\mathbf{u}|^2_{ijk}/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: HS      !< H estimated at next time step
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: KRES    !< Resolved kinetic energy, \f$ |\mathbf{u}|^2_{ijk}/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVX     !< Momentum equation flux terms, \f$ F_{{\rm A},x,ijk}+F_{{\rm B},x,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVY     !< Momentum equation flux terms, \f$ F_{{\rm A},y,ijk}+F_{{\rm B},y,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVZ     !< Momentum equation flux terms, \f$ F_{{\rm A},z,ijk}+F_{{\rm B},z,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVX_B   !< Momentum equation flux terms, \f$ F_{{\rm B},x,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVY_B   !< Momentum equation flux terms, \f$ F_{{\rm B},y,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVZ_B   !< Momentum equation flux terms, \f$ F_{{\rm B},z,ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVX_D   !< Particle drag
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVY_D   !< Particle drag
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: FVZ_D   !< Particle drag
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: RHO     !< Density (kg/m3) at current time step, \f$ \rho_{ijk}^n \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: RHOS    !< Density (kg/m3) at next time step, \f$ \rho_{ijk}^* \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: MU      !< Turbulent viscosity (kg/m/s), \f$ \mu_{{\rm t},ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: MU_DNS  !< Laminar viscosity (kg/m/s)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: TMP     !< Gas temperature, \f$ T_{ijk} \f$ (K)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: Q       !< Heat release rate per unit volume, \f$ \dot{q}_{ijk}''' \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: KAPPA_GAS !< Radiation absorption coefficient by gas, \f$ \kappa_{ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: CHI_R   !< Radiative fraction, \f$ \chi_{{\rm r},ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: QR      !< Radiation source term, \f$ -\nabla \cdot \dot{\mathbf{q}}_{\rm r}'' \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: QR_W    !< Radiation source term, particles and droplets
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: UII     !< Integrated intensity, \f$ U_{ijk}=\sum_{l=1}^N I_{ijk}^l\delta\Omega^l\f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: RSUM    !< \f$ R_0 \sum_\alpha Z_{\alpha,ijk}/W_\alpha \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: D_SOURCE!< Source terms in the expression for the divergence
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: CSD2    !< \f$ C_s \Delta^2 \f$ in Smagorinsky turbulence expression
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: CHEM_SUBIT  !< Number of chemistry sub-iterations
   INTEGER,  ALLOCATABLE, DIMENSION(:,:)   :: CHEM_ACTIVE_CELLS !< I , J ,K info of chemically active cells.
   INTEGER,  ALLOCATABLE, DIMENSION(:,:)   :: CHEM_ACTIVE_CC !< ICC, JCC of chemically active cells.
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: MIX_TIME    !< Mixing-controlled combustion reaction time (s)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: STRAIN_RATE !< Strain rate \f$ |S|_{ijk} \f$ (1/s)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: D_Z_MAX     !< \f$ \max D_\alpha \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: PP_RESIDUAL !< Pressure Poisson residual (debug)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: LES_FILTER_WIDTH !< Characteristic cell dimension (m)

   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: ZZ               !< Lumped species, current time step, \f$ Z_{\alpha,ijk}^n \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: ZZS              !< Lumped species, next time step, \f$ Z_{\alpha,ijk}^* \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: REAC_SOURCE_TERM !< \f$ \dot{m}_{\alpha,ijk}''' \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DEL_RHO_D_DEL_Z  !< \f$ (\nabla \cdot \rho D_\alpha \nabla Z_\alpha)_{ijk} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: FX               !< \f$ \rho Z_{\alpha,ijk} \f$ at \f$ x \f$ face of cell
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: FY               !< \f$ \rho Z_{\alpha,ijk} \f$ at \f$ y \f$ face of cell
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: FZ               !< \f$ \rho Z_{\alpha,ijk} \f$ at \f$ z \f$ face of cell
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: Q_REAC           !< \f$ \dot{q}_{ijk}''' \f$ for a specified reaction
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: AVG_DROP_DEN     !< Droplet mass per unit volume for a certain droplet type
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: AVG_DROP_TMP     !< Average temperature for a certain droplet type
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: AVG_DROP_RAD     !< Average radius for a certain droplet type
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: AVG_DROP_AREA    !< Average area for a certain droplet type
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: M_DOT_PPP        !< Mass source term, \f$ \dot{m}_{\alpha,ijk}''' \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: ADV_FX
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: ADV_FY
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: ADV_FZ
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FX
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FY
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FZ
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FXS
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FYS
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: DIF_FZS

   REAL(EB) :: POIS_PTB,POIS_ERR
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: SAVE1,SAVE2,WORK
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: PRHS !< Right hand side of Poisson (pressure) equation
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: BXS,BXF,BYS,BYF,BZS,BZF, BXST,BXFT,BYST,BYFT,BZST,BZFT
   INTEGER :: LSAVE,LWORK,LBC,MBC,NBC,LBC2,MBC2,NBC2,ITRN,JTRN,KTRN,IPS

   REAL(EB), ALLOCATABLE, DIMENSION(:) :: P_0         !< Ambient pressure profile, \f$ \overline{p}_0(z) \f$ (Pa)
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RHO_0       !< Ambient density profile, \f$ \overline{\rho}_0(z) \f$ (kg/m\f$^3\f$)
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: TMP_0       !< Ambient temperature profile, \f$ \overline{T}_0(z) \f$ (K)
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: D_PBAR_DT   !< \f$ (\partial \overline{p}_m/\partial t)^n \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: D_PBAR_DT_S !< \f$ (\partial \overline{p}_m/\partial t)^* \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: U_LEAK
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: U_WIND      !< Component of wind in x direction
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: V_WIND      !< Component of wind in y direction
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: W_WIND      !< Component of wind in z direction

   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: PBAR      !< Background pressure, current, \f$ \overline{p}_m^n(z,t) \f$ (Pa)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: PBAR_S    !< Background pressure, estimated, \f$ \overline{p}_m^*(z,t) \f$ (Pa)
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: R_PBAR    !< \f$ 1/\overline{p}_m(z,t) \f$
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: PRESSURE_ZONE        !< Index of the pressure zone for cell (I,J,K)
   TYPE (ZONE_MESH_TYPE), ALLOCATABLE, DIMENSION(:) :: ZONE_MESH  !< Stores Pardiso parameters for specific ZONE on a MESH
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: MUNKH !< Stores pressure unknown indexes for the mesh.

   ! Work arrays

   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: SWORK1
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: SWORK2
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: SWORK3
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: SWORK4
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,WORK8,WORK9
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: IWORK1
   REAL(EB),     ALLOCATABLE, DIMENSION(:,:,:) :: PWORK1,PWORK2,PWORK3,PWORK4
   COMPLEX(EB),  ALLOCATABLE, DIMENSION(:,:,:) :: PWORK5,PWORK6,PWORK7,PWORK8
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: TURB_WORK1,TURB_WORK2,TURB_WORK3,TURB_WORK4
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: TURB_WORK5,TURB_WORK6,TURB_WORK7,TURB_WORK8
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: TURB_WORK9,TURB_WORK10
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:) :: CCVELDIV,CARTVELDIV
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: WALL_WORK1,WALL_WORK2,FACE_WORK1,FACE_WORK2,FACE_WORK3
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: FIRE_ARRIVAL_TIME,FIRE_RESIDENCE_TIME
   REAL(FB), ALLOCATABLE, DIMENSION(:,:,:,:) :: QQ, QQ2
   REAL(FB), ALLOCATABLE, DIMENSION(:,:) :: PP,PPN,BNDF_TIME_INTEGRAL
   INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IBK
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: IBLK
   TYPE(STORAGE_TYPE) :: WALL_STORAGE,CFACE_STORAGE

   REAL(EB) :: CFL,DIVMX,DIVMN,VN,RESMAX,PART_UVWMAX=0._EB
   INTEGER  :: ICFL,JCFL,KCFL,IMX,JMX,KMX,IMN,JMN,KMN, I_VN,J_VN,K_VN,IRM,JRM,KRM, DT_RESTRICT_COUNT=0,DT_RESTRICT_STORE=0
   LOGICAL  :: CLIP_RHOMIN=.FALSE.,CLIP_RHOMAX=.FALSE.
   LOGICAL :: BAROCLINIC_TERMS_ATTACHED=.FALSE.
   LOGICAL :: BNDF_DUMP=.TRUE.
   LOGICAL :: PARTICLES_EXISTED = .FALSE.
   CHARACTER(LABEL_LENGTH) :: TRNX_ID,TRNY_ID,TRNZ_ID
   INTEGER     :: NCHEM_ACTIVE_CELLS=0, NCHEM_ACTIVE_CC=0 !< Number of Chemically active cells and cutcells 


   TYPE(EDGE_TYPE), ALLOCATABLE, DIMENSION(:) :: EDGE

   INTEGER :: IBAR !< Number of cells in the \f$ x \f$ direction, \f$ I \f$
   INTEGER :: JBAR !< Number of cells in the \f$ y \f$ direction, \f$ J \f$
   INTEGER :: KBAR !< Number of cells in the \f$ z \f$ direction, \f$ K \f$
   INTEGER :: IBM1 !< IBAR minus 1
   INTEGER :: JBM1 !< JBAR minus 1
   INTEGER :: KBM1 !< KBAR minus 1
   INTEGER :: IBP1 !< IBAR plus 1
   INTEGER :: JBP1 !< JBAR plus 1
   INTEGER :: KBP1 !< KBAR plus 1
   INTEGER :: N_NEIGHBORING_MESHES !< Number of meshing abutting the current one
   INTEGER, ALLOCATABLE, DIMENSION(:) :: NEIGHBORING_MESH  !< Array listing the indices of neighboring meshes
   INTEGER, ALLOCATABLE, DIMENSION(:) :: RGB               !< Color indices of the mesh for Smokeview

   ! Mesh coordinate variables

   REAL(EB) :: DXI                               !< \f$ \delta \xi = (x_I-x_0)/I \f$
   REAL(EB) :: DETA                              !< \f$ \delta \eta = (y_J-y_0)/J \f$
   REAL(EB) :: DZETA                             !< \f$ \delta \zeta = (z_K-z_0)/K \f$
   REAL(EB) :: RDXI                              !< \f$ 1/ \delta \xi \f$
   REAL(EB) :: RDETA                             !< \f$ 1/ \delta \eta \f$
   REAL(EB) :: RDZETA                            !< \f$ 1/ \delta \zeta \f$
   REAL(EB) :: DXMIN                             !< \f$ \min_i \delta x_i \f$
   REAL(EB) :: DXMAX                             !< \f$ \max_i \delta x_i \f$
   REAL(EB) :: DYMIN                             !< \f$ \min_j \delta y_j \f$
   REAL(EB) :: DYMAX                             !< \f$ \max_j \delta y_j \f$
   REAL(EB) :: DZMIN                             !< \f$ \min_k \delta z_k \f$
   REAL(EB) :: DZMAX                             !< \f$ \max_k \delta z_k \f$
   REAL(EB) :: XS                                !< Lower extent of mesh x coordinate, \f$ x_0 \f$
   REAL(EB) :: XF                                !< Upper extent of mesh x coordinate, \f$ x_I \f$
   REAL(EB) :: YS                                !< Lower extent of mesh y coordinate, \f$ y_0 \f$
   REAL(EB) :: YF                                !< Upper extent of mesh y coordinate, \f$ y_J \f$
   REAL(EB) :: ZS                                !< Lower extent of mesh z coordinate, \f$ z_0 \f$
   REAL(EB) :: ZF                                !< Upper extent of mesh z coordinate, \f$ z_K \f$
   REAL(EB) :: RDXINT                            !< \f$ 500/\delta \xi \f$
   REAL(EB) :: RDYINT                            !< \f$ 500/\delta \eta \f$
   REAL(EB) :: RDZINT                            !< \f$ 500/\delta \zeta \f$
   REAL(EB) :: CELL_SIZE                         !< Approximate cell size, \f$ (\delta\xi\,\delta\eta\,\delta\zeta)^{1/3} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: R      !< Radial coordinate, \f$ r_i \f$, for CYLINDRICAL geometry
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RC     !< Radial coordinate, cell center, \f$ (r_i+r_{i-1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RRN    !< \f$ 2/(r_i+r_{i-1}) \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: X      !< Position of forward x face of cell (I,J,K), \f$ x_i \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: Y      !< Position of forward y face of cell (I,J,K), \f$ y_j \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: Z      !< Position of forward z face of cell (I,J,K), \f$ z_k \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: XC     !< x coordinate of cell center, \f$ (x_i+x_{i-1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: YC     !< y coordinate of cell center, \f$ (y_j+y_{j-1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: ZC     !< z coordinate of cell center, \f$ (z_k+z_{k-1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: HX     !< Grid stretch factor, \f$ (x_i-x_{i-1})/\delta \xi \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: HY     !< Grid stretch factor, \f$ (y_j-y_{j-1})/\delta \eta \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: HZ     !< Grid stretch factor, \f$ (z_k-z_{k-1})/\delta \zeta \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DX     !< \f$ \delta x_i = x_i-x_{i-1} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DY     !< \f$ \delta y_j = y_j-y_{j-1} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DZ     !< \f$ \delta z_k = z_k-z_{k-1} \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDX    !< \f$ 1/\delta x_i \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDY    !< \f$ 1/\delta y_j \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDZ    !< \f$ 1/\delta z_k \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DXN    !< \f$ (x_i+x_{i+1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DYN    !< \f$ (y_j+y_{j+1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: DZN    !< \f$ (z_k+z_{k+1})/2 \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDXN   !< \f$ 2/(x_i+x_{i+1}) \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDYN   !< \f$ 2/(y_j+y_{j+1}) \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: RDZN   !< \f$ 2/(z_k+z_{k+1}) \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: CELLSI !< Array used to locate the cell index of \f$ x \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: CELLSJ !< Array used to locate the cell index of \f$ y \f$
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: CELLSK !< Array used to locate the cell index of \f$ z \f$
   INTEGER :: CELLSI_LO,CELLSI_HI                !< hold CELLSI array bounds
   INTEGER :: CELLSJ_LO,CELLSJ_HI                !< hold CELLSJ array bounds
   INTEGER :: CELLSK_LO,CELLSK_HI                !< hold CELLSK array bounds
   REAL(FB), ALLOCATABLE, DIMENSION(:) :: XPLT   !< 4 byte real array holding \f$ x \f$ mesh coordinates
   REAL(FB), ALLOCATABLE, DIMENSION(:) :: YPLT   !< 4 byte real array holding \f$ y \f$ mesh coordinates
   REAL(FB), ALLOCATABLE, DIMENSION(:) :: ZPLT   !< 4 byte real array holding \f$ z \f$ mesh coordinates

   INTEGER :: N_OBST=0                                              !< Number of obstructions in the mesh
   TYPE(OBSTRUCTION_TYPE), ALLOCATABLE, DIMENSION(:) :: OBSTRUCTION !< Derived type variable holding obstruction information

   INTEGER :: N_VENT=0                                              !< Number of vents in the mesh
   TYPE(VENTS_TYPE), ALLOCATABLE, DIMENSION(:) :: VENTS             !< Derived type variable holding vent information

   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: CELL_INDEX             !< Unique integer identifier for grid cell (I,J,K)
   TYPE(CELL_TYPE), ALLOCATABLE, DIMENSION(:) :: CELL               !< Variables associated with nearby solids
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: CELL_ILW
   INTEGER, ALLOCATABLE, DIMENSION(:) :: CELL_INTEGERS              !< 1-D array to hold CELL variables
   LOGICAL, ALLOCATABLE, DIMENSION(:) :: CELL_LOGICALS              !< 1-D array to hold CELL variables

   LOGICAL, ALLOCATABLE, DIMENSION(:) :: CONNECTED_MESH             !< T or F if cell is within another mesh

   INTEGER :: NREGFACE_H(MAX_DIM)=0
   TYPE(CC_REGFACE_TYPE), ALLOCATABLE, DIMENSION(:) :: REGFACE_IAXIS_H, &
                                                       REGFACE_JAXIS_H, &
                                                       REGFACE_KAXIS_H

   ! CC_IBM mesh arrays

   INTEGER :: N_EDGE_CROSS=0,  N_CUTEDGE_MESH=0, N_CUTFACE_MESH=0, N_CUTCELL_MESH=0
   INTEGER :: N_BBCUTFACE_MESH=0, N_GCCUTFACE_MESH=0, N_GCCUTCELL_MESH=0
   INTEGER :: FINEST_LINK_LEV=0, NUNK_F=0
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: VERTVAR, CCVAR
   INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: ECVAR, FCVAR
   TYPE(CC_EDGECROSS_TYPE), ALLOCATABLE, DIMENSION(:) :: EDGE_CROSS
   TYPE(CC_CUTEDGE_TYPE),   ALLOCATABLE, DIMENSION(:) :: CUT_EDGE
   TYPE(CC_CUTFACE_TYPE),   ALLOCATABLE, DIMENSION(:) :: CUT_FACE
   TYPE(CC_CUTCELL_TYPE),   ALLOCATABLE, DIMENSION(:) :: CUT_CELL

   INTEGER :: CC_NREGFACE_Z(MAX_DIM)=0, CC_NBBREGFACE_Z(MAX_DIM)=0
   TYPE(CC_REGFACEZ_TYPE), ALLOCATABLE, DIMENSION(:) :: CC_REGFACE_IAXIS_Z, CC_REGFACE_JAXIS_Z, CC_REGFACE_KAXIS_Z
   INTEGER :: CC_NRCFACE_Z=0, CC_NBBRCFACE_Z=0, CC_NRCFACE_H=0
   INTEGER, ALLOCATABLE, DIMENSION(:) :: RCF_H
   TYPE(CC_RCFACE_TYPE), ALLOCATABLE, DIMENSION(:) :: RC_FACE
   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: RHO_ZZN

   ! CFACE to be used in conjunction with solid side solvers:
   TYPE(CFACE_TYPE), ALLOCATABLE, DIMENSION(:) :: CFACE

   ! Edges connected to regular Gas faces to receive wall model shear stress and vorticity.
   INTEGER :: CC_NRCEDGE=0, CC_NIBEDGE=0
   TYPE(CC_EDGE_TYPE), ALLOCATABLE, DIMENSION(:) :: CC_RCEDGE, CC_IBEDGE

   ! Array with maximum height (Z) of geometry intersections with vertical grid lines in the mesh.
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: GEOM_ZMAX

   ! Arrays for special cut-cells:
   INTEGER :: N_SPCELL=0, N_SPCELL_CF=0
   INTEGER, ALLOCATABLE, DIMENSION(:,:) :: SPCELL_LIST

   ! Linked face velocity arrays:
   REAL(EB), ALLOCATABLE, DIMENSION(:) :: EWC_UN_LNK, UN_LNK, UN_ULNK

   ! Array with boundary cut-face areas pes cut-cell, for use in cut-cell blocking.
   TYPE(CC_INBCF_AREA_TYPE), ALLOCATABLE, DIMENSION(:,:,:) :: INBCF_AREA

   ! Arrays for cut-cell blocking:
   INTEGER :: N_CC_BLOCKED=0
   INTEGER,  ALLOCATABLE, DIMENSION(:,:) :: JBT_CC_BLOCKED
   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: XYZ_CC_BLOCKED

   ! ...

   INTEGER :: N_WALL_CELLS=0,N_WALL_CELLS_DIM=0,N_INTERNAL_WALL_CELLS=0,N_EXTERNAL_WALL_CELLS=0
   INTEGER :: N_THIN_WALL_CELLS=0,N_THIN_WALL_CELLS_DIM=0
   INTEGER :: HT_3D_SWEEP_DIRECTION=0
   INTEGER :: N_EXTERNAL_CFACE_CELLS=0,N_INTWALL_CFACE_CELLS=0,INTERNAL_CFACE_CELLS_LB=0,&
              N_INTERNAL_CFACE_CELLS=0,N_CFACE_CELLS_DIM=0
   REAL(EB) :: BC_CLOCK
   REAL(EB), ALLOCATABLE, DIMENSION(:)   :: UVW_SAVE,U_GHOST,V_GHOST,W_GHOST
   TYPE(WALL_TYPE), ALLOCATABLE, DIMENSION(:) :: WALL
   TYPE(THIN_WALL_TYPE), ALLOCATABLE, DIMENSION(:) :: THIN_WALL
   TYPE(EXTERNAL_WALL_TYPE), ALLOCATABLE, DIMENSION(:) :: EXTERNAL_WALL
   TYPE(OMESH_TYPE), ALLOCATABLE, DIMENSION(:) :: OMESH
   TYPE(LAGRANGIAN_PARTICLE_TYPE), ALLOCATABLE, DIMENSION(:) :: LAGRANGIAN_PARTICLE
   TYPE(BOUNDARY_ONE_D_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_ONE_D
   TYPE(BOUNDARY_THR_D_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_THR_D
   TYPE(BOUNDARY_COORD_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_COORD
   TYPE(BOUNDARY_PROP1_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_PROP1
   TYPE(BOUNDARY_PROP2_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_PROP2
   TYPE(BOUNDARY_RADIA_TYPE), ALLOCATABLE, DIMENSION(:) :: BOUNDARY_RADIA
   INTEGER :: N_BOUNDARY_COORD_DIM=0,N_BOUNDARY_ONE_D_DIM=0,N_BOUNDARY_THR_D_DIM=0,N_BOUNDARY_PROP1_DIM=0,N_BOUNDARY_PROP2_DIM=0,&
              N_BOUNDARY_RADIA_DIM=0
   INTEGER :: NEXT_AVAILABLE_BOUNDARY_COORD_SLOT=1,&
              NEXT_AVAILABLE_BOUNDARY_ONE_D_SLOT=1,&
              NEXT_AVAILABLE_BOUNDARY_THR_D_SLOT=1,&
              NEXT_AVAILABLE_BOUNDARY_PROP1_SLOT=1,&
              NEXT_AVAILABLE_BOUNDARY_PROP2_SLOT=1,&
              NEXT_AVAILABLE_BOUNDARY_RADIA_SLOT=1
   INTEGER, ALLOCATABLE, DIMENSION(:) :: BOUNDARY_COORD_OCCUPANCY,BOUNDARY_ONE_D_OCCUPANCY,BOUNDARY_THR_D_OCCUPANCY, &
                                         BOUNDARY_PROP1_OCCUPANCY,BOUNDARY_PROP2_OCCUPANCY,BOUNDARY_RADIA_OCCUPANCY,PARTICLE_LAST
   INTEGER :: NLP,NLPDIM,PARTICLE_TAG

   INTEGER :: N_SLCF=0
   TYPE(SLICE_TYPE), ALLOCATABLE, DIMENSION(:) :: SLICE

   INTEGER :: N_RADF=0
   TYPE(RAD_FILE_TYPE), ALLOCATABLE, DIMENSION(:) :: RAD_FILE

   INTEGER :: N_PATCH,N_BNDF_POINTS,N_EXTERIOR_PATCH
   TYPE(PATCH_TYPE), ALLOCATABLE, DIMENSION(:) :: PATCH
   TYPE(PATCH_TYPE), ALLOCATABLE, DIMENSION(:) :: EXTERIOR_PATCH

   REAL(EB), ALLOCATABLE, DIMENSION(:,:,:,:) :: UIID
   INTEGER :: RAD_CALL_COUNTER,ANGLE_INC_COUNTER

   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: INTERPOLATED_MESH

   CHARACTER(MESH_STRING_LENGTH), ALLOCATABLE, DIMENSION(:) :: STRING
   INTEGER :: N_STRINGS,N_STRINGS_MAX

   INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: K_AGL_SLICE
   INTEGER, ALLOCATABLE, DIMENSION(:,:) :: LS_KLO_TERRAIN,LS_KHI_TERRAIN,K_LS,LS_SURF_INDEX
   INTEGER :: N_TERRAIN_SLCF=0

   REAL(EB), ALLOCATABLE, DIMENSION(:,:) :: FLUX0_LS,FLUX1_LS,PHI_LS,PHI1_LS,ROS_BACKU, &
                                            ROS_HEAD,ROS_FLANK,WIND_EXP, &
                                            SR_X_LS,SR_Y_LS,U_LS,V_LS,Z_LS,DZTDX,DZTDY,MAG_ZT, &
                                            PHI_WS,UMF,THETA_ELPS,PHI_S,PHI_S_X,PHI_S_Y,PHI_W,LS_WORK1,LS_WORK2

END TYPE MESH_TYPE

TYPE (MESH_TYPE), SAVE, DIMENSION(:), ALLOCATABLE, TARGET :: MESHES

END MODULE MESH_VARIABLES


MODULE MESH_POINTERS

USE PRECISION_PARAMETERS
USE MESH_VARIABLES
IMPLICIT NONE (TYPE,EXTERNAL)

REAL(EB), POINTER, DIMENSION(:,:,:) :: &
   U,V,W,US,VS,WS,DDDT,D,DS,H,HS,KRES,FVX,FVY,FVZ,FVX_B,FVY_B,FVZ_B,FVX_D,FVY_D,FVZ_D,RHO,RHOS, &
   MU,MU_DNS,TMP,Q,KAPPA_GAS,CHI_R,QR,QR_W,UII,RSUM,D_SOURCE, &
   CSD2,MTR,MSR,WEM,MIX_TIME,CHEM_SUBIT,STRAIN_RATE,D_Z_MAX,PP_RESIDUAL,LES_FILTER_WIDTH
REAL(EB), POINTER, DIMENSION(:,:,:,:) :: ZZ,ZZS,REAC_SOURCE_TERM,DEL_RHO_D_DEL_Z,FX,FY,FZ, &
                                         SWORK1,SWORK2,SWORK3,SWORK4, &
                                         Q_REAC,AVG_DROP_DEN,AVG_DROP_TMP,AVG_DROP_RAD,AVG_DROP_AREA,M_DOT_PPP, &
                                         ADV_FX,ADV_FY,ADV_FZ,DIF_FX,DIF_FY,DIF_FZ,DIF_FXS,DIF_FYS,DIF_FZS
INTEGER, POINTER, DIMENSION(:,:)   :: CHEM_ACTIVE_CELLS
INTEGER, POINTER, DIMENSION(:,:)   :: CHEM_ACTIVE_CC
REAL(EB), POINTER :: POIS_PTB,POIS_ERR
REAL(EB), POINTER, DIMENSION(:) :: SAVE1,SAVE2,WORK
REAL(EB), POINTER, DIMENSION(:,:,:) :: PRHS
REAL(EB), POINTER, DIMENSION(:,:) :: BXS,BXF,BYS,BYF,BZS,BZF, BXST,BXFT,BYST,BYFT,BZST,BZFT
INTEGER, POINTER :: LSAVE,LWORK,LBC,MBC,NBC,LBC2,MBC2,NBC2,ITRN,JTRN,KTRN,IPS
REAL(EB), POINTER, DIMENSION(:) :: P_0,RHO_0,TMP_0,D_PBAR_DT,D_PBAR_DT_S,U_LEAK,U_WIND,V_WIND,W_WIND
REAL(EB), POINTER, DIMENSION(:,:) :: PBAR,PBAR_S,R_PBAR
INTEGER, POINTER, DIMENSION(:,:,:) :: PRESSURE_ZONE
TYPE (ZONE_MESH_TYPE), POINTER, DIMENSION(:) :: ZONE_MESH
INTEGER, POINTER, DIMENSION(:,:,:) :: MUNKH
REAL(EB), POINTER, DIMENSION(:,:,:) :: WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,WORK8,WORK9
INTEGER, POINTER, DIMENSION(:,:,:) :: IWORK1

REAL(EB),     POINTER, DIMENSION(:,:,:) :: PWORK1,PWORK2,PWORK3,PWORK4
COMPLEX(EB),  POINTER, DIMENSION(:,:,:) :: PWORK5,PWORK6,PWORK7,PWORK8

REAL(EB), POINTER, DIMENSION(:,:,:) :: TURB_WORK1,TURB_WORK2,TURB_WORK3,TURB_WORK4
REAL(EB), POINTER, DIMENSION(:,:,:) :: TURB_WORK5,TURB_WORK6,TURB_WORK7,TURB_WORK8
REAL(EB), POINTER, DIMENSION(:,:,:) :: TURB_WORK9,TURB_WORK10

REAL(EB), POINTER, DIMENSION(:,:,:) :: CCVELDIV,CARTVELDIV

REAL(EB), POINTER, DIMENSION(:) :: WALL_WORK1,WALL_WORK2,FACE_WORK1,FACE_WORK2,FACE_WORK3
REAL(EB), POINTER, DIMENSION(:) :: FIRE_ARRIVAL_TIME,FIRE_RESIDENCE_TIME

REAL(FB), POINTER, DIMENSION(:,:,:,:) :: QQ, QQ2
REAL(FB), POINTER, DIMENSION(:,:) :: PP,PPN,BNDF_TIME_INTEGRAL
INTEGER, POINTER, DIMENSION(:,:) :: IBK
INTEGER, POINTER, DIMENSION(:,:,:) :: IBLK
TYPE(STORAGE_TYPE), POINTER :: WALL_STORAGE,CFACE_STORAGE
REAL(EB), POINTER :: CFL,DIVMX,DIVMN,VN,RESMAX,PART_UVWMAX
INTEGER, POINTER :: ICFL,JCFL,KCFL,IMX,JMX,KMX,IMN,JMN,KMN,I_VN,J_VN,K_VN,IRM,JRM,KRM,DT_RESTRICT_COUNT,DT_RESTRICT_STORE
LOGICAL, POINTER :: CLIP_RHOMIN,CLIP_RHOMAX
LOGICAL, POINTER :: BAROCLINIC_TERMS_ATTACHED
LOGICAL, POINTER :: BNDF_DUMP
LOGICAL, POINTER :: PARTICLES_EXISTED
TYPE(EDGE_TYPE), POINTER, DIMENSION(:) :: EDGE

INTEGER, POINTER :: IBAR,JBAR,KBAR,IBM1,JBM1,KBM1,IBP1,JBP1,KBP1
INTEGER, POINTER :: N_NEIGHBORING_MESHES
INTEGER, POINTER, DIMENSION(:) :: NEIGHBORING_MESH

INTEGER, POINTER, DIMENSION(:) :: RGB
REAL(EB), POINTER :: DXI,DETA,DZETA,RDXI,RDETA,RDZETA, &
   DXMIN,DXMAX,DYMIN,DYMAX,DZMIN,DZMAX, &
   XS,XF,YS,YF,ZS,ZF,RDXINT,RDYINT,RDZINT,CELL_SIZE
REAL(EB), POINTER, DIMENSION(:) :: R,RC,X,Y,Z,XC,YC,ZC,HX,HY,HZ, &
   DX,RDX,DXN,RDXN,DY,RDY,DYN,RDYN,DZ,RDZ,DZN,RDZN, &
   CELLSI,CELLSJ,CELLSK,RRN
INTEGER, POINTER :: CELLSI_LO,CELLSI_HI,CELLSJ_LO,CELLSJ_HI,CELLSK_LO,CELLSK_HI
REAL(FB), POINTER, DIMENSION(:) :: XPLT,YPLT,ZPLT
INTEGER, POINTER :: N_OBST
TYPE(OBSTRUCTION_TYPE), POINTER, DIMENSION(:) :: OBSTRUCTION
INTEGER, POINTER :: N_VENT
TYPE(VENTS_TYPE), POINTER, DIMENSION(:) :: VENTS
INTEGER, POINTER, DIMENSION(:,:,:) :: CELL_INDEX
TYPE(CELL_TYPE), POINTER, DIMENSION(:) :: CELL
REAL(EB), POINTER, DIMENSION(:,:) :: CELL_ILW
INTEGER, POINTER, DIMENSION(:) :: CELL_INTEGERS
LOGICAL, POINTER, DIMENSION(:) :: CELL_LOGICALS
TYPE(CC_REGFACE_TYPE), POINTER, DIMENSION(:) :: REGFACE_IAXIS_H, REGFACE_JAXIS_H, REGFACE_KAXIS_H
INTEGER, POINTER, DIMENSION(:,:,:,:,:) :: FCVAR, ECVAR
INTEGER, POINTER, DIMENSION(:,:,:,:)   :: CCVAR
TYPE(CC_CUTEDGE_TYPE),   POINTER, DIMENSION(:) :: CUT_EDGE
TYPE(CC_CUTFACE_TYPE),   POINTER, DIMENSION(:) :: CUT_FACE
TYPE(CC_CUTCELL_TYPE),   POINTER, DIMENSION(:) :: CUT_CELL
TYPE(CC_REGFACEZ_TYPE),  POINTER, DIMENSION(:) :: CC_REGFACE_IAXIS_Z, CC_REGFACE_JAXIS_Z, CC_REGFACE_KAXIS_Z
TYPE(CC_RCFACE_TYPE), POINTER, DIMENSION(:):: RC_FACE
TYPE(CC_EDGE_TYPE), POINTER, DIMENSION(:):: CC_RCEDGE, CC_IBEDGE
TYPE(CFACE_TYPE), POINTER, DIMENSION(:) :: CFACE
REAL(EB), POINTER, DIMENSION(:,:) :: GEOM_ZMAX
REAL(EB), POINTER, DIMENSION(:) :: EWC_UN_LNK,UN_LNK,UN_ULNK

LOGICAL, POINTER, DIMENSION(:) :: CONNECTED_MESH
INTEGER, POINTER :: N_WALL_CELLS,N_WALL_CELLS_DIM,N_INTERNAL_WALL_CELLS,N_EXTERNAL_WALL_CELLS
INTEGER, POINTER :: N_THIN_WALL_CELLS,N_THIN_WALL_CELLS_DIM
INTEGER, POINTER :: HT_3D_SWEEP_DIRECTION
INTEGER, POINTER ::  N_EXTERNAL_CFACE_CELLS,N_INTWALL_CFACE_CELLS,INTERNAL_CFACE_CELLS_LB,&
                     N_INTERNAL_CFACE_CELLS,N_CFACE_CELLS_DIM
REAL(EB),POINTER :: BC_CLOCK
REAL(EB), POINTER, DIMENSION(:)   :: UVW_SAVE,U_GHOST,V_GHOST,W_GHOST
TYPE(WALL_TYPE), POINTER, DIMENSION(:) :: WALL
TYPE(THIN_WALL_TYPE), POINTER, DIMENSION(:) :: THIN_WALL
TYPE(EXTERNAL_WALL_TYPE), POINTER, DIMENSION(:) :: EXTERNAL_WALL
TYPE(OMESH_TYPE), POINTER, DIMENSION(:) :: OMESH
TYPE(LAGRANGIAN_PARTICLE_TYPE), POINTER, DIMENSION(:) :: LAGRANGIAN_PARTICLE
TYPE(BOUNDARY_ONE_D_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_ONE_D
TYPE(BOUNDARY_THR_D_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_THR_D
TYPE(BOUNDARY_COORD_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_COORD
TYPE(BOUNDARY_PROP1_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_PROP1
TYPE(BOUNDARY_PROP2_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_PROP2
TYPE(BOUNDARY_RADIA_TYPE), POINTER, DIMENSION(:) :: BOUNDARY_RADIA
INTEGER, POINTER :: N_BOUNDARY_COORD_DIM,N_BOUNDARY_ONE_D_DIM,N_BOUNDARY_THR_D_DIM,N_BOUNDARY_PROP1_DIM,N_BOUNDARY_PROP2_DIM,&
                    N_BOUNDARY_RADIA_DIM
INTEGER, POINTER :: NEXT_AVAILABLE_BOUNDARY_COORD_SLOT,NEXT_AVAILABLE_BOUNDARY_ONE_D_SLOT,NEXT_AVAILABLE_BOUNDARY_THR_D_SLOT, &
                    NEXT_AVAILABLE_BOUNDARY_PROP1_SLOT,NEXT_AVAILABLE_BOUNDARY_PROP2_SLOT,NEXT_AVAILABLE_BOUNDARY_RADIA_SLOT
INTEGER, POINTER, DIMENSION(:) :: BOUNDARY_COORD_OCCUPANCY,BOUNDARY_ONE_D_OCCUPANCY,BOUNDARY_THR_D_OCCUPANCY, &
                                  BOUNDARY_PROP1_OCCUPANCY,BOUNDARY_PROP2_OCCUPANCY,BOUNDARY_RADIA_OCCUPANCY,PARTICLE_LAST
INTEGER, POINTER :: NLP,NLPDIM,PARTICLE_TAG
INTEGER, POINTER :: N_SLCF
TYPE(SLICE_TYPE), POINTER, DIMENSION(:) :: SLICE
INTEGER, POINTER :: N_RADF
TYPE(RAD_FILE_TYPE), POINTER, DIMENSION(:) :: RAD_FILE
INTEGER, POINTER :: N_PATCH,N_BNDF_POINTS,N_EXTERIOR_PATCH
TYPE(PATCH_TYPE), POINTER, DIMENSION(:) :: PATCH,EXTERIOR_PATCH
REAL(EB), POINTER, DIMENSION(:,:,:,:) :: UIID
INTEGER,  POINTER :: RAD_CALL_COUNTER,ANGLE_INC_COUNTER
INTEGER, POINTER, DIMENSION(:,:,:) :: INTERPOLATED_MESH
CHARACTER(MESH_STRING_LENGTH), POINTER, DIMENSION(:) :: STRING
INTEGER, POINTER :: N_STRINGS,N_STRINGS_MAX
INTEGER, POINTER, DIMENSION(:,:,:) :: K_AGL_SLICE
INTEGER, POINTER, DIMENSION(:,:) :: LS_KLO_TERRAIN,LS_KHI_TERRAIN,K_LS,LS_SURF_INDEX
INTEGER, POINTER :: N_TERRAIN_SLCF
REAL(EB), POINTER, DIMENSION(:,:) :: FLUX0_LS,FLUX1_LS,PHI_LS,PHI1_LS,ROS_BACKU, &
                                     ROS_HEAD,ROS_FLANK,WIND_EXP, &
                                     SR_X_LS,SR_Y_LS,U_LS,V_LS,Z_LS,DZTDX,DZTDY,MAG_ZT, &
                                     PHI_WS,UMF,THETA_ELPS,PHI_S,PHI_S_X,PHI_S_Y,PHI_W,LS_WORK1,LS_WORK2


CONTAINS


SUBROUTINE POINT_TO_MESH(NM)

! Local names for MESH variables point to Global names

INTEGER, INTENT(IN) ::  NM
TYPE (MESH_TYPE), POINTER :: M=>NULL()

M=>MESHES(NM)
U=>M%U
V=>M%V
W=>M%W
US=>M%US
VS=>M%VS
WS=>M%WS
DDDT=>M%DDDT
D=>M%D
DS=>M%DS
H=>M%H
HS=>M%HS
KRES=>M%KRES
FVX=>M%FVX
FVY=>M%FVY
FVZ=>M%FVZ
FVX_B=>M%FVX_B
FVY_B=>M%FVY_B
FVZ_B=>M%FVZ_B
FVX_D=>M%FVX_D
FVY_D=>M%FVY_D
FVZ_D=>M%FVZ_D
RHO=>M%RHO
RHOS=>M%RHOS
TMP=>M%TMP
CHEM_SUBIT=>M%CHEM_SUBIT
CHEM_ACTIVE_CELLS=>M%CHEM_ACTIVE_CELLS
CHEM_ACTIVE_CC=>M%CHEM_ACTIVE_CC
MU=>M%MU
MU_DNS=>M%MU_DNS
D_Z_MAX=>M%D_Z_MAX
CSD2=>M%CSD2
STRAIN_RATE=>M%STRAIN_RATE
MIX_TIME=>M%MIX_TIME
Q=>M%Q
Q_REAC=>M%Q_REAC
CHI_R => M%CHI_R
QR=>M%QR
QR_W=>M%QR_W
KAPPA_GAS=>M%KAPPA_GAS
UII=>M%UII
PP_RESIDUAL=>M%PP_RESIDUAL
LES_FILTER_WIDTH=>M%LES_FILTER_WIDTH
M_DOT_PPP=>M%M_DOT_PPP
AVG_DROP_DEN=>M%AVG_DROP_DEN
AVG_DROP_AREA=>M%AVG_DROP_AREA
AVG_DROP_TMP=>M%AVG_DROP_TMP
AVG_DROP_RAD=>M%AVG_DROP_RAD
D_SOURCE=>M%D_SOURCE
RSUM=>M%RSUM
ZZ=>M%ZZ
ZZS=>M%ZZS
REAC_SOURCE_TERM=>M%REAC_SOURCE_TERM
DEL_RHO_D_DEL_Z=>M%DEL_RHO_D_DEL_Z
FX=>M%FX
FY=>M%FY
FZ=>M%FZ
POIS_PTB=>M%POIS_PTB
POIS_ERR=>M%POIS_ERR
SAVE1=>M%SAVE1
SAVE2=>M%SAVE2
ADV_FX=>M%ADV_FX
ADV_FY=>M%ADV_FY
ADV_FZ=>M%ADV_FZ
DIF_FX=>M%DIF_FX
DIF_FY=>M%DIF_FY
DIF_FZ=>M%DIF_FZ
DIF_FXS=>M%DIF_FXS
DIF_FYS=>M%DIF_FYS
DIF_FZS=>M%DIF_FZS
SWORK1=>M%SWORK1
SWORK2=>M%SWORK2
SWORK3=>M%SWORK3
SWORK4=>M%SWORK4
WORK=>M%WORK
LSAVE=>M%LSAVE
LWORK=>M%LWORK
PRHS=>M%PRHS
BXS=>M%BXS
BXF=>M%BXF
BYS=>M%BYS
BYF=>M%BYF
BZS=>M%BZS
BZF=>M%BZF
BXST=>M%BXST
BXFT=>M%BXFT
BYST=>M%BYST
BYFT=>M%BYFT
BZST=>M%BZST
BZFT=>M%BZFT
LBC=>M%LBC
MBC=>M%MBC
NBC=>M%NBC
LBC2=>M%LBC2
MBC2=>M%MBC2
NBC2=>M%NBC2
ITRN=>M%ITRN
JTRN=>M%JTRN
KTRN=>M%KTRN
IPS=>M%IPS
U_LEAK=>M%U_LEAK
U_WIND=>M%U_WIND
V_WIND=>M%V_WIND
W_WIND=>M%W_WIND
D_PBAR_DT=>M%D_PBAR_DT
D_PBAR_DT_S=>M%D_PBAR_DT_S
PBAR=>M%PBAR
PBAR_S=>M%PBAR_S
R_PBAR=>M%R_PBAR
P_0=>M%P_0
RHO_0=>M%RHO_0
TMP_0=>M%TMP_0
PRESSURE_ZONE=>M%PRESSURE_ZONE
ZONE_MESH=>M%ZONE_MESH
MUNKH=>M%MUNKH
WORK1=>M%WORK1
WORK2=>M%WORK2
WORK3=>M%WORK3
WORK4=>M%WORK4
WORK5=>M%WORK5
WORK6=>M%WORK6
WORK7=>M%WORK7
WORK8=>M%WORK8
WORK9=>M%WORK9
IWORK1=>M%IWORK1
PWORK1=>M%PWORK1
PWORK2=>M%PWORK2
PWORK3=>M%PWORK3
PWORK4=>M%PWORK4
PWORK5=>M%PWORK5
PWORK6=>M%PWORK6
PWORK7=>M%PWORK7
PWORK8=>M%PWORK8
TURB_WORK1=>M%TURB_WORK1
TURB_WORK2=>M%TURB_WORK2
TURB_WORK3=>M%TURB_WORK3
TURB_WORK4=>M%TURB_WORK4
TURB_WORK5=>M%TURB_WORK5
TURB_WORK6=>M%TURB_WORK6
TURB_WORK7=>M%TURB_WORK7
TURB_WORK8=>M%TURB_WORK8
TURB_WORK9=>M%TURB_WORK9
TURB_WORK10=>M%TURB_WORK10
CCVELDIV=>M%CCVELDIV
CARTVELDIV=>M%CARTVELDIV
WALL_WORK1=>M%WALL_WORK1
WALL_WORK2=>M%WALL_WORK2
FACE_WORK1=>M%FACE_WORK1
FACE_WORK2=>M%FACE_WORK2
FACE_WORK3=>M%FACE_WORK3
FIRE_ARRIVAL_TIME=>M%FIRE_ARRIVAL_TIME
FIRE_RESIDENCE_TIME=>M%FIRE_RESIDENCE_TIME
QQ=>M%QQ
QQ2=>M%QQ2
PP=>M%PP
PPN=>M%PPN
BNDF_TIME_INTEGRAL=>M%BNDF_TIME_INTEGRAL
IBK=>M%IBK
IBLK=>M%IBLK
WALL_STORAGE=>M%WALL_STORAGE
CFACE_STORAGE=>M%CFACE_STORAGE
CFL=>M%CFL
DIVMX=>M%DIVMX
VN=>M%VN
RESMAX=>M%RESMAX
DIVMN=>M%DIVMN
PART_UVWMAX=>M%PART_UVWMAX
ICFL=>M%ICFL
JCFL=>M%JCFL
KCFL=>M%KCFL
CLIP_RHOMIN=>M%CLIP_RHOMIN
CLIP_RHOMAX=>M%CLIP_RHOMAX
DT_RESTRICT_COUNT=>M%DT_RESTRICT_COUNT
DT_RESTRICT_STORE=>M%DT_RESTRICT_STORE
IMX=>M%IMX
JMX=>M%JMX
KMX=>M%KMX
IMN=>M%IMN
JMN=>M%JMN
KMN=>M%KMN
IRM=>M%IRM
JRM=>M%JRM
KRM=>M%KRM
BAROCLINIC_TERMS_ATTACHED=>M%BAROCLINIC_TERMS_ATTACHED
BNDF_DUMP=>M%BNDF_DUMP
PARTICLES_EXISTED=>M%PARTICLES_EXISTED
I_VN=>M%I_VN
J_VN=>M%J_VN
K_VN=>M%K_VN
EDGE=>M%EDGE
IBAR=>M%IBAR
JBAR=>M%JBAR
KBAR=>M%KBAR
IBM1=>M%IBM1
JBM1=>M%JBM1
KBM1=>M%KBM1
IBP1=>M%IBP1
JBP1=>M%JBP1
KBP1=>M%KBP1
N_NEIGHBORING_MESHES=>M%N_NEIGHBORING_MESHES
NEIGHBORING_MESH=>M%NEIGHBORING_MESH
RGB=>M%RGB
DXI=>M%DXI
DETA=>M%DETA
DZETA=>M%DZETA
RDXI=>M%RDXI
RDETA=>M%RDETA
RDZETA=>M%RDZETA
DXMIN=>M%DXMIN
DXMAX=>M%DXMAX
DYMIN=>M%DYMIN
DYMAX=>M%DYMAX
DZMIN=>M%DZMIN
DZMAX=>M%DZMAX
CELL_SIZE=>M%CELL_SIZE
XS=>M%XS
XF=>M%XF
YS=>M%YS
YF=>M%YF
ZS=>M%ZS
ZF=>M%ZF
RDXINT=>M%RDXINT
RDYINT=>M%RDYINT
RDZINT=>M%RDZINT
R=>M%R
RC=>M%RC
X=>M%X
Y=>M%Y
Z=>M%Z
XC=>M%XC
YC=>M%YC
ZC=>M%ZC
HX=>M%HX
HY=>M%HY
HZ=>M%HZ
DX=>M%DX
DY=>M%DY
DZ=>M%DZ
DXN=>M%DXN
DYN=>M%DYN
DZN=>M%DZN
RDX=>M%RDX
RDY=>M%RDY
RDZ=>M%RDZ
RDXN=>M%RDXN
RDYN=>M%RDYN
RDZN=>M%RDZN
CELLSI=>M%CELLSI
CELLSJ=>M%CELLSJ
CELLSK=>M%CELLSK
CELLSI_LO=>M%CELLSI_LO
CELLSJ_LO=>M%CELLSJ_LO
CELLSK_LO=>M%CELLSK_LO
CELLSI_HI=>M%CELLSI_HI
CELLSJ_HI=>M%CELLSJ_HI
CELLSK_HI=>M%CELLSK_HI
RRN=>M%RRN
XPLT=>M%XPLT
YPLT=>M%YPLT
ZPLT=>M%ZPLT
N_OBST=>M%N_OBST
OBSTRUCTION=>M%OBSTRUCTION
N_VENT=>M%N_VENT
VENTS=>M%VENTS
CELL_INDEX=>M%CELL_INDEX
CELL=>M%CELL
CELL_ILW=>M%CELL_ILW
CELL_INTEGERS=>M%CELL_INTEGERS
CELL_LOGICALS=>M%CELL_LOGICALS
REGFACE_IAXIS_H=>M%REGFACE_IAXIS_H
REGFACE_JAXIS_H=>M%REGFACE_JAXIS_H
REGFACE_KAXIS_H=>M%REGFACE_KAXIS_H
ECVAR=>M%ECVAR
FCVAR=>M%FCVAR
CCVAR=>M%CCVAR
CUT_EDGE=>M%CUT_EDGE
CUT_FACE=>M%CUT_FACE
CUT_CELL=>M%CUT_CELL
CC_REGFACE_IAXIS_Z=>M%CC_REGFACE_IAXIS_Z
CC_REGFACE_JAXIS_Z=>M%CC_REGFACE_JAXIS_Z
CC_REGFACE_KAXIS_Z=>M%CC_REGFACE_KAXIS_Z
RC_FACE=>M%RC_FACE
CC_RCEDGE=>M%CC_RCEDGE
CC_IBEDGE=>M%CC_IBEDGE
CFACE=>M%CFACE
GEOM_ZMAX=>M%GEOM_ZMAX
EWC_UN_LNK=>M%EWC_UN_LNK
UN_LNK=>M%UN_LNK
UN_ULNK=>M%UN_ULNK
CONNECTED_MESH=>M%CONNECTED_MESH
N_EXTERNAL_CFACE_CELLS=>M%N_EXTERNAL_CFACE_CELLS
N_INTWALL_CFACE_CELLS=>M%N_INTWALL_CFACE_CELLS
INTERNAL_CFACE_CELLS_LB=>M%INTERNAL_CFACE_CELLS_LB
N_INTERNAL_CFACE_CELLS=>M%N_INTERNAL_CFACE_CELLS
N_CFACE_CELLS_DIM=>M%N_CFACE_CELLS_DIM
N_WALL_CELLS=>M%N_WALL_CELLS
N_WALL_CELLS_DIM=>M%N_WALL_CELLS_DIM
N_THIN_WALL_CELLS=>M%N_THIN_WALL_CELLS
N_THIN_WALL_CELLS_DIM=>M%N_THIN_WALL_CELLS_DIM
HT_3D_SWEEP_DIRECTION=>M%HT_3D_SWEEP_DIRECTION
N_INTERNAL_WALL_CELLS=>M%N_INTERNAL_WALL_CELLS
N_EXTERNAL_WALL_CELLS=>M%N_EXTERNAL_WALL_CELLS
BC_CLOCK=>M%BC_CLOCK
UVW_SAVE=>M%UVW_SAVE
U_GHOST=>M%U_GHOST
V_GHOST=>M%V_GHOST
W_GHOST=>M%W_GHOST
WALL=>M%WALL
THIN_WALL=>M%THIN_WALL
EXTERNAL_WALL=>M%EXTERNAL_WALL
OMESH=>M%OMESH
LAGRANGIAN_PARTICLE =>M%LAGRANGIAN_PARTICLE
BOUNDARY_ONE_D=>M%BOUNDARY_ONE_D
BOUNDARY_THR_D=>M%BOUNDARY_THR_D
BOUNDARY_COORD=>M%BOUNDARY_COORD
BOUNDARY_PROP1=>M%BOUNDARY_PROP1
BOUNDARY_PROP2=>M%BOUNDARY_PROP2
BOUNDARY_RADIA=>M%BOUNDARY_RADIA
N_BOUNDARY_COORD_DIM=>M%N_BOUNDARY_COORD_DIM
N_BOUNDARY_ONE_D_DIM=>M%N_BOUNDARY_ONE_D_DIM
N_BOUNDARY_THR_D_DIM=>M%N_BOUNDARY_THR_D_DIM
N_BOUNDARY_PROP1_DIM=>M%N_BOUNDARY_PROP1_DIM
N_BOUNDARY_PROP2_DIM=>M%N_BOUNDARY_PROP2_DIM
N_BOUNDARY_RADIA_DIM=>M%N_BOUNDARY_RADIA_DIM
NEXT_AVAILABLE_BOUNDARY_COORD_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_COORD_SLOT
NEXT_AVAILABLE_BOUNDARY_ONE_D_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_ONE_D_SLOT
NEXT_AVAILABLE_BOUNDARY_THR_D_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_THR_D_SLOT
NEXT_AVAILABLE_BOUNDARY_PROP1_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_PROP1_SLOT
NEXT_AVAILABLE_BOUNDARY_PROP2_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_PROP2_SLOT
NEXT_AVAILABLE_BOUNDARY_RADIA_SLOT=>M%NEXT_AVAILABLE_BOUNDARY_RADIA_SLOT
BOUNDARY_COORD_OCCUPANCY=>M%BOUNDARY_COORD_OCCUPANCY
BOUNDARY_ONE_D_OCCUPANCY=>M%BOUNDARY_ONE_D_OCCUPANCY
BOUNDARY_THR_D_OCCUPANCY=>M%BOUNDARY_THR_D_OCCUPANCY
BOUNDARY_PROP1_OCCUPANCY=>M%BOUNDARY_PROP1_OCCUPANCY
BOUNDARY_PROP2_OCCUPANCY=>M%BOUNDARY_PROP2_OCCUPANCY
BOUNDARY_RADIA_OCCUPANCY=>M%BOUNDARY_RADIA_OCCUPANCY
NLP=>M%NLP
NLPDIM=>M%NLPDIM
PARTICLE_TAG=>M%PARTICLE_TAG
PARTICLE_LAST=>M%PARTICLE_LAST
N_SLCF=>M%N_SLCF
SLICE=>M%SLICE
N_RADF=>M%N_RADF
RAD_FILE=>M%RAD_FILE
N_PATCH=>M%N_PATCH
N_EXTERIOR_PATCH=>M%N_EXTERIOR_PATCH
N_BNDF_POINTS=>M%N_BNDF_POINTS
PATCH=>M%PATCH
EXTERIOR_PATCH=>M%EXTERIOR_PATCH
UIID=>M%UIID
RAD_CALL_COUNTER=>M%RAD_CALL_COUNTER
ANGLE_INC_COUNTER=>M%ANGLE_INC_COUNTER
INTERPOLATED_MESH => M%INTERPOLATED_MESH
STRING=>M%STRING
N_STRINGS=>M%N_STRINGS
N_STRINGS_MAX=>M%N_STRINGS_MAX
K_AGL_SLICE   =>M%K_AGL_SLICE
LS_KLO_TERRAIN => M%LS_KLO_TERRAIN
LS_KHI_TERRAIN => M%LS_KHI_TERRAIN
N_TERRAIN_SLCF=>M%N_TERRAIN_SLCF
K_LS  =>M%K_LS
LS_SURF_INDEX =>M%LS_SURF_INDEX
FLUX0_LS => M%FLUX0_LS
FLUX1_LS => M%FLUX1_LS
PHI_LS => M%PHI_LS
PHI1_LS => M%PHI1_LS
ROS_BACKU => M%ROS_BACKU
ROS_HEAD => M%ROS_HEAD
ROS_FLANK => M%ROS_FLANK
WIND_EXP => M%WIND_EXP
SR_X_LS => M%SR_X_LS
SR_Y_LS => M%SR_Y_LS
U_LS => M%U_LS
V_LS => M%V_LS
Z_LS => M%Z_LS
DZTDX => M%DZTDX
DZTDY => M%DZTDY
MAG_ZT => M%MAG_ZT
PHI_WS => M%PHI_WS
UMF => M%UMF
THETA_ELPS => M%THETA_ELPS
PHI_S => M%PHI_S
PHI_S_X => M%PHI_S_X
PHI_S_Y => M%PHI_S_Y
PHI_W => M%PHI_W
LS_WORK1 => M%LS_WORK1
LS_WORK2 => M%LS_WORK2

END SUBROUTINE POINT_TO_MESH

END MODULE MESH_POINTERS
