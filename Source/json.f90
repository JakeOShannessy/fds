
MODULE JSON_FORMAT

USE PRECISION_PARAMETERS
USE MESH_VARIABLES
USE GLOBAL_CONSTANTS
USE TRAN
USE MESH_POINTERS
USE OUTPUT_DATA
USE COMP_FUNCTIONS, ONLY: CHECKREAD, SHUTDOWN, CHECK_XB, SCAN_INPUT_FILE
USE MEMORY_FUNCTIONS, ONLY: ChkMemErr,REALLOCATE2D,REALLOCATE
USE COMP_FUNCTIONS, ONLY: GET_INPUT_FILE
USE MISC_FUNCTIONS, ONLY: SEARCH_CONTROLLER,WRITE_SUMMARY_INFO
USE HVAC_ROUTINES, ONLY: READ_HVAC,PROC_HVAC
USE COMPLEX_GEOMETRY, ONLY: READ_GEOM
USE MPI_F08
USE THERMO_PROPS

IMPLICIT NONE (TYPE,EXTERNAL)
PRIVATE

PUBLIC PRINT_JSON

CONTAINS

SUBROUTINE PRINT_JSON(JSON_OUTPUT_PATH)
    use,intrinsic :: iso_fortran_env, only: wp => real64
    use :: json_module, rk => json_rk
    USE DEVICE_VARIABLES, ONLY: DEVICE_TYPE,DEVICE,N_DEVC,PROPERTY_TYPE,N_PROP,PROPERTY

    CHARACTER(FN_LENGTH), INTENT(IN)  :: JSON_OUTPUT_PATH
    INTEGER :: N, NM, NQ
    TYPE (MESH_TYPE), POINTER :: M
    TYPE (DEVICE_TYPE), POINTER :: DV
    TYPE (PROPERTY_TYPE), POINTER :: PY
    TYPE (LAGRANGIAN_PARTICLE_CLASS_TYPE), POINTER :: LPC=>NULL()
    TYPE (OBSTRUCTION_TYPE), POINTER :: OB=>NULL()
    TYPE (REACTION_TYPE),POINTER :: RN=>NULL()
    TYPE(SURFACE_TYPE),POINTER:: SF=>NULL()
    TYPE(VENTS_TYPE), POINTER :: VT=>NULL()


    type(json_core) :: json
    type(json_value),pointer :: p, surfaces_obj, vents_obj, meshes_obj, obj, \
       obj2, obj3, devices_obj, obj4

    ! initialize the class
    call json%initialize()

    ! initialize the structure:
    call json%create_object(p,'')

    call json%add(p, 'version', '0.1.0')

    call json%add(p, 'chid', TRIM(CHID))

    call json%add(p, 'ec_ll', EC_LL)
    call json%add(p, 'visibility_factor', VISIBILITY_FACTOR)

    call json%create_object(obj,'dump')
    call json%add(p, obj)
    call json%add(obj, 'nframes', NFRAMES)
    nullify(obj)
    ! DT_BNDF      = -1._EB
    ! DT_CPU       =  HUGE(EB)
    ! DT_CTRL      = -1._EB
    ! DT_DEVC      = -1._EB
    ! DT_FLUSH     = -1._EB
    ! DT_GEOM      =  HUGE(EB)
    ! DT_HRR       = -1._EB
    ! DT_HVAC      = -1._EB
    ! DT_ISOF      = -1._EB
    ! DT_MASS      = -1._EB
    ! DT_PART      = -1._EB
    ! DT_PL3D      =  HUGE(EB)
    ! IF (DT_PL3D_SPE /= HUGE(EB)) THEN
    !    call json%add(obj, 'dt_pl3d', DT_PL3D)
    ! ENDIF

    ! DT_PROF      = -1._EB
    ! DT_RADF      =  HUGE(EB)
    ! DT_RESTART   =  HUGE(EB)
    ! DT_SLCF      = -1._EB
    ! IF (DT_SLCF /= -1._EB) THEN
    !    call json%add(obj, 'dt_slcf', DT_PL3D)
    ! ENDIF
    ! DT_SL3D      =  (T_END-T_BEGIN)/5._EB
    ! DT_SMOKE3D   = -1._EB
    ! DT_UVW       =  HUGE(EB)
    ! DT_TMP       =  HUGE(EB)
    ! DT_SPEC      =  HUGE(EB)

    call json%create_object(obj,'time')
    call json%add(p, obj)
    call json%add(obj, 'begin', T_BEGIN)
    call json%add(obj, 'end', T_END)
    nullify(obj)

    call json%create_array(surfaces_obj,'reacs')
    call json%add(p, surfaces_obj)
    DO N=1,N_REACTIONS
       RN => REACTION(N)
       call json%create_object(obj,'reac')
       call json%add(surfaces_obj, obj)
       call json%add(obj, 'index', N)
       call json%add(obj, 'id', TRIM(RN%ID))
       call json%add(obj, 'equation', TRIM(RN%ID))
       call json%add(obj, 'c', RN%C)
       call json%add(obj, 'h', RN%H)
       call json%add(obj, 'n', RN%N)
       call json%add(obj, 'o', RN%O)
       call json%add(obj, 'epumo2', RN%EPUMO2)
       call json%add(obj, 'heat_of_combustion', RN%HEAT_OF_COMBUSTION)
       call json%add(obj, 'soot_yield', RN%SOOT_YIELD)
       call json%add(obj, 'co_yield', RN%CO_YIELD)
       nullify(obj)
    ENDDO
    nullify(surfaces_obj)

    ! add an "surfaces" object to the structure:
    call json%create_array(surfaces_obj,'surfaces')
    call json%add(p, surfaces_obj)
    ! surfaces start from zero as the special INERT surface is 0
    DO N=0,N_SURF
       SF => SURFACE(N)
       call json%create_object(obj,'surface')
       call json%add(surfaces_obj, obj)
       call json%add(obj, 'index', N)
       call json%add(obj, 'id', TRIM(SF%ID))
       IF (SF%HRRPUA /= 0.0) THEN
          call json%add(obj, 'hrrpua', SF%HRRPUA)
       ENDIF
       IF (SF%VOLUME_FLOW /= 0.0) THEN
          call json%add(obj, 'volume_flow', SF%VOLUME_FLOW)
       ENDIF
       IF (SF%VEL /= 0.0) THEN
          call json%add(obj, 'vel', SF%VEL)
       ENDIF
       if (SF%TMP_FRONT /= -1._EB) THEN
          call json%add(obj, 'tmp_front', SF%TMP_FRONT)
       ENDIF
       IF ( SF%RAMP(TIME_HEAT)%TAU /= TAU_DEFAULT) THEN
          call json%add(obj, 'tau_q', SF%RAMP(TIME_HEAT)%TAU)
       ENDIF
       IF (LEN_TRIM(SF%FYI) /= 0 .AND. TRIM(SF%FYI) /= 'null') THEN
          call json%add(obj, 'fyi', TRIM(SF%FYI))
       ENDIF
       nullify(obj)
    ENDDO
    nullify(surfaces_obj)


    call json%create_array(surfaces_obj,'props')
    call json%add(p, surfaces_obj)
    DO N=1,N_PROP
       PY => PROPERTY(N)
       call json%create_object(obj,'prop')
       call json%add(surfaces_obj, obj)
       ! call json%add(obj, 'index', N)
       call json%add(obj, 'id', TRIM(PY%ID))
       if (TRIM(PY%SPEC_ID) /= 'null') then
          call json%add(obj, 'part_id', TRIM(PY%PART_ID))
       endif
       if (TRIM(PY%SPEC_ID) /= 'null') then
          call json%add(obj, 'spec_id', TRIM(PY%SPEC_ID))
       endif
       call json%add(obj, 'quantity', TRIM(PY%QUANTITY))
       call json%add(obj, 'activation_temperature', PY%ACTIVATION_TEMPERATURE)
       call json%add(obj, 'activation_obscuration', PY%ACTIVATION_OBSCURATION)
       call json%add(obj, 'flow_rate', PY%FLOW_RATE)
       call json%add(obj, 'particle_velocity', PY%PARTICLE_VELOCITY)
       nullify(obj)
    ENDDO
    nullify(surfaces_obj)

    call json%create_array(surfaces_obj,'parts')
    call json%add(p, surfaces_obj)
    DO N=1,N_LAGRANGIAN_CLASSES
       LPC => LAGRANGIAN_PARTICLE_CLASS(N)
       call json%create_object(obj,'part')
       call json%add(surfaces_obj, obj)
       call json%add(obj, 'index', N)
       call json%add(obj, 'id', TRIM(LPC%ID))
       call json%add(obj, 'spec_id', TRIM(LPC%SPEC_ID))
       call json%add(obj, 'devc_id', TRIM(LPC%DEVC_ID))
       call json%add(obj, 'ctrl_id', TRIM(LPC%CTRL_ID))
       call json%add(obj, 'surf_id', TRIM(LPC%SURF_ID))
       call json%add(obj, 'prop_id', TRIM(LPC%PROP_ID))
       call json%add(obj, 'diameter',  LPC%DIAMETER)
       call json%add(obj, 'monodisperse',  LPC%MONODISPERSE)
       call json%add(obj, 'age',  LPC%LIFETIME)
       call json%add(obj, 'sampling_factor',  LPC%SAMPLING_FACTOR)
       nullify(obj)
    ENDDO
    nullify(surfaces_obj)

    call json%create_array(meshes_obj,'meshes')
    call json%add(p, meshes_obj)

    DO NM=1,NMESHES
       M => MESHES(NM)
       call json%create_object(obj,'mesh')
       call json%add(meshes_obj, obj)
       call json%add(obj, 'index', NM)
       call json%add(obj, 'id', TRIM(MESH_NAME(NM)))

       call json%create_object(obj3,'ijk')
       call json%add(obj, obj3)
       call json%add(obj3, 'i', M%IBAR)
       call json%add(obj3, 'j', M%JBAR)
       call json%add(obj3, 'k', M%KBAR)
       nullify(obj3)

       call json%create_object(obj3,'dimensions')
       call json%add(obj, obj3)
       call json%add(obj3, 'x1', M%XS)
       call json%add(obj3, 'x2', M%XF)
       call json%add(obj3, 'y1', M%YS)
       call json%add(obj3, 'y2', M%YF)
       call json%add(obj3, 'z1', M%ZS)
       call json%add(obj3, 'z2', M%ZF)
       nullify(obj3)

       call json%create_object(obj3,'cell_sizes')
       call json%add(obj, obj3)
       call json%add(obj3, 'dx', M%DXMIN)
       ! call json%add(obj3, 'dx_max', M%DXMAX)
       call json%add(obj3, 'dy', M%DYMIN)
       ! call json%add(obj3, 'dy_max', M%DYMAX)
       call json%add(obj3, 'dz', M%DZMIN)
       ! call json%add(obj3, 'dz_max', M%DZMAX)
       nullify(obj3)

       if (M%N_VENT > 0) THEN
          call json%create_array(vents_obj,'vents')
          call json%add(obj, vents_obj)
          PRINT_VENT_LOOP: DO N=1,M%N_VENT
             VT => MESHES(NM)%VENTS(N)
             call json%create_object(obj2,'vent')
             call json%add(vents_obj, obj2)
             call json%add(obj2, 'index', N)
             call json%add(obj2, 'id', TRIM(VT%ID))
             call json%add(obj2, 'surface', TRIM(SURFACE(VT%SURF_INDEX)%ID))
             call json%add(obj2, 'devc_id', TRIM(VT%DEVC_ID))
             call json%add(obj2, 'ctrl_id', TRIM(VT%CTRL_ID))

             call json%create_object(obj3,'dimensions')
             call json%add(obj2, obj3)
             call json%add(obj3, 'x1', VT%X1)
             call json%add(obj3, 'x2', VT%X2)
             call json%add(obj3, 'y1', VT%Y1)
             call json%add(obj3, 'y2', VT%Y2)
             call json%add(obj3, 'z1', VT%Z1)
             call json%add(obj3, 'z2', VT%Z2)
             nullify(obj3)

             call json%add(obj2, 'fds_area', VT%FDS_AREA)

             nullify(obj2)
          ENDDO  PRINT_VENT_LOOP
          nullify(vents_obj)
       ENDIF

       if (M%N_OBST > 0) THEN
          call json%create_array(vents_obj,'obsts')
          call json%add(obj, vents_obj)
          DO N=1,M%N_OBST
             OB => MESHES(NM)%OBSTRUCTION(N)
             call json%create_object(obj2,'obst')
             call json%add(vents_obj, obj2)
             call json%add(obj2, 'index', N)
             call json%add(obj2, 'id', TRIM(OB%ID))

             call json%create_object(obj3,'surfaces')
             call json%add(obj2, obj3)
             call json%add(obj3, 'x_min', TRIM(SURFACE(OB%SURF_INDEX(-3))%ID))
             call json%add(obj3, 'x_max', TRIM(SURFACE(OB%SURF_INDEX(-2))%ID))
             call json%add(obj3, 'y_min', TRIM(SURFACE(OB%SURF_INDEX(-1))%ID))
             call json%add(obj3, 'y_max', TRIM(SURFACE(OB%SURF_INDEX(1))%ID))
             call json%add(obj3, 'z_min', TRIM(SURFACE(OB%SURF_INDEX(2))%ID))
             call json%add(obj3, 'z_max', TRIM(SURFACE(OB%SURF_INDEX(3))%ID))
             nullify(obj3)

             call json%create_object(obj3,'input_area')
             call json%add(obj2, obj3)
             call json%add(obj3, 'x', OB%INPUT_AREA(1))
             call json%add(obj3, 'y', OB%INPUT_AREA(2))
             call json%add(obj3, 'z', OB%INPUT_AREA(3))
             nullify(obj3)

             call json%create_object(obj3,'fds_area')
             call json%add(obj2, obj3)
             call json%add(obj3, 'x', OB%FDS_AREA(1))
             call json%add(obj3, 'y', OB%FDS_AREA(2))
             call json%add(obj3, 'z', OB%FDS_AREA(3))
             nullify(obj3)


             if (TRIM(OB%DEVC_ID) /= 'null') then
                call json%add(obj2, 'devc_id', TRIM(OB%DEVC_ID))
             endif
             if (TRIM(OB%CTRL_ID) /= 'null') then
                call json%add(obj2, 'ctrl_id', TRIM(OB%CTRL_ID))
             endif

             call json%create_object(obj3,'dimensions')
             call json%add(obj2, obj3)
             call json%add(obj3, 'x1', OB%X1)
             call json%add(obj3, 'x2', OB%X2)
             call json%add(obj3, 'y1', OB%Y1)
             call json%add(obj3, 'y2', OB%Y2)
             call json%add(obj3, 'z1', OB%Z1)
             call json%add(obj3, 'z2', OB%Z2)
             nullify(obj3)

             call json%create_object(obj3,'bounds')
             call json%add(obj2, obj3)
             call json%add(obj3, 'i_min', OB%I1)
             call json%add(obj3, 'i_max', OB%I2)
             call json%add(obj3, 'j_min', OB%J1)
             call json%add(obj3, 'j_max', OB%J2)
             call json%add(obj3, 'k_min', OB%K1)
             call json%add(obj3, 'k_max', OB%K2)
             nullify(obj3)


             ! call json%add(obj2, 'fds_area', VT%FDS_AREA)

             nullify(obj2)
          ENDDO
          nullify(vents_obj)
       ENDIF

       nullify(obj)
    ENDDO
    nullify(meshes_obj)

    call json%create_array(devices_obj,'devices')
    call json%add(p, devices_obj)

    DO N=1,N_DEVC
       DV => DEVICE(N)
       call json%create_object(obj,'device')
       call json%add(devices_obj, obj)
       call json%add(obj, 'index', N)
       call json%add(obj, 'id', TRIM(DV%ID))
       call json%add(obj, 'label', TRIM(DV%SMOKEVIEW_LABEL))
       if (TRIM(DV%SPATIAL_STATISTIC) /= 'null') then
          call json%add(obj, 'spatial_statistic', TRIM(DV%SPATIAL_STATISTIC))
       endif
       if (TRIM(DV%SPEC_ID) /= 'null') then
          call json%add(obj, 'spec_id', TRIM(DV%SPEC_ID))
       endif
       if (TRIM(DV%PROP_ID) /= 'null') then
          call json%add(obj, 'prop_id', TRIM(DV%PROP_ID))
       endif
       call json%add(obj, 'mesh',  DV%MESH)
       call json%add(obj, 'setpoint',  DV%SETPOINT)

       call json%create_object(obj3,'dimensions')
       call json%add(obj, obj3)
       call json%add(obj3, 'x1', DV%X1)
       call json%add(obj3, 'x2', DV%X2)
       call json%add(obj3, 'y1', DV%Y1)
       call json%add(obj3, 'y2', DV%Y2)
       call json%add(obj3, 'z1', DV%Z1)
       call json%add(obj3, 'z2', DV%Z2)
       nullify(obj3)

       call json%create_object(obj3,'location')
       call json%add(obj, obj3)
       call json%add(obj3, 'x', DV%X)
       call json%add(obj3, 'y', DV%Y)
       call json%add(obj3, 'z', DV%Z)
       nullify(obj3)

       call json%create_array(obj2,'quantities')
       call json%add(obj, obj2)
       DO NQ=1,DV%N_QUANTITY
          call json%create_string(obj3,TRIM(DV%QUANTITY(NQ)),'quantity')
          call json%add(obj2, obj3)
          nullify(obj3)
       ENDDO
       nullify(obj2)

       call json%create_array(obj2,'points')
       call json%add(obj, obj2)
       M => MESHES(DV%MESH)
       DO NQ=1,DV%N_POINTS
          call json%create_object(obj3,'point')
          call json%add(obj2, obj3)
          call json%add(obj3, 'i', DV%I(NQ))
          call json%add(obj3, 'j', DV%J(NQ))
          call json%add(obj3, 'k', DV%K(NQ))
          ! Is the cell this point is in solid at the start of the simulation?
          call json%add(obj3, 'init_solid', M%CELL(M%CELL_INDEX(DV%I(NQ),DV%J(NQ),DV%K(NQ)))%SOLID)
          ! Is the cell above this point solid at the start of the simulation?
          if (DV%K(NQ) < M%KBAR) THEN
             call json%add(obj3, 'init_solid_zplus', M%CELL(M%CELL_INDEX(DV%I(NQ),DV%J(NQ),DV%K(NQ)+1))%SOLID)
          ENDIF
          nullify(obj3)
       ENDDO
       nullify(obj2)

       nullify(obj)
    ENDDO
    nullify(devices_obj)

    ! write the file:
    if (JSON_OUTPUT_PATH == '-') then
       IF (MY_RANK==0) WRITE(LU_ERR,'(A)') ' Outputting JSON info to stdout'
       call json%print(p)
    else
       IF (MY_RANK==0) WRITE(LU_ERR,'(A,A)') ' Outputting JSON info to ', JSON_OUTPUT_PATH
       call json%print(p,JSON_OUTPUT_PATH)
    endif

    !cleanup:
    call json%destroy(p)
END SUBROUTINE PRINT_JSON

END MODULE JSON_FORMAT
