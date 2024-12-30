cmake_minimum_required(VERSION 3.24 FATAL_ERROR)

project(
    fds
    VERSION 6.9.1
    LANGUAGES Fortran
)
enable_language(Fortran)

option(USE_HYPRE           "Use the hypre library"                    OFF)
option(USE_SYSTEM_HYPRE    "Use the hypre library from the system"    OFF)

option(USE_SUNDIALS        "Use the sundials library"                 OFF)
option(USE_SYSTEM_SUNDIALS "Use the sundials library from the system" OFF)

# The existing FDS makefile links MKL statically, but this is optional and can
# be changed to dynamic
option(STATIC_MKL          "Statically link MKL"                      ON)

add_executable(fds
    Source/main.f90
    Source/prec.f90
    Source/cons.f90
    Source/chem.f90
    Source/prop.f90
    Source/devc.f90
    Source/type.f90
    Source/data.f90
    Source/mesh.f90
    Source/func.f90
    Source/gsmv.f90
    Source/smvv.f90
    Source/rcal.f90
    Source/turb.f90
    Source/soot.f90
    Source/pois.f90
    Source/geom.f90
    Source/ccib.f90
    Source/radi.f90
    Source/part.f90
    Source/vege.f90
    Source/ctrl.f90
    Source/hvac.f90
    Source/mass.f90
    Source/imkl.f90
    Source/wall.f90
    Source/fire.f90
    Source/velo.f90
    Source/pres.f90
    Source/init.f90
    Source/dump.f90
    Source/read.f90
    Source/divg.f90
)
target_include_directories(fds PRIVATE .)

# Get various properties about the time and git revision
execute_process(COMMAND date "+%b %d\\, %Y  %T"          OUTPUT_VARIABLE BUILD_DATE_XLF OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND date "+%b %d, %Y  %T"            OUTPUT_VARIABLE BUILD_DATE     OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND git log -1 --format=%cd          OUTPUT_VARIABLE GIT_DATE       OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND git rev-parse --abbrev-ref HEAD  OUTPUT_VARIABLE GIT_BRANCH     OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND git describe --long --abbrev=7   OUTPUT_VARIABLE GIT_HASH       OUTPUT_STRIP_TRAILING_WHITESPACE)

# TODO: this will only change on configure
execute_process(COMMAND git diff --shortstat Source/*.f90 OUTPUT_VARIABLE GIT_DIRTY)
if (GIT_DIRTY STREQUAL "")
    set(GIT_STAT "")
else()
    set(GIT_STAT "-dirty")
endif()

target_compile_options(fds PRIVATE -m64 -cpp)
target_compile_definitions(fds PRIVATE BUILDDATE_PP="${BUILD_DATE}")
target_compile_definitions(fds PRIVATE GITHASH_PP="${GIT_HASH}${GIT_STAT}-${GIT_BRANCH}")
target_compile_definitions(fds PRIVATE GITDATE_PP="${GIT_DATE}")

# Use MPI (mandatory)
find_package(MPI REQUIRED)
target_link_libraries(fds PRIVATE MPI::MPI_Fortran)

# FDS use lp64 (4-byte integer), MKL often defaults to ilp64 (8-byte integer)
set(MKL_INTERFACE lp64)
# If we have elected to link MKL statically, set that option
if(STATIC_MKL)
    set(MKL_LINK static)
endif()
# Turn on BLACS
set(ENABLE_BLACS ON)
# Use MKL if found
find_package(MKL CONFIG)
if (MKL_FOUND)
    message(STATUS "${MKL_IMPORTED_TARGETS}")
    get_target_property(MKLS MKL::MKL INTERFACE_COMPILE_OPTIONS)
    message(STATUS "MKL Compile Options: ${MKLS}")
    get_target_property(MKLS MKL::MKL INTERFACE_INCLUDE_DIRECTORIES)
    message(STATUS "MKL Include Directories: ${MKLS}")
    target_compile_definitions(fds PRIVATE WITH_MKL)
    target_link_libraries(fds PRIVATE MKL::MKL)
endif()

# Use OpenMP if found
find_package(OpenMP)
if(OpenMP_Fortran_FOUND)
    target_link_libraries(fds PUBLIC OpenMP::OpenMP_Fortran)
else()
    message(STATUS "OpenMP was not found. OpenMP will not be linked.")
endif()

# Set compiler flags for various compilers
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fds PRIVATE -O2 -std=f2018 -frecursive -ffpe-summary=none -fall-intrinsics)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    target_compile_options(fds PRIVATE -O2 -no-wrap-margin)
    # When compiling with Intel on Linux, enable the IFPORT library
    if (LINUX)
        target_compile_definitions(fds PRIVATE USE_IFPORT)
    endif()
endif()


# Find or build sundials if the option is set to use it
if(USE_SUNDIALS)
    # Unless forced to use the system version, download and build sundials
    if(NOT(USE_SYSTEM_SUNDIALS))
        include(FetchContent)
        # Set some options for building sundials
        set(ENABLE_MPI ON)
        set(BUILD_FORTRAN_MODULE_INTERFACE ON)
        FetchContent_Declare(
            SUNDIALS
            GIT_REPOSITORY https://github.com/LLNL/sundials.git
            GIT_TAG        aaeab8d907c6b7dfca86041401fdc1448f35f826 # v6.7.0
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(SUNDIALS)
    endif()

    # Use sundials if found
    if(SUNDIALS_FOUND)
        find_package(SUNDIALS REQUIRED)
        target_link_libraries(fds PRIVATE SUNDIALS::fcvode_mod)
        target_link_libraries(fds PRIVATE SUNDIALS::fnvecserial_mod)
        target_compile_definitions(fds PRIVATE WITH_SUNDIALS)
        target_compile_definitions(fds PRIVATE SUNDIALS_PP="${SUNDIALS_VERSION}")
    endif()
endif()

# Find or build hypre if the option is set to use it
if(USE_HYPRE)
    # Unless forced to use the system version, download and build hypre
    if(NOT(USE_SYSTEM_HYPRE))
        include(FetchContent)
        FetchContent_Declare(
            HYPRE
            GIT_REPOSITORY https://github.com/hypre-space/hypre.git
            GIT_TAG        cb7597ffd998dc5270c2e32025e799e68048b1cd # v2.32.0
            SOURCE_SUBDIR  src
            OVERRIDE_FIND_PACKAGE
        )
        FetchContent_MakeAvailable(HYPRE)
    endif()

    # Use hypre if found
    find_package(HYPRE)
    if(HYPRE_FOUND)
        target_link_libraries(fds PRIVATE HYPRE)
        target_compile_definitions(fds PRIVATE WITH_HYPRE)
        target_compile_definitions(fds PRIVATE HYPRE_PP="${HYPRE_VERSION}")
    endif()
endif()

install(TARGETS fds)
