cmake_minimum_required(VERSION 3.24 FATAL_ERROR)

project(
    fds
    VERSION 6.9.1
    LANGUAGES Fortran C
)
enable_language(Fortran C)

option(DUMP_JSON           "Add the option to dump info to JSON"      OFF)

option(USE_OPENMP          "Use OpenMP"                               ON)

# The existing FDS makefile links MKL statically, but this is optional and can
# be changed to dynamic
set(MKL_LINK static CACHE STRING "Linking method for MKL (static or dynamic)")
set_property(CACHE MKL_LINK PROPERTY STRINGS static dynamic)

add_executable(fds
    FDS_Source/cons.f90
    FDS_Source/ctrl.f90
    FDS_Source/devc.f90
    FDS_Source/divg.f90
    FDS_Source/dump.f90
    FDS_Source/evac.f90
    FDS_Source/fire.f90
    FDS_Source/func.f90
    FDS_Source/hvac.f90
    FDS_Source/ieva.f90
    FDS_Source/init.f90
    FDS_Source/irad.f90
    FDS_Source/isob.c
    FDS_Source/main.f90
    FDS_Source/mass.f90
    FDS_Source/mesh.f90
    FDS_Source/mpip.f90
    FDS_Source/part.f90
    FDS_Source/pois.f90
    FDS_Source/prec.f90
    FDS_Source/pres.f90
    FDS_Source/radi.f90
    FDS_Source/read.f90
    FDS_Source/scrc.f90
    FDS_Source/smvv.f90
    FDS_Source/turb.f90
    FDS_Source/type.f90
    FDS_Source/vege.f90
    FDS_Source/velo.f90
    FDS_Source/wall.f90
)

if (DUMP_JSON)
    target_sources(fds PRIVATE FDS_Source/json.f90)
endif()

target_include_directories(fds PRIVATE .)

# Get various properties about the time and git revision. These can be
# overridden which is important for building in situations where we don't have
# the git repo.
if (NOT(WIN32) AND NOT(BUILD_DATE_XLF))
    string(TIMESTAMP BUILD_DATE_XLF "%b %d\\, %Y  %H:%M:%S")
endif()
if (NOT(BUILD_DATE))
    string(TIMESTAMP BUILD_DATE "%b %d, %Y  %H:%M:%S ")
endif()
if (NOT(GIT_DATE))
    execute_process(COMMAND git log -1 --format=%cd          OUTPUT_VARIABLE GIT_DATE       OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
if (NOT(GIT_BRANCH))
    execute_process(COMMAND git rev-parse --abbrev-ref HEAD  OUTPUT_VARIABLE GIT_BRANCH     OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
if (NOT(GIT_HASH))
    execute_process(COMMAND git describe --long --abbrev=7   OUTPUT_VARIABLE GIT_HASH       OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
if (NOT(GIT_DIRTY))
    execute_process(COMMAND git diff --shortstat Source/*.f90 OUTPUT_VARIABLE GIT_DIRTY)
endif()
if (GIT_DIRTY STREQUAL "")
    set(GIT_STAT "")
else()
    set(GIT_STAT "-dirty")
endif()

target_compile_definitions(fds PRIVATE BUILDDATE_PP="${BUILD_DATE}")
target_compile_definitions(fds PRIVATE GITHASH_PP="${GIT_HASH}${GIT_STAT}-${GIT_BRANCH}")
target_compile_definitions(fds PRIVATE GITDATE_PP="${GIT_DATE}")

# Use MPI (mandatory)
find_package(MPI REQUIRED)
target_link_libraries(fds PRIVATE MPI::MPI_Fortran)

# FDS use lp64 (4-byte integer), MKL often defaults to ilp64 (8-byte integer)
set(MKL_INTERFACE lp64)
# Turn on BLACS
set(ENABLE_BLACS ON)
# We don't need ScaLAPACK
set(ENABLE_SCALAPACK OFF)
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

if (USE_OPENMP)
    # Use OpenMP
    find_package(OpenMP REQUIRED)
    target_link_libraries(fds PUBLIC OpenMP::OpenMP_Fortran)
endif()

if (WIN32)
    target_compile_definitions(fds PRIVATE pp_noappend)
endif()

# Set compiler flags for various compilers
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fds PRIVATE -cpp -std=f2018 -frecursive -ffpe-summary=none -fall-intrinsics)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    target_compile_options(fds PRIVATE -fpp )
    # When compiling with Intel on Linux, enable the IFPORT library
    if (LINUX)
        target_compile_definitions(fds PRIVATE USE_IFPORT)
    endif()
    # When compiling with Intel and not on Windows apply -no-wrap-margin.
    if (NOT(WIN32))
        target_compile_options(fds PRIVATE -no-wrap-margin)
    endif()
endif()

# If we are using the old Intel Fortran compiler (ifort) suppress the warning
# that this is an old compiler.
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
    target_compile_options(fds PRIVATE /Qdiag-disable:10448)
endif()

if(WIN32)
    set(BUILD_SHARED_LIBS OFF)
endif()

if (DUMP_JSON)
    target_compile_definitions(fds PRIVATE DUMP_JSON)
    if (NOT WIN32 AND USE_SYSTEM_JSON)
        # On non-Windows platforms use the system version of json-fortran
        find_package(PkgConfig REQUIRED)
        pkg_check_modules(json-fortran REQUIRED IMPORTED_TARGET json-fortran)
        target_link_libraries(fds PRIVATE PkgConfig::json-fortran)
        target_include_directories(fds PRIVATE /usr/lib64/gfortran/modules)
    else()
        include(FetchContent)
        # There are currently issues with the version of json-fortran provided by
        # vcpkg, therefore we fetch and build json-fortran ourselves.
        FetchContent_Declare(
            jsonfortran
            GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
            GIT_TAG        a012a4dc5c6f69b736157d70e45ba7820c5bd23f
        )
        set(ENABLE_TESTS OFF CACHE INTERNAL "Turn off json-fortran tests")
        FetchContent_MakeAvailable(jsonfortran)
        add_dependencies(fds jsonfortran-static)
        target_link_libraries(fds PRIVATE jsonfortran-static)
        # This is a workaround due to where json-fortran places its module files
        target_include_directories(fds PRIVATE ${jsonfortran_BINARY_DIR})
    endif()
endif()

install(TARGETS fds)

include(CTest)
enable_testing()
add_test(NAME "FDS Executes"
    COMMAND fds)
