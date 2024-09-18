cmake_minimum_required(VERSION 3.24 FATAL_ERROR)

project(
    fds
    VERSION 8.5.2
    LANGUAGES Fortran
)
enable_language(Fortran)
include(FetchContent)

find_package(MPI REQUIRED)

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
    Source/json.f90
    Source/wall.f90
    Source/fire.f90
    Source/velo.f90
    Source/pres.f90
    Source/init.f90
    Source/dump.f90
    Source/read.f90
    Source/divg.f90
)

# BUILD_DATE := $(shell date "+%b %d, %Y  %T")
# GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)$(GIT_STAT)-$(GIT_BRANCH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
# GITINFOGNU=-cpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
set(BUILD_DATE "BUILD_DATE")

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fds PRIVATE -m64 -O2 -std=f2018)
    target_compile_options(fds PRIVATE -frecursive -ffpe-summary=none -fall-intrinsics)
    target_compile_options(fds PRIVATE -cpp -DBUILDDATE_PP="${BUILD_DATE}")
    # target_compile_options(fds PRIVATE -DGITHASH_PP="$(GIT_HASH)$(GIT_STAT)-$(GIT_BRANCH)" -DGITDATE_PP="$(GIT_DATE)")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    # target_compile_options(fds PRIVATE -D_WIN32 /Qipo /O2  /wrap-margin-   -DBUILDDATE_PP="BUILD_DATE" )
endif()
target_include_directories(fds PRIVATE .)
target_link_libraries(fds PRIVATE MPI::MPI_Fortran)


if (NOT WIN32)
    # On non-Windows platforms use the system version of json-fortran
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(json-fortran REQUIRED IMPORTED_TARGET json-fortran)
    target_link_libraries(fds PRIVATE PkgConfig::json-fortran)
    target_include_directories(fds PRIVATE /usr/lib64/gfortran/modules)
else()
    # There are currently issues with the version fo json-fortran provided by
    # vcpkg, therefore we fetch and build json-fortran ourselves.
    FetchContent_Declare(
        jsonfortran
        GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
        GIT_TAG        3ab8f98209871875325c6985dd0e50085d1c82c2
        )
    set(ENABLE_TESTS OFF CACHE INTERNAL "Turn off json-fortran tests")
    FetchContent_MakeAvailable(jsonfortran)
    add_dependencies(fds jsonfortran-static)
    target_link_libraries(fds PRIVATE jsonfortran-static)
endif()
install(TARGETS fds)


include(CTest)
enable_testing()
add_subdirectory(Tests)
