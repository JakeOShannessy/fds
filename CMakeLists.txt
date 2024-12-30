cmake_minimum_required(VERSION 3.24 FATAL_ERROR)

project(
    fds
    VERSION 8.5.2
    LANGUAGES Fortran
)
enable_language(Fortran)

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
    # Source/json.f90
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

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_compile_options(fds PRIVATE -m64 -O2 -std=f2018 -frecursive -ffpe-summary=none -fall-intrinsics -cpp   -DBUILDDATE_PP="BUILD_DATE"  )
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    # target_compile_options(fds PRIVATE -D_WIN32 /Qipo /O2  /wrap-margin-   -DBUILDDATE_PP="BUILD_DATE" )
endif()
target_include_directories(fds PRIVATE .)
target_link_libraries(fds PRIVATE MPI::MPI_Fortran)

# Link OpenMP
if(OpenMP_Fortran_FOUND)
    target_link_libraries(fds PUBLIC OpenMP::OpenMP_Fortran)
else()
    message(STATUS "OpenMP was not found. OpenMP will not be linked.")
endif()

# if ( _json-fortran_FOUND)
#     find_package(PkgConfig REQUIRED)
#     pkg_check_modules(json-fortran REQUIRED IMPORTED_TARGET json-fortran)
#     target_link_libraries(fds PRIVATE PkgConfig::json-fortran)
#     target_include_directories(fds PRIVATE /usr/lib64/gfortran/modules)
# else()
#     include(FetchContent)
#     FetchContent_Declare(
#         jsonfortran
#         GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran.git
#         GIT_TAG        3ab8f98209871875325c6985dd0e50085d1c82c2
#         )
#     set(ENABLE_TESTS OFF CACHE INTERNAL "Turn off json-fortran tests")
#     FetchContent_MakeAvailable(jsonfortran)
#     add_dependencies(fds jsonfortran-static)
#     target_link_libraries(fds PRIVATE jsonfortran-static)
# endif()
install(TARGETS fds)


# include(CTest)
# enable_testing()
# add_subdirectory(Tests)
