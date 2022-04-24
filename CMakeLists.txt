cmake_minimum_required(VERSION 3.21)
cmake_policy(SET CMP0091 NEW)
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
project(fds LANGUAGES Fortran)

# set_property(GLOBAL PROPERTY C_STANDARD 99)

find_package(MKL CONFIG)
if (MKL_FOUND)
message(STATUS "${MKL_IMPORTED_TARGETS}")
get_target_property(MKLS MKL::MKL INTERFACE_COMPILE_OPTIONS)
message(STATUS "MKL Compile Options: ${MKLS}")
get_target_property(MKLS MKL::MKL INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "MKL Include Directories: ${MKLS}")
endif()
find_package(OpenMP)

add_executable(fds
    Source/ccib.f90
    Source/cons.f90
    Source/ctrl.f90
    Source/data.f90
    Source/devc.f90
    Source/divg.f90
    Source/dump.f90
    Source/fire.f90
    Source/func.f90
    Source/geom.f90
    Source/gsmv.f90
    Source/hvac.f90
    Source/imkl.f90
    Source/init.f90
    Source/main.f90
    Source/mass.f90
    Source/mesh.f90
    Source/part.f90
    Source/prec.f90
    Source/pois.f90
    Source/pres.f90
    Source/radi.f90
    Source/rcal.f90
    Source/read.f90
    Source/scrc.f90
    Source/smvv.f90
    Source/soot.f90
    Source/turb.f90
    Source/type.f90
    Source/vege.f90
    Source/velo.f90
    Source/wall.f90
)

# target_compile_options(fds PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
# target_include_directories(fds PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
# target_link_libraries(fds PUBLIC $<LINK_ONLY:MKL::MKL>)

# Link OpenMP
if(OpenMP_Fortran_FOUND)
    target_link_libraries(fds PUBLIC OpenMP::OpenMP_Fortran)
else()
    message(STATUS "OpenMP was not found. OpenMP will not be linked.")
endif()

# add_test(NAME mytest COMMAND myapp)
# if(MKL_ENV)
#   set_tests_properties(mytest PROPERTIES ENVIRONMENT "${MKL_ENV}")
# endif()


# ifeq ($(shell echo "check_quotes"),"check_quotes")
# # windows
#   GIT_HASH := $(shell ..\Scripts\githash)
#   GIT_STAT := $(shell ..\Scripts\gitstat)
#   GIT_DATE := $(shell ..\Scripts\gitlog)
#   GIT_BRANCH := $(shell ..\Scripts\gitbranch)
#   BUILD_DATE := $(shell date /t) $(shell time /t)
# else
# # linux/osx
#   SHELL:=/bin/bash
#   GIT_HASH := $(shell git describe --long)
#   ifeq ("$(shell git diff --shortstat $(VPATH)/*.f90)","")
#     GIT_STAT :=
#   else
#     GIT_STAT := -dirty
#   endif
#   GIT_DATE := $(shell git log -1 --format=%cd)
#   GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
#   BUILD_DATE := $(shell date "+%b %d, %Y  %T")
# # The preprocessor used by xlf uses comma as separation between preprocessing commands.
# # Commas needs to be escaped if they are to be used as part of a value in a preprocessing variable.
#   BUILD_DATE_XLF := $(shell date "+%b %d\, %Y  %T")
# endif

# GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)$(GIT_STAT)-$(GIT_BRANCH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
# GITINFO=-fpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
# GITINFOGNU=-cpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
# GITINFOXLF=-qsuffix=cpp=f90 '-WF,-DGITHASH_PP=\"$(GIT_HASH)\",-DGITDATE_PP=\"$(GIT_DATE)\",-DBUILDDATE_PP=\"$(BUILD_DATE_XLF)\"'
# GITINFOPGI=-Mpreprocess $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
# GFORTRAN_OPTIONS=

# ifeq ($(shell echo "check_quotes"),"check_quotes")
# # windows
#   INTELMPI_COMPVERSION := $(shell ..\Scripts\intel_compversion $(INTEL_IFORT))
#   GNU_COMPVERSION := $(shell ..\Scripts\gnu_compversion)
# else
# # linux/osx
#   INTELMPI_COMPVERSION := $(shell ../Scripts/intelmpi_compversion.sh $(INTEL_IFORT))
#   OPENMPI_COMPVERSION := $(shell ../Scripts/openmpi_compversion.sh)
#   GNU_COMPVERSION := $(shell ../Scripts/gnu_compversion.sh)
#   ifeq ($(shell ../Scripts/gnu_compmajorversion.sh),10)
#     GFORTRAN_OPTIONS=-fallow-argument-mismatch
#   endif
# endif

# INTELMPI_COMPINFO=-DCOMPVER_PP=\"$(INTELMPI_COMPVERSION)\"
# OPENMPI_COMPINFO=-DCOMPVER_PP=\"$(OPENMPI_COMPVERSION)\"
# GNU_COMPINFO=-DCOMPVER_PP=\"$(GNU_COMPVERSION)\"

# FFLAGSMKL_INTEL =
# LFLAGSMKL_INTEL =
# FFLAGSMKL_OPENMPI =
# LFLAGSMKL_OPENMPI =
# FFLAGSMKL_CUSTOM =
# LFLAGSMKL_CUSTOM =
# ifdef MKLROOT # This assumes the MKL library is available.
# ifeq ($(shell echo "check_quotes"),"check_quotes")
# # windows
# ifeq ($(shell ..\Scripts\havemkl),1)
# # Use cluster_sparse_solver with intelmpi blacs
#   FFLAGSMKL_INTEL = -DWITH_MKL /I"$(MKLROOT)"\include
#   LFLAGSMKL_INTEL = "$(MKLROOT)"\lib\intel64\mkl_intel_lp64.lib "$(MKLROOT)"\lib\intel64\mkl_intel_thread.lib "$(MKLROOT)"\lib\intel64\mkl_core.lib "$(MKLROOT)"\lib\intel64\mkl_blacs_intelmpi_lp64.lib
# endif
# else
# # linux/osx
# ifneq ("$(wildcard ${MKLROOT}/lib/intel64/libmkl_blacs_intelmpi_lp64.a)","")
# # Use cluster_sparse_solver with intelmpi blacs
#   FFLAGSMKL_INTEL = -DWITH_MKL -I$(MKLROOT)/include
#   LFLAGSMKL_INTEL = -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_blacs_intelmpi_lp64.a -Wl,--end-group -lpthread -lm -ldl
# endif
# ifneq ("$(wildcard ${MKLROOT}/lib/intel64/libmkl_blacs_openmpi_lp64.a)","")
# # Use cluster_sparse_solver with openmpi blacs
#   FFLAGSMKL_OPENMPI = -DWITH_MKL -I$(MKLROOT)/include
#   LFLAGSMKL_OPENMPI = -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_blacs_openmpi_lp64.a -Wl,--end-group -lpthread -lm -ldl
#   FFLAGSMKL_GNU_OPENMPI = -DWITH_MKL -I$(MKLROOT)/include
#   LFLAGSMKL_GNU_OPENMPI =  -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_lp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_blacs_openmpi_lp64.a -Wl,--end-group -lgomp -lpthread -lm -ldl
# endif
# ifneq ("$(wildcard ${MKLROOT}/lib/libmkl_blacs_custom_lp64.a)","")
# # Use cluster_sparse_solver with custom blacs
#   FFLAGSMKL_CUSTOM = -DWITH_MKL -I$(MKLROOT)/include
# # MKL_LIBS needs to occur 3 times in order to avoid unresolved external reference warnings
#   MKL_LIBS = ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_core.a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_blacs_custom_lp64.a
#   LFLAGSMKL_CUSTOM = ${MKL_LIBS} ${MKL_LIBS} ${MKL_LIBS} -lpthread -lm -ldl
#   FFLAGSMKL_GNU_CUSTOM = -DWITH_MKL -I$(MKLROOT)/include
#   LFLAGSMKL_GNU_CUSTOM =  ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_sequential.a ${MKLROOT}/lib/libmkl_core.a ${MKLROOT}/lib/libmkl_blacs_custom_lp64.a -lpthread -lm -ldl
# endif
# endif # OS test
# endif # MKLROOT test

target_compile_options(fds PRIVATE -m64 -cpp)#$(GITINFOGNU) $(GNU_COMPINFO) $(FFLAGSMKL_GNU_OPENMPI) $(GFORTRAN_OPTIONS))

# GNU
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
target_compile_options(fds PRIVATE -O2 -std=f2018 -frecursive -ffpe-summary=none -fall-intrinsics)#$(GITINFOGNU) $(GNU_COMPINFO) $(FFLAGSMKL_GNU_OPENMPI) $(GFORTRAN_OPTIONS))
endif()
# Intel
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
set(I_IFORT ifort)
target_compile_options(fds PRIVATE -fc=${I_IFORT} -O2 -ipo -no-wrap-margin -DUSE_IFPORT)
endif()