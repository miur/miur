# cmake_minimum_required(VERSION 3.15...3.27)
cmake_minimum_required(VERSION 4.2)

## FAIL: doesn't work anymore for cmake>3.30 -- NEED: cmake .. -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=conan_provider.cmake
## Bootstrap the Conan Provider automatically
# set(CONAN_PROVIDER_HASH "648e6df950de16df21a74a3aed70f89b19dc8045")
# set(CONAN_PROVIDER_URL "https://raw.githubusercontent.com/conan-io/cmake-conan/${CONAN_PROVIDER_HASH}/conan_provider.cmake")
# if(NOT EXISTS "${CMAKE_BINARY_DIR}/conan_provider.cmake")
#   message(STATUS "Downloading Conan provider from ${CONAN_PROVIDER_URL}")
#   file(DOWNLOAD "${CONAN_PROVIDER_URL}"
#     "${CMAKE_BINARY_DIR}/conan_provider.cmake"
#     EXPECTED_HASH SHA256=2440641a32f24ad1e7c5d00d3fc4aab74c1d225f5bcdfdfb2972ed85ade672fd  # <OPT
#     STATUS download_status)
#   list(GET download_status 0 status_code)
#   if(NOT status_code EQUAL 0)
#     message(FATAL_ERROR "Failed to download conan_provider.cmake: ${download_status}")
#   endif()
# endif()
# include("${CMAKE_BINARY_DIR}/conan_provider.cmake")


project(miurcache_ext_cc LANGUAGES C)
# project(${SKBUILD_PROJECT_NAME} LANGUAGES C)

## DISABLED: should come from conan toolchain
# set(CMAKE_C_STANDARD 2y)
# set(CMAKE_C_STANDARD_REQUIRED ON)
# set(CMAKE_C_EXTENSIONS OFF)

# Find the Python development headers
find_package(Python REQUIRED COMPONENTS Development.Module)

# WARN: name MUST match the module.name in pyproject.toml
python_add_library(${PROJECT_NAME} MODULE ext_cc/miurcache_core.c)
# target_include_directories(${PROJECT_NAME} PRIVATE inc)
# 2. Rename the output file to "_core" (so it becomes _core.so or _core.pyd)

set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME "_ext_cc")

# RQ:(Python::Module is correct): links on Windows but does nothing on Linux
target_link_libraries(${PROJECT_NAME} PRIVATE Python::Module)

## <conanstuff>
# find_package(OpenSSL REQUIRED)
# target_link_libraries(my_extension PRIVATE OpenSSL::SSL OpenSSL::Crypto)


## Get the include path for a sibling package 'other_ext'
# execute_process(
#     COMMAND ${Python_EXECUTABLE} -c "import other_ext; print(other_ext.get_include())"
#     OUTPUT_VARIABLE OTHER_EXT_INCLUDE
#     OUTPUT_STRIP_TRAILING_WHITESPACE
# )
# target_include_directories(my_extension PRIVATE ${OTHER_EXT_INCLUDE})

## WARN: you must distribute Wheels, not Source Distributions (sdists), because the user won't have Clang 21 or Conan.
## The "Shared Library" Approach (Sibling Packages)
# If you have a core C library used by multiple extensions in your uv workspace, do not link the extensions to each other. Instead:
# * Build the core logic as a Shared Library (libcore.so).
# * Link both extension_a and extension_b to libcore.so.
# * scikit-build-core will see that both extensions need libcore.so and bundle one copy into the wheel.
## The Build Pipeline
# * CI (GitHub Actions): Runs on a clean image (e.g., manylinux_2_34).
# * Build: Runs uv build --wheel.
# * Repair: auditwheel (Linux) or delvewheel (Windows) notices your extension is linked to libssl.so or libcore.so.
# * Bundle: It copies those libs into a subfolder in the wheel (e.g., my_pkg.libs/).
# * Mangle: It renames the libs to include a hash (e.g., libssl-f3a2.so) to avoid conflicts with system libraries.



# 3. Apply the Limited API flag directly to the target
target_compile_definitions(${PROJECT_NAME} PRIVATE
    Py_LIMITED_API=0x030E0000
)

# WARN:(TEMP:DEV only): sanitized binaries run much slower and will crash w/o same rt-lib for user
# WARN:(need LD_PRELOAD): orse crashes as .py allocator doesn't follow ASan's strict tracking rules.
#   USAGE: $ LD_PRELOAD=$(clang -print-file-name=libclang_rt.asan-x86_64.so) python -m pytest
# target_compile_options(${PROJECT_NAME} PRIVATE
#     -fsanitize=address,undefined -fsanitize-address-use-after-scope
#     # -fsanitize=memory -fsanitize-memory-track-origins=2  # <WARN: mutually exclusive
#     -fno-omit-frame-pointer  # BAD: no useful effect on ARM
#     # -funwind-tables -fasynchronous-unwind-tables  # for ARM (v7 and v8)
#     -fno-common
# )

set_target_properties(${PROJECT_NAME} PROPERTIES
  # ALT: SUFFIX ".abi3.so"
  PYTHON_EXT_STABLE_ABI ON  # < tell scikit-build-core to target the Stable ABI (Limited API)
  C_VISIBILITY_PRESET hidden
  VISIBILITY_INLINES_HIDDEN ON
)

# 3. Tell scikit-build-core to put the binary inside the miurcache python folder
install(TARGETS ${PROJECT_NAME} DESTINATION miurcache)
