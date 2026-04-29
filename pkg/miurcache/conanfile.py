from conan import ConanFile
from conan.tools.cmake import cmake_layout


class MiurCacheRecipe(ConanFile):
    # name = "miurcache"
    # version = "0.1.0"
    languages = "C", "C++"
    ## Standard settings for a compiled C extension
    settings = ("os", "compiler", "build_type", "arch")
    # Generate the CMake toolchain (for compiler/flags) and dependencies
    generators = "CMakeDeps", "CMakeToolchain"

    # Do not hardcode C23 here.
    # Let the user's profile or CMakeLists handle the language standard.

    def layout(self) -> None:
        # NOTE: build_folder="build" is the default, but being explicit is better
        # cmake_layout(self, build_folder="build")
        # cmake_layout(self)
        # CRITICAL FIX: 'build_folder="."' flattens the hierarchy.
        # This stops Conan from appending "build/Release" to your output folder,
        # perfectly aligning it with CMakePresets and scikit-build-core.
        cmake_layout(self, build_folder=".")
        # cmake_layout(self, build_folder="build/default")
        # cmake_layout(self, build_folder="build/{build_type}")

        # Forces all generated files (like conan_toolchain.cmake) into a less-nested subfolder
        # self.folders.generators = "build/conan"
        # self.folders.generators = "generators"
        self.folders.generators = "."

        ## TRY: redir to :/out/<pkg>/
        # # We tell Conan the build folder is actually two levels up in 'out'
        # # This matches the scikit-build-core 'build-dir'
        # self.folders.build = "../../out/my_package_name"
        # self.folders.build = "build"
        #
        # # This puts conan_toolchain.cmake and generators in 'out/<pkg>/conan'
        # self.folders.generators = f"{self.folders.build}/conan"
        #
        # # Standard layout logic for source
        # self.folders.source = "."

    # def requirements(self):
    #     # If you strictly want Conan to supply the Python 3.14 headers
    #     # (rather than relying on system headers or uv environments),
    #     # uncomment the line below. Otherwise, let CMake's FindPython handle it.
    #     # self.requires("cpython/3.14.0")
    #     pass
    #     # self.requires("openssl/3.2.1")  # Example dependency
    #     # self.requires("my_internal_lib/1.0.0")
