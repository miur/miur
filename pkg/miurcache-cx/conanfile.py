from conan import ConanFile
from conan.tools.cmake import cmake_layout


class MyExtension(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    # Do not hardcode C23 here.
    # Let the user's profile or CMakeLists handle the language standard.

    def layout(self):
        # NOTE: build_folder="build" is the default, but being explicit is better
        # cmake_layout(self, build_folder="build")
        cmake_layout(self)
        # self.folders.build = "build"

        # This puts conan_toolchain.cmake into build/conan/
        self.folders.generators = "build/conan"

        ## TRY: redir to :/out/<pkg>/
        # # We tell Conan the build folder is actually two levels up in 'out'
        # # This matches the scikit-build-core 'build-dir'
        # self.folders.build = "../../out/my_package_name"
        #
        # # This puts conan_toolchain.cmake and generators in 'out/<pkg>/conan'
        # self.folders.generators = f"{self.folders.build}/conan"
        #
        # # Standard layout logic for source
        # self.folders.source = "."

    # def requirements(self):
    #     self.requires("openssl/3.2.1")  # Example dependency
    #     # self.requires("my_internal_lib/1.0.0")
