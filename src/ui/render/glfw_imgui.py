# SRC: /d/miur/&/opengl/imgui_bundle/bindings/imgui_bundle/demos_python/demos_immapp/imgui_example_glfw_opengl3.py
#   OFF: https://github.com/ocornut/imgui/blob/master/examples/example_glfw_opengl3/main.cpp
import os.path
import sys

# FAIL: Always import glfw *after* imgui_bundle
#   (since imgui_bundle will set the correct path where to look for the correct version of the glfw dynamic library)
import glfw  # type: ignore
import OpenGL.GL as GL  # type: ignore
from imgui_bundle import imgui


def glfw_error_callback(error: int, description: str) -> None:
    sys.stderr.write(f"Glfw Error {error}: {description}\n")


def main() -> None:
    # Setup window
    glfw.set_error_callback(glfw_error_callback)
    if not glfw.init():
        sys.exit(1)

    # GL 3.0 + GLSL 130
    glsl_version = "#version 130"
    glfw.window_hint(glfw.CONTEXT_VERSION_MAJOR, 3)
    glfw.window_hint(glfw.CONTEXT_VERSION_MINOR, 0)
    # glfw.window_hint(glfw.OPENGL_PROFILE, glfw.OPENGL_CORE_PROFILE) # // 3.2+ only
    # glfw.window_hint(glfw.OPENGL_FORWARD_COMPAT, GL_TRUE)

    window = glfw.create_window(1280, 720, "ImGui+GLFW+OpenGL3", None, None)
    if window is None:
        sys.exit(1)
    glfw.make_context_current(window)
    glfw.swap_interval(1)  # <Enable vsync

    imgui.create_context()
    io = imgui.get_io()
    io.set_ini_filename("")
    io.config_flags |= imgui.ConfigFlags_.nav_enable_keyboard.value
    io.config_flags |= imgui.ConfigFlags_.docking_enable.value

    imgui.style_colors_dark()
    # imgui.style_colors_classic()

    import ctypes

    window_address = ctypes.cast(window, ctypes.c_void_p).value
    assert window_address is not None
    imgui.backends.glfw_init_for_opengl(window_address, True)

    imgui.backends.opengl3_init(glsl_version)

    show_demo_window: bool | None = True
    show_another_window = False
    clear_color = [0.45, 0.55, 0.60, 1.00]
    f = 0.0
    counter = 0

    while not glfw.window_should_close(window):
        glfw.poll_events()

        imgui.backends.opengl3_new_frame()
        imgui.backends.glfw_new_frame()
        imgui.new_frame()

        if show_demo_window:
            _show_demo_window = imgui.show_demo_window(show_demo_window)

        # 2. Show a simple window that we create ourselves. We use a Begin/End pair to created a named window.
        def show_simple_window() -> None:
            nonlocal show_demo_window, show_another_window, clear_color, counter, f
            # static float f = 0.0f;
            # static int counter = 0;
            # NOTE:  Create a window called "Hello, world!" and append into it.
            imgui.begin("Hello, world!")
            imgui.text("This is some useful text.")
            assert show_demo_window is not None
            _, show_demo_window = imgui.checkbox("Demo Window", show_demo_window)
            _, show_another_window = imgui.checkbox(
                "Another Window", show_another_window
            )

            _, f = imgui.slider_float("float", f, 0.0, 1.0)
            _, clear_color = imgui.color_edit4("clear color", clear_color)

            # NOTE: Buttons return true when clicked (most widgets return true when edited/activated)
            if imgui.button("Button"):
                counter += 1

            imgui.same_line()
            imgui.text(f"counter = {counter}")
            fps = imgui.get_io().framerate
            imgui.text(f"App avg={1000.0 / fps:.3f} ms/frame ({fps:.1f} FPS)")
            imgui.end()

        show_simple_window()

        def gui_another_window() -> None:
            nonlocal show_another_window
            if show_another_window:
                # NOTE: Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
                imgui.begin("Another Window", show_another_window)
                imgui.text("Hello from another window!")
                if imgui.button("Close Me"):
                    show_another_window = False
                imgui.end()

        gui_another_window()

        imgui.render()
        display_w, display_h = glfw.get_framebuffer_size(window)
        GL.glViewport(0, 0, display_w, display_h)
        GL.glClearColor(
            clear_color[0] * clear_color[3],
            clear_color[1] * clear_color[3],
            clear_color[2] * clear_color[3],
            clear_color[3],
        )
        GL.glClear(GL.GL_COLOR_BUFFER_BIT)
        # NOTE: the backend rendering is implemented in C++ native functions:
        imgui.backends.opengl3_render_draw_data(imgui.get_draw_data())
        glfw.swap_buffers(window)

    imgui.backends.opengl3_shutdown()
    imgui.backends.glfw_shutdown()
    imgui.destroy_context()
    glfw.destroy_window(window)
    glfw.terminate()


if __name__ == "__main__":
    main()
