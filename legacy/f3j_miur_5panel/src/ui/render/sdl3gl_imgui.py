# SRC: /d/miur/&/opengl/PySDL3/opengl.py
# REF: /d/miur/&/opengl/imgui_bundle/bindings/imgui_bundle/python_backends/sdl_backend.py
#   * /d/miur/&/opengl/imgui_bundle/bindings/imgui_bundle/python_backends/examples/example_python_backend_sdl2.py
# USAGE: $ /d/miur/.venv/bin/python ./opengl.py
# ERR: OSError: libjbig.so.0: cannot open shared object file: No such file or directory
#   FIXED: $ ln -vsT /usr/lib/libjbig.so /d/miur/src/ui/render/libjbig.so.0
import colorsys
import ctypes
import os
import sys
import time
from typing import Any

# BAD: it will poison env for all nested apps
#   WTF: why PySDL3 converts .h defines into env vars ???
#   SEE: /d/miur/&/opengl/PySDL3/sdl3/SDL_main_impl.py
os.environ["SDL_MAIN_NOIMPL"] = "1"

import OpenGL.GL as gl
import sdl3
from imgui_bundle import imgui
from imgui_bundle.python_backends.opengl_backend import ProgrammablePipelineRenderer


class SDL3Renderer(ProgrammablePipelineRenderer):
    MOUSE_WHEEL_OFFSET_SCALE = 0.5

    def __init__(self, window: sdl3.LP_SDL_Window) -> None:
        super().__init__()

        self.window = window

        def get_clipboard_text(_imgui_context: Any) -> str:
            r: ctypes.c_char_p = SDL_GetClipboardText()
            return r.decode() if r else ""

        def set_clipboard_text(_imgui_context: Any, text: str) -> None:
            SDL_SetClipboardText(ctypes.c_char_p(text.encode()))

        imgui.get_platform_io().platform_get_clipboard_text_fn = get_clipboard_text
        imgui.get_platform_io().platform_set_clipboard_text_fn = set_clipboard_text

        sdl3.SDL_StartTextInput(window)

        # self.io.key_map[imgui.KEY_SPACE] = sdl3.SDL_SCANCODE_SPACE
        # self.io.key_map[imgui.KEY_ENTER] = sdl3.SDL_SCANCODE_RETURN
        # self.io.key_map[imgui.KEY_ESCAPE] = sdl3.SDL_SCANCODE_ESCAPE
        # self.io.key_map[imgui.KEY_PAD_ENTER] = sdl3.SDL_SCANCODE_KP_ENTER
        # self.io.key_map[imgui.KEY_S] = sdl3.SDL_SCANCODE_S

    def processEvent(self, event: sdl3.SDL_Event) -> None:
        if event.type in [sdl3.SDL_EVENT_MOUSE_WHEEL]:
            self.io.mouse_wheel = event.wheel.y * self.MOUSE_WHEEL_OFFSET_SCALE

        if event.type in [
            sdl3.SDL_EVENT_MOUSE_BUTTON_UP,
            sdl3.SDL_EVENT_MOUSE_BUTTON_DOWN,
        ]:
            buttons = [
                sdl3.SDL_BUTTON_LEFT,
                sdl3.SDL_BUTTON_RIGHT,
                sdl3.SDL_BUTTON_MIDDLE,
            ]

            if event.button.button in buttons:
                self.io.mouse_down[buttons.index(event.button.button)] = (
                    event.type == sdl3.SDL_EVENT_MOUSE_BUTTON_DOWN
                )

        if event.type in [sdl3.SDL_EVENT_MOUSE_MOTION]:
            self.io.mouse_pos = (
                (event.motion.x, event.motion.y)
                if sdl3.SDL_GetWindowFlags(self.window) & sdl3.SDL_WINDOW_MOUSE_FOCUS
                else (-1, -1)
            )

        if event.type in [sdl3.SDL_EVENT_KEY_UP, sdl3.SDL_EVENT_KEY_DOWN]:
            if event.key.scancode < sdl3.SDL_SCANCODE_COUNT:
                # self.io.keys_down[event.key.scancode] = (
                #     event.type == sdl3.SDL_EVENT_KEY_DOWN
                # )
                pass

            self.io.key_shift = (sdl3.SDL_GetModState() & sdl3.SDL_KMOD_SHIFT) != 0
            self.io.key_ctrl = (sdl3.SDL_GetModState() & sdl3.SDL_KMOD_CTRL) != 0
            self.io.key_alt = (sdl3.SDL_GetModState() & sdl3.SDL_KMOD_ALT) != 0
            self.io.key_super = (sdl3.SDL_GetModState() & sdl3.SDL_KMOD_GUI) != 0

        if event.type in [sdl3.SDL_EVENT_TEXT_INPUT]:
            for char in event.text.text.decode("utf-8"):
                self.io.add_input_character(ord(char))


def err(msg: str) -> int:
    print(  # OR: sdl3.SDL_Log(
        msg + ": " + sdl3.SDL_GetError().decode().lower(),
        file=sys.stderr,
        flush=True,
    )
    return -1


def main() -> int:
    if not sdl3.SDL_Init(sdl3.SDL_INIT_VIDEO | sdl3.SDL_INIT_EVENTS):
        return err("failed to initialize library")

    sdl3attrs = [
        (sdl3.SDL_GL_CONTEXT_MAJOR_VERSION, 4),
        (sdl3.SDL_GL_CONTEXT_MINOR_VERSION, 6),
        (sdl3.SDL_GL_CONTEXT_PROFILE_MASK, sdl3.SDL_GL_CONTEXT_PROFILE_COMPATIBILITY),
        (sdl3.SDL_GL_CONTEXT_FLAGS, sdl3.SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG),
    ]
    for k, v in sdl3attrs:
        sdl3.SDL_GL_SetAttribute(k, v)

    wndflags = sdl3.SDL_WINDOW_OPENGL | sdl3.SDL_WINDOW_RESIZABLE
    if not (window := sdl3.SDL_CreateWindow("miur".encode(), 1200, 900, wndflags)):
        return err("failed to create window")
    if not (context := sdl3.SDL_GL_CreateContext(window)):
        return err("failed to create context")
    sdl3.SDL_GL_MakeCurrent(window, context)

    imgui.create_context()
    imgui.get_io().set_ini_filename("")

    renderer = SDL3Renderer(window)
    event = sdl3.SDL_Event()
    running = True
    while running:

        # NOTE: process inputs
        # currentTime = sdl3.SDL_GetTicks() / 1000.0
        # OFF:(base class) self.io = imgui.get_io()
        width, height = ctypes.c_int(0), ctypes.c_int(0)
        sdl3.SDL_GetWindowSize(window, ctypes.byref(width), ctypes.byref(height))
        renderer.io.display_size = (width.value, height.value)

        while sdl3.SDL_PollEvent(ctypes.byref(event)):
            renderer.processEvent(event)
            match event.type:
                case sdl3.SDL_EVENT_QUIT:
                    running = False
                case sdl3.SDL_EVENT_KEY_DOWN:
                    if event.key.key in [sdl3.SDLK_ESCAPE, sdl3.SDLK_Q]:
                        running = False

        gl.glClearColor(0.0, 0.0, 1.0, 1.0)
        gl.glClear(gl.GL_COLOR_BUFFER_BIT)

        imgui.new_frame()
        imgui.show_demo_window()
        imgui.end_frame()
        imgui.render()

        renderer.render(imgui.get_draw_data())

        sdl3.SDL_GL_SwapWindow(window)

    renderer.shutdown()
    imgui.destroy_context()

    sdl3.SDL_GL_MakeCurrent(window, None)
    sdl3.SDL_GL_DestroyContext(context)
    sdl3.SDL_DestroyWindow(window)
    sdl3.SDL_Quit()
    return 0


if __name__ == "__main__":
    main()
