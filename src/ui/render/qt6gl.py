# SRC: https://stackoverflow.com/questions/72689874/a-minimal-opengl-example-in-pyqt6-does-not-work-error-invalid-operation-in-g
#   BET: https://code.qt.io/cgit/qt/qtbase.git/tree/examples/opengl/hellogles3/glwindow.cpp
import sys

from OpenGL import GL as gl

# from PyQt6.QtOpenGL import (
#     QOpenGLBuffer,
#     QOpenGLShader,
#     QOpenGLShaderProgram,
#     QOpenGLTexture,
# )
from PyQt6.QtOpenGLWidgets import QOpenGLWidget
from PyQt6.QtWidgets import QApplication


class Widget(QOpenGLWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("PyQt6, OpenGL 3.3")
        self.resize(400, 400)

    def initializeGL(self):
        gl.glClearColor(0.2, 0.2, 1.0, 1)
        # self.program.addShaderFromSourceCode(QOpenGLShader.ShaderTypeBit.Vertex, vertShaderSrc)
        # self.program.addShaderFromSourceCode(QOpenGLShader.ShaderTypeBit.Fragment, fragShaderSrc)
        # self.texture = QOpenGLTexture(QOpenGLTexture.Target.Target2D)
        # self.texture.setMinMagFilters(QOpenGLTexture.Filter.Linear, QOpenGLTexture.Filter.Linear)
        # self.texture.setWrapMode(QOpenGLTexture.WrapMode.ClampToEdge)

    def paintGL(self):
        gl.glClear(gl.GL_COLOR_BUFFER_BIT)


def main() -> int:
    app = QApplication(sys.argv)
    # app.setAttribute(Qt.ApplicationAttribute.AA_UseDesktopOpenGL)
    w = Widget()
    w.show()
    return app.exec()


if __name__ == "__main__":
    main()
