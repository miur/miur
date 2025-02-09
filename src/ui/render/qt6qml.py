import os.path as fs
import sys

from PyQt6.QtCore import QUrl
from PyQt6.QtGui import QGuiApplication
from PyQt6.QtQuick import QQuickView


def main() -> int:
    app = QGuiApplication([])
    view = QQuickView()
    ## OR:
    # view.engine().addImportPath(fs.dirname(__file__))
    # view.loadFromModule("qml", "qt6qml")
    qmlpath = fs.join(fs.dirname(__file__), "qml", "qt6qml.qml")
    view.setSource(QUrl.fromLocalFile(qmlpath))
    # view.setGeometry(100, 100, 400, 240)
    view.show()
    # view.quit.connect(app.quit)
    rc = app.exec()
    # del view
    sys.exit(rc)


if __name__ == "__main__":
    main()
