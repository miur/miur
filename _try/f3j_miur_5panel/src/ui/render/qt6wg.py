# %USAGE: $ /d/miur/.venv/bin/python qt.py
import os
import sys

os.environ["QT_ACCESSIBILITY"] = "0"
os.environ["NO_AT_BRIDGE"] = "1"
os.environ["QT_NO_DBUS"] = "1"

# REF: https://www.pythonguis.com/faq/pyqt6-vs-pyside6/
# %USAGE: from .qt import QtGui, QtWidgets, QtCore, _enum, _exec
# ALT:SEE: /usr/lib/python3.12/site-packages/IPython/external/qt_for_kernel.py
try:
    if "PyQt6" not in sys.modules:
        import PyQt6
except ModuleNotFoundError:
    from PySide6 import QtCore, QtGui, QtWidgets
    from PySide6.QtCore import Signal, Slot
else:
    from PyQt6 import QtCore, QtDBus, QtGui, QtWidgets
    from PyQt6.QtCore import pyqtSignal as Signal
    from PyQt6.QtCore import pyqtSlot as Slot

# REF: https://www.pythonguis.com/faq/pyqt6-vs-pyside6/
# def _enum(obj, name):
#     """%USAGE: _enum(PySide6.QtCore.Qt, 'Alignment.AlignLeft')"""
#     parent, child = name.split(".")
#     result = getattr(obj, child, False)
#     if result:  # Found using short name only.
#         return result
#
#     obj = getattr(obj, parent)  # Get parent, then child.
#     return getattr(obj, child)
#
#
# def _exec(obj):
#     """%USAGE: app = QApplication(sys.argv); _exec(app)"""
#     if hasattr(obj, "exec"):
#         return obj.exec()
#     else:
#         return obj.exec_()


# class CustomButton(QPushButton)
#     def mousePressEvent(self, e):
#         e.accept()
#
# class CustomButton(QPushButton)
#     def event(self, e):
#         e.ignore()


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.button_is_checked = True
        self.setWindowTitle("My App")
        self.setMouseTracking(True)

        self.button = QtWidgets.QPushButton("Press Me!")
        self.button.setCheckable(True)
        self.button.clicked.connect(self.the_button_was_clicked)
        self.button.clicked.connect(self.the_button_was_toggled)
        self.button.released.connect(self.the_button_was_released)
        self.button.setChecked(self.button_is_checked)

        self.label = QtWidgets.QLabel()
        self.input = QtWidgets.QLineEdit()
        # self.input.setMaxLength(10)
        # self.input.setPlaceholderText("Enter your text")
        self.input.textChanged.connect(self.label.setText)
        # self.input.returnPressed.connect(self.return_pressed)
        # self.input.selectionChanged.connect(self.selection_changed)
        # self.input.textEdited.connect(self.text_edited)

        widget = QtWidgets.QListWidget()
        widget.addItems(["One", "Two", "Three"])
        widget.currentItemChanged.connect(self.index_changed)
        widget.currentTextChanged.connect(self.text_changed)

        layout = QtWidgets.QVBoxLayout()
        layout.addWidget(self.input)
        layout.addWidget(self.label)
        layout.addWidget(self.button)
        layout.addWidget(widget)

        container = QtWidgets.QWidget()
        container.setLayout(layout)
        self.setCentralWidget(container)

    def the_button_was_clicked(self) -> None:
        print("Clicked!")
        self.button.setText("You already clicked me.")
        # self.button.setEnabled(False)

    def the_button_was_toggled(self, checked) -> None:
        print("Checked?", checked)

    def the_button_was_released(self) -> None:
        self.button_is_checked = self.button.isChecked()
        print(self.button_is_checked)

    def mouseMoveEvent(self, e) -> None:
        self.label.setText("mouseMoveEvent")

    def mousePressEvent(self, e) -> None:
        self.label.setText(f"mousePressEvent: {e.button()}")
        # OPT: super(self, MainWindow).contextMenuEvent(event)

    def mouseReleaseEvent(self, e) -> None:
        self.label.setText("mouseReleaseEvent")

    def mouseDoubleClickEvent(self, e) -> None:
        self.label.setText("mouseDoubleClickEvent")

    ## ALT: signals inof override
    # self.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
    # self.customContextMenuRequested.connect(self.on_context_menu)
    # def on_context_menu(self, pos):
    #     context = QMenu(self)
    #     context.addAction(QAction("test 1", self))
    #     context.addAction(QAction("test 2", self))
    #     context.addAction(QAction("test 3", self))
    #     context.exec(self.mapToGlobal(pos))

    def contextMenuEvent(self, e) -> None:
        context = QtWidgets.QMenu(self)
        context.addAction(QtGui.QAction("test 1", self))
        context.addAction(QtGui.QAction("test 2", self))
        context.addAction(QtGui.QAction("test 3", self))
        context.exec(e.globalPos())

    def index_changed(self, item):
        print(item.text())

    def text_changed(self, s: str):
        print(s)


def main() -> int:
    app = QtWidgets.QApplication([])  # sys.argv
    window = MainWindow()
    window.show()
    return app.exec()
    # app.quit()


if __name__ == "__main__":
    main()

# WARN:FATAL: new GUI thread should be never destroyed!
#   FAIL: use asyncio from !miur main thread
#     COS: you never know if there are new events until you call QApplication.processEvents()
#       # loop = asyncio.get_running_loop()
#       # loop.add_reader(QT_FD, QApplication.processEvents)
#   ALT: reuse Qt mainloop for asyncio, and then set previous asyncio loop back
#     BAD~ we need to migrate all already registered tasks in the loop forth'n'back
# while True:
# main()

# REF: https://stackoverflow.com/questions/1386043/how-to-make-qt-work-when-main-thread-is-busy
#   :: main thread of a process can be different from the main Qt thread (which instantiates the first Qt object)
# ERR: WARNING: QApplication was not created in the main() thread
#   FIXED: should run Qt *imports* also in the new thread!
#   SRC: https://stackoverflow.com/questions/22289423/how-to-avoid-qt-app-exec-blocking-main-thread/22290909#comment90345367_22290909
#     FAIL:(MacOSX): due to restrictions in the Cocoa framework, the main thread MUST be the GUI thread.
#       OFF: https://bugreports.qt.io/browse/QTBUG-7393
#   ALT:(integ): run_in_gui_thread(new RunEventImpl([](){ QMainWindow* window=new QMainWindow(); window->show(); }));
# from threading import Thread
# thread1 = Thread(target=main)
# thread1.start()
# thread1.join()


## NOTE: Disable the D-Bus connection
#  $ pstree -at
#   └─python qt.py
#       ├─{QDBusConnection}
#       ├─{QXcbEventQueue}
#       └─4*[{Thread (pooled)}]
# BAD: threads are created immediately above
#   ~~ rest of Qt may still init D-Bus in bkgr
#   ALT: export QT_NO_DBUS=1  (ALSO:SEE: QT_QPA_PLATFORM_PLUGIN_PATH)
#   ALT: disable DBus for non-GUI apps
#   app = QtCore.QCoreApplication(sys.argv)
# app = QtWidgets.QApplication(sys.argv)
# # print(QtDBus.QDBusConnection.sessionBus().name())
# # print(QtDBus.QDBusConnection.systemBus().name())
# QtDBus.QDBusConnection.disconnectFromBus("qt_default_session_bus")
# QtDBus.QDBusConnection.disconnectFromBus("qt_default_system_bus")
# # WTF: =True
# print(QtDBus.QDBusConnection.sessionBus().isConnected())
# print(QtDBus.QDBusConnection.systemBus().isConnected())
# window = MainWindow()
# window.show()
# app.exec()

# async def mainloop_asyncio() -> None:
#     app = QtWidgets.QApplication(sys.argv)
#     window = MainWindow()
#     window.show()
#
#     # WaitForMoreEvents         = ...  # 0x4
#     # EventLoopExec             = ...  # 0x20
#     # mflags = WaitForMoreEvents | EventLoopExec
#     mflags = (
#         QtCore.QEventLoop.ProcessEventsFlag.WaitForMoreEvents
#         # | PySide6.QtCore.QEventLoop.ProcessEventsFlag.EventLoopExec
#     )
#     while True:
#         app.processEvents(mflags)
#
#
# import asyncio
#
# asyncio.run(mainloop_asyncio())
