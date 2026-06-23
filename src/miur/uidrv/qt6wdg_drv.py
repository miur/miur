import os
import random
import sys
from queue import Queue
from threading import Event
from typing import Callable

# pylint:disable=unsupported-assignment-operation
os.environ["QT_ACCESSIBILITY"] = "0"
os.environ["NO_AT_BRIDGE"] = "1"
os.environ["QT_NO_DBUS"] = "1"

# pylint:disable=wrong-import-position
from PySide6 import QtCore, QtGui, QtWidgets
from PySide6.QtCore import Property, QRect, Qt
from PySide6.QtGui import QColor, QFont, QPainter
from PySide6.QtWidgets import QWidget

from .. import log

# TUT: https://www.pythonguis.com/
# CMP: https://www.reddit.com/r/Python/comments/1g6brra/pyqt_best_option_for_commercial_use/


class FileItem:
    """Represents a mock semantic backend file entry with variable dimensions."""

    def __init__(
        self,
        name: str,
        annotation: str = "",
        is_selected: bool = False,
        is_modified: bool = False,
    ) -> None:
        self.name = name
        self.annotation = annotation
        self.is_selected = is_selected
        self.is_modified = is_modified


class MillerColumnWidget(QWidget):
    """An IMGUI-style custom column widget that uses a single function to layout & draw."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.display_list: list[FileItem] = []
        self.cached_items: list[tuple[int, QRect]] = []
        self.cached_width = 150  # Discovered dynamically during the recorder pass
        self.top_visible_index = 0

        # Enforce pixel-perfect fixed-width font rules
        self.mono_font = QFont("Monospace", 10)
        self.mono_font.setStyleHint(QFont.StyleHint.TypeWriter)
        self.setFont(self.mono_font)

    def set_display_list(self, new_list: list[FileItem]) -> None:
        self.display_list = new_list
        self.top_visible_index = 0
        self.refresh_layout_recording()

    def refresh_layout_recording(self) -> None:
        """Runs the loop without a painter to compute and cache layout state."""
        self.render_column_pass(painter=None)

    def render_column_pass(self, painter: QPainter | None = None) -> None:
        """
        THE SINGLE SOURCE OF TRUTH.
        If painter is None, it behaves as a layout recorder and updates geometries.
        If painter is active, it draws pixels directly inside the recorded boundaries.
        """
        font_metrics = self.fontMetrics()
        current_y = 0
        max_y = self.height() if self.height() > 0 else 600
        max_w = 150  # Enforce a solid fallback baseline width
        idx = self.top_visible_index
        is_recording = painter is None

        if is_recording:
            self.cached_items.clear()

        # Render rows dynamically down the canvas screen until the visible space fills up
        while current_y < max_y and idx < len(self.display_list):
            item = self.display_list[idx]

            # --- METRICS CALCULATIONS (Executed Exactly Once per Loop) ---
            # Dynamic height modifier: modified items occupy two rows of text
            base_row_height = font_metrics.height() + 8
            item_h = base_row_height * 2 if item.is_modified else base_row_height

            # Dynamic width modifier based on font metadata sizing
            text_width = font_metrics.horizontalAdvance(item.name)
            item_w = text_width + 30  # Standard name width + padding

            if item.annotation:
                # Append decoration text width dynamically to the running coordinate line
                item_w += font_metrics.horizontalAdvance(item.annotation) + 20

            if item_w > max_w:
                max_w = item_w

            # Stabilize boundary structures
            # The layout phase uses the running max width,
            #   while painting locks into the fully computed column width
            row_width = max_w if is_recording else self.cached_width
            rect = QRect(0, current_y, row_width, item_h)

            if is_recording:
                self.cached_items.append((idx, rect))
            else:
                # Pixel Painter Branch
                self.draw_item_pixels(painter, rect, item, font_metrics)

            current_y += item_h
            idx += 1

        if is_recording:
            self.cached_width = max_w

    def draw_item_pixels(
        self,
        painter: QPainter,
        rect: QRect,
        item: FileItem,
        font_metrics: QtGui.QFontMetrics,
    ) -> None:
        """Handles low-level canvas painting directly within calculated layout constraints."""
        # 1. Paint selection state backgrounds
        if item.is_selected:
            painter.fillRect(rect, QColor("#2a52be"))  # Royal Blue Selection
            painter.setPen(QColor("#ffffff"))
        else:
            painter.setPen(QColor("#000000"))

        # 2. Paint primary directory/file text
        text_padding_y = 4
        text_rect = rect.adjusted(10, text_padding_y, -10, 0)
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignTop, item.name)

        # 3. Query end-coordinates dynamically to safely place decorations relative to names
        if item.annotation:
            text_end_x = rect.left() + 10 + font_metrics.horizontalAdvance(item.name)
            annot_rect = QRect(
                text_end_x + 15,
                rect.top() + text_padding_y,
                rect.width() - text_end_x,
                rect.height(),
            )

            if not item.is_selected:
                painter.setPen(QColor("#888888"))  # Gray out annotation metadata text
            painter.drawText(annot_rect, Qt.AlignmentFlag.AlignTop, item.annotation)

        # 4. Paint sub-row details for specialized elements (e.g. dynamic height blocks)
        if item.is_modified:
            painter.setPen(
                QColor("#cc0000") if not item.is_selected else QColor("#ffcccc")
            )
            sub_text_rect = rect.adjusted(
                25, text_padding_y + font_metrics.height(), -10, 0
            )
            painter.drawText(
                sub_text_rect,
                Qt.AlignmentFlag.AlignTop,
                " -> [ modified git tracker status ]",
            )

    def paintEvent(self, _event: QtGui.QPaintEvent) -> None:
        painter = QPainter(self)
        painter.setRenderHint(
            QPainter.RenderHint.Antialiasing, False
        )  # Pure pixel alignments
        # Paint pixels directly by passing the active canvas window handle
        self.render_column_pass(painter=painter)


class MillerCanvas(QWidget):
    """The master viewport controller tracking columns, sizes, and slide sequences."""

    anim: QtCore.QPropertyAnimation

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._slide_offset = 0

        # Instantiate exactly 4 reusable columns to drive a 3-panel sliding window view
        self.ordered_panels = [MillerColumnWidget(self) for _ in range(4)]

        # Seed placeholder mock data layouts
        for i, panel in enumerate(self.ordered_panels):
            panel.set_display_list(self.generate_mock_directory(f"Col_{i}_Dir"))

        self.update_global_layout()

    # @Property(int)
    def _get_slide_offset(self) -> int:
        return self._slide_offset

    # @slideOffset.setter
    def _set_slide_offset(self, value: int) -> None:
        self._slide_offset = value
        self.update_global_layout()

    # EXPL: used inof @Property decor to avoid pyright complaining about fn-names overlap
    slideOffset = Property(int, _get_slide_offset, _set_slide_offset)

    def generate_mock_directory(self, prefix: str) -> list[FileItem]:
        """Generates random lists to emulate complex item sizes, strings, and states."""
        items: list[FileItem] = []
        for i in range(50):
            has_annot = random.choice([True, False])
            is_mod = random.choice([True, False, False, False])  # 25% chance
            annot_txt = f"[Size: {random.randint(1, 99)}MB]" if has_annot else ""
            name = f"📄 {prefix}_file_item_long_hash_{random.randint(100, 999)}"
            items.append(
                FileItem(name, annot_txt, is_selected=(i == int(2)), is_modified=is_mod)
            )
        return items

    def update_global_layout(self) -> None:
        """Sequences coordinates adaptively based on dynamic width definitions."""
        # Force columns to compute their text boundaries during a layout pass
        for panel in self.ordered_panels:
            panel.refresh_layout_recording()

        # Extract the off-screen left panel's width to set the start coordinate anchor
        hidden_left_width = self.ordered_panels[0].cached_width
        current_x = -hidden_left_width + self._slide_offset

        # Chain column coordinates tightly alongside one another
        for panel in self.ordered_panels:
            panel.setGeometry(current_x, 0, panel.cached_width, self.height())
            panel.update()  # Request repaint safely
            current_x += panel.cached_width

    def resizeEvent(self, _event: QtGui.QResizeEvent) -> None:
        self.update_global_layout()

    def navigate_forward(self) -> None:
        """Animates a dynamic width shift cleanly across panels."""
        if (
            hasattr(self, "anim")
            and self.anim.state() == QtCore.QPropertyAnimation.State.Running
        ):
            return

        # Prepare upcoming metadata files on the hidden 4th track before initiating the slide
        self.ordered_panels[3].set_display_list(
            self.generate_mock_directory("Sublevel")
        )

        # Slide distance exactly matches the width of the panel moving out of view
        slide_distance = self.ordered_panels[1].cached_width

        self.anim = QtCore.QPropertyAnimation(self, b"slideOffset")
        self.anim.setDuration(250)
        self.anim.setStartValue(0)
        self.anim.setEndValue(-slide_distance)
        self.anim.setEasingCurve(QtCore.QEasingCurve.Type.OutQuad)
        self.anim.finished.connect(self.finalize_forward_navigation)
        self.anim.start()

    def finalize_forward_navigation(self) -> None:
        """Executes a reference array rotation to reset layout flags instantly."""
        # Shift reference slots: Drop hidden index 0 and push it to track 3
        discarded_left_panel = self.ordered_panels.pop(0)
        self.ordered_panels.append(discarded_left_panel)

        # Reset offsets to zero. Visual location stays exactly identical with zero pixel jumps.
        self._slide_offset = 0
        self.update_global_layout()


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, recv_q: Queue[dict[str, object]]) -> None:
        super().__init__()
        self.miur_recv_q = recv_q
        self.setWindowTitle("Terminal Miller Navigation - PySide6 IMGUI Loop")
        self.resize(900, 500)

        # Main Layout Setup
        central_widget = QWidget(self)
        # MAYBE: make it into TEMP status
        #   BUT: I already have statusline in DisplayList!
        #   BET? send logline back
        self.label = QtWidgets.QLabel("Waiting for live Python objects...", margin=20)
        self.label.setWordWrap(False)
        P = QtWidgets.QSizePolicy.Policy
        self.label.setSizePolicy(P.Preferred, P.Fixed)

        self.setCentralWidget(central_widget)
        layout = QtWidgets.QVBoxLayout(central_widget)
        layout.setContentsMargins(0, 0, 0, 0)

        # Initialize the custom Miller view canvas
        self.canvas = MillerCanvas(self)
        layout.addWidget(self.canvas)
        layout.addWidget(self.label)

        print("Press <Space> to simulate navigating into dirs")

    def keyPressEvent(self, event: QtGui.QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Space:
            self.canvas.navigate_forward()
        else:
            super().keyPressEvent(event)
        evkey = {
            "ev": "keyPressEvent",
            "code": event.key(),
            "char": event.text(),
            "shift_on": bool(event.modifiers() & Qt.KeyboardModifier.ShiftModifier),
        }
        self.miur_recv_q.put(evkey)


class QtBridge(QtCore.QObject):
    """Encapsulates cross-thread communication and GUI state mutation."""

    # Assumption: Signal definition requires the builtin `object` type to satisfy
    # the C++ meta-object compiler, not the `typing.Any` type hint.
    # Qt natively handles the cross-thread context switch.
    data_received = QtCore.Signal(object)

    def __init__(self, app: QtWidgets.QApplication, wnd: MainWindow) -> None:
        super().__init__()

        # Limitation: We must bind UI components to the instance since we
        # can no longer rely on the outer scope closure.
        self._app = app
        self._wnd = wnd

        # Connect the signal to the internal slot. Qt guarantees that if this
        # object lives on the GUI thread, the slot executes safely on the GUI thread.
        self.data_received.connect(self.process_incoming)

    @QtCore.Slot(object)
    def process_incoming(self, item: object) -> None:
        """Executes strictly inside the Qt main thread loop natively."""

        log.warning(item)

        # Corner Case: Raw strings for control flow can cause collisions if
        # your data stream normally contains strings. A typed sentinel
        # (e.g., `class ShutdownEvent: pass`) is structurally safer.
        # FIXME: use "MiurEvent" type here
        if item == "SHUTDOWN":
            self._app.quit()
            log.info("quit")
            self._wnd.miur_recv_q.put({"ev": item})
            return

        # from .models import DisplayElement
        # if isinstance(item, DisplayElement):
        #     text = f"Rendering a {item.color} {item.shape_type} at ({item.x}, {item.y})"

        text = str(item)
        # 2. Set Elide mode so long text scales with an ellipsis instead of breaking lines
        lb = self._wnd.label
        right = Qt.TextElideMode.ElideRight
        lb.setText(lb.fontMetrics().elidedText(text, right, lb.width()))
        # Perform canvas updates...


def main(
    send_bridge: list[Callable[[object], None]],
    miur_recv_q: Queue[dict[str, object]],
    ready_event: Event,
) -> int:
    try:
        app = QtWidgets.QApplication([])  # sys.argv
        app.setQuitOnLastWindowClosed(
            False
        )  # CRITICAL: Keeps Qt alive when windows close

        window = MainWindow(miur_recv_q)
        window.show()

        bridge = QtBridge(app=app, wnd=window)

        # Export the bound emit method to the main thread
        send_bridge.append(bridge.data_received.emit)
    except Exception as exc:
        log.exception(exc)
        raise
    finally:
        # TEMP:HACK: unblock main app to crash
        ready_event.set()

    # FIXME: if it crashes *here* we should still return somehow...
    try:
        # Blocks until app.quit() is called
        rc = app.exec()

        log.info("qt_exit")

        # CRITICAL: Sever the link from the main thread immediately.
        # This guarantees the reference count to `bridge` drops to 0 inside the
        # worker thread before it attempts C++ garbage collection.
        send_bridge.clear()

        # --- CRITICAL CLEANUP BLOCK ---
        # Limitation: Python's GC is non-deterministic. If we let these variables
        # naturally go out of scope, the C++ objects might outlive the thread and
        # be destroyed by the main thread during process exit.

        # 1. Signal/Slot connections can create invisible reference cycles in PySide6.
        # Disconnecting them ensures del drops the reference count to exactly 0.
        bridge.data_received.disconnect(bridge.process_incoming)

        # 2. Schedule native C++ deletion
        window.deleteLater()
        bridge.deleteLater()

        # 3. Flush the event queue to process the deleteLater() calls natively
        app.processEvents()

        log.info("qt_destroy")

        # 4. Explicitly drop Python references to trigger immediate C++ destructors
        # while we are still safely inside the GUI thread.

        # Corner Case: Python's GC is lazy, and QCoreApplication is a C++ singleton.
        # shiboken6.delete() instantly invalidates the Python wrapper and forces the
        # C++ destructor to run on THIS thread, preempting PySide6's atexit hook.
        import shiboken6  # PySide6's underlying C++ wrapper engine

        shiboken6.delete(bridge)
        shiboken6.delete(window)
        shiboken6.delete(app)
        del bridge
        del window
        del app
        log.info("qt_del")

        # --- PHASE 2: Metaprogramming & Registry Eradication ---
        # 1. Unbind PySide's global atexit hooks to prevent the shutdown crash
        # try:
        #     import atexit
        #     # PySide6 injects internal private teardown functions into atexit.
        #     # We clear the atexit registry to stop C++ singletons trying to access a dead thread.
        #     atexit._clear()
        # except Exception:
        #     pass

        # 2. Force-purge all PySide6 / Shiboken module caching out of sys.modules.
        # This destroys the class definitions holding onto those uncollectable Property objects.
        pyside_modules = [
            mod
            for mod in list(sys.modules.keys())
            if "PySide6" in mod or "shiboken" in mod
        ]
        for mod in pyside_modules:
            del sys.modules[mod]

        # --- PHASE 3: The Ultimate Sweep ---
        # Clear local frame references and run an aggressive, multi-generational GC collect.
        import gc

        # Corner Case: Force a GC run inside the thread to clean up any
        # lingering Python wrappers before the thread dies.
        gc.collect()
        # gc.select_subgraph if hasattr(
        #     gc, "select_subgraph"
        # ) else None  # For specialized runtimes
        del gc.garbage[:]  # Force break remaining uncollectable cycles if any exist

        return rc

    except Exception as exc:
        log.exception(exc)
        print(log.archive_recent(dump=True), file=sys.stderr)
        raise


if __name__ == "__main__":
    main([], Queue(), Event())
