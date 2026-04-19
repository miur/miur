# %VIZ: $ /d/miur/.venv/bin/python -m pyqtgraph.examples
# ALT:TRY: vispy
import numpy as np
import pyqtgraph as pg
import pyqtgraph.opengl as gl


def main() -> None:
    app = pg.mkQApp("WirePlot + Font")
    w = gl.GLViewWidget()
    w.show()
    w.setWindowTitle("GLLinePlotItem + GLTextItem")
    w.setCameraPosition(distance=40)

    gx = gl.GLGridItem()
    gx.rotate(90, 0, 1, 0)
    gx.translate(-10, 0, 0)
    # gx.setSize(10, 10)
    # gx.setSpacing(1, 1)
    w.addItem(gx)
    gy = gl.GLGridItem()
    gy.rotate(90, 1, 0, 0)
    gy.translate(0, -10, 0)
    w.addItem(gy)
    gz = gl.GLGridItem()
    gz.translate(0, 0, -10)
    w.addItem(gz)

    axisitem = gl.GLAxisItem()
    w.addItem(axisitem)
    txtitem1 = gl.GLTextItem(pos=(0.0, 0.0, 0.0), text="text1")
    w.addItem(txtitem1)
    txtitem2 = gl.GLTextItem()
    txtitem2.setData(pos=(1.0, -1.0, 2.0), color=(127, 255, 127, 255), text="text2")
    w.addItem(txtitem2)

    n = 51
    y = np.linspace(-10, 10, n)
    x = np.linspace(-10, 10, 100)
    for i in range(n):
        yi = y[i]
        d = np.hypot(x, yi)
        z = 10 * np.cos(d) / (d + 1)
        pts = np.column_stack([x, np.full_like(x, yi), z])
        plt = gl.GLLinePlotItem(
            pos=pts,
            color=pg.mkColor((i, n * 1.3)),
            width=(i + 1) / 10.0,
            antialias=True,
        )
        w.addItem(plt)

    pg.exec()


if __name__ == "__main__":
    main()
