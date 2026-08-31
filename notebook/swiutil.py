from dataclasses import dataclass
from pathlib import Path

import matplotlib.animation as animation
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from matplotlib.collections import PatchCollection
import numpy as np
import flopy


def get_layered_top(top, botm):
    nlay, nrow, ncol = botm.shape
    layered_top = np.zeros((nlay, nrow, ncol))
    layered_top[0, :, :] = top
    for k in range(1, nlay):
        layered_top[k, :, :] = botm[k - 1, :, :]
    return layered_top


def make_cross_section_plot(ax, sim, top=None, bot=None, **kwargs):

    gwf = sim.gwf[0]
    modelgrid = gwf.modelgrid
    nlay = modelgrid.nlay
    delr = modelgrid.delr
    ncol = modelgrid.ncol
    if bot is None:
        bot = modelgrid.botm
    if top is None:
        top = get_layered_top(modelgrid.top, modelgrid.botm)
    pc = []
    i = 0

    for k in range(nlay):
        for j in range(ncol):
            x = modelgrid.xcellcenters[i, j] - 0.5 * delr[j]
            w = delr[j]
            y = bot[k, i, j]
            yt = top[k, i, j]
            rect = Rectangle((x, y), w, yt - y)
            pc.append(rect)

    pc = PatchCollection(pc)
    pc.set(**kwargs)
    ax.add_collection(pc)
    return ax


@dataclass
class SwiAnimator:
    sim: object
    freshwater_color: str = "lightblue"
    saltwater_color: str = "darkblue"
    edgecolor: str = "k"
    figsize: tuple = (8, 6)
    get_title: callable = None
    layer_lines: bool = True
    layer_line_color: str = "0.4"

    def __post_init__(self):
        self.gwf = self.sim.gwf[0]
        self.modelgrid = self.gwf.modelgrid
        self.x = self.modelgrid.xcellcenters.flatten()
        self.botm = self.modelgrid.botm
        self.top_layered = get_layered_top(self.modelgrid.top, self.botm)
        self.ylim = self.modelgrid.botm.min(), self.modelgrid.top.max()

        self.zeta_all = self.gwf.swi.output.zeta().get_alldata()
        self.head_all = self.gwf.output.head().get_alldata()
        self.times = self.gwf.output.head().times

        self.fig, self.ax = plt.subplots(figsize=self.figsize)
        self.ax.set_xlim(self.x.min(), self.x.max())
        self.ax.set_ylim(*self.ylim)

        # static layer boundaries (model top and each layer bottom); drawn as
        # lines, so they survive _clear_collections between frames
        if self.layer_lines:
            top = np.asarray(self.modelgrid.top).reshape(self.botm.shape[1:])
            self.ax.plot(
                self.x, top.flatten(), color=self.layer_line_color, lw=0.8
            )
            for k in range(self.botm.shape[0]):
                self.ax.plot(
                    self.x,
                    self.botm[k].flatten(),
                    color=self.layer_line_color,
                    lw=0.8,
                )

        self.make_plot_func = make_cross_section_plot

    def _clear_collections(self):
        # Remove prior frame patches only
        for coll in list(self.ax.collections):
            coll.remove()

    def _get_title(self, i):
        time = self.times[i]
        return f"Time = {time} days"

    def _draw_frame(self, i):
        self._clear_collections()

        head = self.head_all[i]
        zeta = self.zeta_all[i]

        # Freshwater
        fw_top = np.minimum(self.top_layered, head)
        fw_bot = np.maximum(zeta, self.botm)
        self.ax = self.make_plot_func(
            self.ax, self.sim, top=fw_top, bot=fw_bot,
            facecolor=self.freshwater_color, edgecolor=self.edgecolor
        )

        # Saltwater
        sw_top = np.minimum(self.top_layered, zeta)
        sw_bot = self.botm
        self.ax = self.make_plot_func(
            self.ax, self.sim, top=sw_top, bot=sw_bot,
            facecolor=self.saltwater_color, edgecolor=self.edgecolor
        )

        if self.get_title is not None:
            t = self.get_title(self.times[i])
        else:
            t = self._get_title(i)
        self.ax.set_title(t)
        return self.ax.collections

    def plot_frame(self, i):
        """Draw a single saved time step as a static figure (negative
        indices count from the end, e.g. -1 for the final step)."""
        if i < 0:
            i = self.zeta_all.shape[0] + i
        self._draw_frame(i)
        return self.fig

    def create(self, frames=None):
        """Create the animation. By default every saved time step becomes a
        frame; pass an iterable of frame indices to animate a subset (e.g.
        range(i0, nframes) to skip a spin-up period)."""
        if frames is None:
            frames = self.zeta_all.shape[0]
        ani = animation.FuncAnimation(
            self.fig, self._draw_frame, frames=frames, blit=False
        )
        plt.close(self.fig)
        return ani