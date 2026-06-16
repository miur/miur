## RND:(umbrella term): .aid = {attributes|algorithm|animation|appearance|applet|artificial}_id
#   WHY: short name .sid (static "style_id") is too similar to system_id
#   NICE: "aid" == "the aux means to accomplish smth" (e.g. rendering)
#   ALT:(name): style/effect/theme/look/presentation
#   ARCH: good fit for "presentation/appearance"
#     * signals a visual intent (inof "how to draw")
#     * allows composition of effects
#     * can apply transformation to content (like vertex shader)
#     * can describe transitioning algo (like animation)
#     * decorative animations and ops indicators
#       - blinking cursor OR plasma gradient bkgr
#       - bright dot on cursor boarder coursing around item text
#       - highlight one cell left-right-left-right as "indeterminate waiting"
#         ~~ e.g. for currenly downloaded partial files
#       - flash bg to indicate "copy/paste" operations
#         NEED: history to cvt to selection any "last flashed pasted selection"
#           or "touched/created files in folder"
#         ARCH: should be done fully on client side to minimize server/kernel latency for timestamps
#           ~~ ALSO: each UI may do stuff totally differently, so it may have no common parts on server
#             ALT: calc ani frame in kernel to get displ with static styles only on client
#       - WARN: diff-update won't work as both token and style are the same
#          >> NEED: store animations in some list to update those by timer
#     * allows gradients/beautifications
#       - e.g. enlarging font-size in Qt/GL or making a distorted zoom-bubble around cursor
#     * parametrized styles (e.g. color is based on some argvalue/function)
#       ? similar to "nested style" (TRY: generalize one to another)
#     * associates dif attrs to be used by diff UIs (e.g. RGB24 vs I16/I256 colors)
#       - ALSO: provides fg/bg styles for TUI; border for GUI; shape for opengl
#     * same id can mean dif things for obj-type (Text/Rect/Image/etc.)
#   IDEA: use each name fg/bg as progressbar for e.g. !mpd OR as gauge for e.g. relative file/dirsize like !ncdu

from enum import IntEnum, auto


# BAD:(IntEnum + jurigged): old inst/vars will break or cmp as False against the updated version
class Aid(IntEnum):
    unknown = 0  # ALT: py:$ def _generate_next_value_(name, start, count, last_values): return count
    default = auto()
    item = auto()
    itempunct = auto()
    lineidx = auto()
    itemidx = auto()
    footer = auto()
