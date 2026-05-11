from collections import defaultdict
from typing import NamedTuple


class Style(NamedTuple):
    # No __slots__ = () needed for immediate child of NamedTuple (but needed for next inheritance)
    fg: str  # e.g., "red" or "#FF0000"
    bg: str | None = None
    bold: bool = False
    italic: bool = False
    # font_family: str = "monospace"


## USAGE
# registry = StyleRegistry()
# error_style_id = registry.get_or_create(Style(fg="red", bold=True))
# token = TextSpan(0, 0, error_style_id, "Fatal Error!")
class StyleRegistry:
    def __init__(self):
        self._styles: dict[int, Style] = {}
        self._hash_to_id: dict[int, int] = {}
        self._next_id = 0

    def get_or_create(self, style: Style) -> int:
        h = hash(style)
        if h in self._hash_to_id:
            return self._hash_to_id[h]

        style_id = self._next_id
        self._styles[style_id] = style
        self._hash_to_id[h] = style_id
        self._next_id += 1
        return style_id

    def fetch(self, style_id: int) -> Style:
        return self._styles[style_id]


class TextSpan(NamedTuple):
    x: int  # FUT: use virtual abs coords e.g. 1cell=100pt for shared Qt/OpenGL draw
    y: int
    style_id: int
    content: str
    # ALT:(z_index): uzi (unique Z index -- assign increasing numbers for each token)
    #   BET: use Z-index of whole layer bucket (NOT per-element) for stable element identity
    z_index: int = 1  # Text stays on top (so array order won't matter for set(curr))
    # parent_uid: Optional[str] = None  # < Grouping without Nesting (click propagation)
    # uid: str         # The element that "owns" this text


class Box(NamedTuple):
    x: int
    y: int
    w: int
    h: int
    style_id: int
    z_index: int = 0  # Backgrounds get lower numbers
    # uid: Optional[str] = None


## FUT: raw buffer for unique "hystograms"
# class ImageRawBuffer(x, y, width, height, data_ptr)
## FUT: Procedural Style ID or a Vertex-based Token
## WHY: "don't look at a static color—apply this function/shader instead."
# class EffectSpan(NamedTuple):
#     x: int
#     y: int
#     effect_id: int  # e.g., EFFECT_RAINBOW
#     phase: float    # Controls the animation (e.g., time % 1.0)
#     content: str


type DisplayToken = CellSpan  # Union[CellSpan, CursorMove]
# RENAME? == Layered Display List of Atomic Spans
type DisplayList = list[DisplayToken]


def intersects(a, b) -> bool:
    # Standard overlap check: (LeftA < RightB) and (RightA > LeftB) ...
    return a.x < b.x + b.w and a.x + a.w > b.x and a.y < b.y + b.h and a.y + a.h > b.y


class SpatialIndex:
    def __init__(self, cell_size=500):
        self.cell_size = cell_size
        self.grid = defaultdict(list)

    def add(self, token):
        # Determine which grid cells this token touches
        x1, y1 = token.x // self.cell_size, token.y // self.cell_size
        x2, y2 = (
            (token.x + token.w) // self.cell_size,
            (token.y + token.h) // self.cell_size,
        )

        for i in range(x1, x2 + 1):
            for j in range(y1, y2 + 1):
                self.grid[(i, j)].append(token)

    ## USAGE: Repairing a hole after moving popup
    # unordered_repairs = spatial_index.query(void_rect)
    # # Restore order by checking against the current frame's master list (O(N))
    # ordered_repairs = [t for t in current_frame_list if t in unordered_repairs]
    def query(self, target: Rect) -> set[Token]:
        """Returns all tokens in the new frame that intersect with the target area."""
        # Use the same cell logic to find relevant buckets
        # Then perform a fine-grained 'intersects' check on those tokens
        results = set()

        # 1. Determine the range of grid cells that the target area covers
        x1, y1 = target.x // self.cell_size, target.y // self.cell_size
        x2, y2 = (
            (target.x + target.w - 1) // self.cell_size,
            (target.y + target.h - 1) // self.cell_size,
        )

        # 2. Iterate through touched cells
        for i in range(x1, x2 + 1):
            for j in range(y1, y2 + 1):
                cell_items = self.grid.get((i, j))
                if cell_items:
                    # 3. Perform fine-grained check to eliminate false positives
                    # (tokens in the same cell but not touching the target)
                    for token in cell_items:
                        if intersects(token, target):
                            results.add(token)
        return results


## TBD: .compose() bw .bake and .render: Instead of sending raw boxes and raw text
#  to the driver, you "flatten" them into a single-layer Display List
#  where overlapping areas are pre-calculated.
#   If Box(Z=0) is blue and OverlayText(Z=1) is over it, the compositor emits
#     a TextSpan with bg_style=blue.
#   If the Box changes to red, the compositor realizes the intersection changed
#     and emits a new TextSpan with bg_style=red.
# FIXME: If your text spans are long (many cells), you might want the TextSpan to sample the bg_map at multiple
# points if it crosses into a different background color. However, for most UIs, sampling at the start
# coordinate is sufficient.
def compose(layered_elements, cell_size=100):
    """
    layered_elements: list[list[RawElement]] - Raw widgets/DOM nodes by layer
    Returns: dict[int, list[Token]] - Baked Display List with resolved bg_style_ids
    """
    display_list = {}
    # Sparse map: (grid_x, grid_y) -> style_id
    # Tracks the 'topmost' background style for every virtual cell
    bg_map = {}

    for z, layer in enumerate(layered_elements):
        baked_layer = []

        for element in layer:
            # 1. Spatial Hashing: Find what is 'under' this element
            # We sample the center or top-left of the element's start
            grid_coord = (element.x // cell_size, element.y // cell_size)
            inherited_bg = bg_map.get(grid_coord, DEFAULT_BG_ID)

            # 2. Bake the Token
            if element.is_text:
                token = TextSpan(
                    x=element.x,
                    y=element.y,
                    style_id=element.sid,
                    bg_style_id=inherited_bg,  # THE BLEED: Use color from lower layer
                    content=element.text,
                )
            else:
                # It's a Box/Rect
                token = Rect(element.x, element.y, element.w, element.h, element.sid)

                # 3. Update the Map: This box now provides the background for higher layers
                # Fill all grid cells this box covers
                x1, y1 = element.x // cell_size, element.y // cell_size
                x2, y2 = (
                    (element.x + element.w) // cell_size,
                    (element.y + element.h) // cell_size,
                )

                for i in range(x1, x2 + 1):
                    for j in range(y1, y2 + 1):
                        bg_map[(i, j)] = element.sid

            baked_layer.append(token)

        display_list[z] = baked_layer

    return display_list


def render_layer(current: DisplayList) -> None:
    mydrv_print = print
    # PERF: redraw only dirty_tokens
    added = set(current) - set(previous)
    removed = set(previous) - set(current)
    final_render_list = [t for t in current if t in added or overlaps_any(t, removed)]
    for token in final_render_list:
        match token:
            # TEMP: use wnd.abs(x,y) -> then change to area.rel(x,y) and patch offset in batch
            # OR: case TextSpan(x=x, y=y, style_id=sid, text=text):
            case CellSpan(x, y, sid, text):
                # style = registry.fetch(sid)
                # driver.set_pen(style.fg, style.bold)
                mydrv_print(x, y, sid, text)
            # case CursorMove(x, y):
            #     my_driver.move(x, y)


# If void_rects contains many items, the any() check becomes \(O(N \cdot M)\). To keep it fast, merge
# overlapping void_rects into a few larger bounding boxes before starting the loop, or query the spatial_index
# once for the union of all voids.
def render(current_frame, previous_frame, spatial_index, driver):
    """
    current_frame: dict[int, list[Token]] - {0: [base_tokens], 1: [popup_tokens]}
    previous_frame: dict[int, list[Token]]
    spatial_index: SpatialIndex (populated with current_frame tokens)
    """

    # 1. Identify "The Void" - Areas where tokens were removed or changed
    # We must redraw anything currently in these regions to prevent "ghosting"
    void_rects = []
    for z in sorted(current_frame.keys() | previous_frame.keys()):
        curr_set = set(current_frame.get(z, []))
        prev_set = set(previous_frame.get(z, []))

        # Any token in 'prev' but not 'curr' has left a hole
        removed = prev_set - curr_set
        for t in removed:
            void_rects.append(t)  # NamedTuples act as Rects (x, y, w, h)

    # 2. Layer-by-Layer Render Pass
    for z in sorted(current_frame.keys()):
        layer_tokens = current_frame[z]
        prev_layer_set = set(previous_frame.get(z, []))

        # 3. Optimized Diff: [t for t in current if t is NEW or in a VOID]
        # This preserves the baked generator order while providing O(1) lookup
        to_draw = [
            t
            for t in layer_tokens
            if t not in prev_layer_set or any(intersects(t, v) for v in void_rects)
        ]

        # 4. Stream to Driver
        # Since 'to_draw' is a slice of 'layer_tokens', order is stable
        for token in to_draw:
            match token:
                case TextSpan(x, y, sid, bg_sid, text):
                    driver.draw_text(x, y, sid, bg_sid, text)
                case Rect(x, y, w, h, sid):
                    driver.draw_rect(x, y, w, h, sid)
                case CursorMove(x, y):
                    driver.move_cursor(x, y)


# To handle bolding (or any "style-only" change) that affects existing text on lower layers, your Compositor needs to switch from a "Bottom-Up" approach to a Two-Pass Composition.
# Instead of baking tokens immediately when you see them, the Compositor should:Pass 1 (Collect): Walk through
# all layers and gather all StyleModifiers into a Spatial Index.Pass 2 (Bake): Walk through all TextSpans and
# "query" the modifiers in their area to produce a final style_id.
class StyleModifier(NamedTuple):
    x: int
    y: int
    w: int
    h: int
    add_bold: bool = False
    fg_override: Optional[int] = None
    # No content - it just modifies what's underneath


# To keep style_registry.get_modified fast, use a Cache: (base_sid, frozenset(modifiers)) -> new_sid. This
# prevents you from re-hashing style objects every single frame for the same bolded text.
def compose(layered_elements):
    # Pass 1: Gather modifiers (O(M))
    modifiers = SpatialIndex()
    for layer in layered_elements:
        for el in layer:
            if isinstance(el, StyleModifier):
                modifiers.add(el)

    # Pass 2: Bake Spans with Modifier lookup (O(N))
    display_list = {}
    for z, layer in enumerate(layered_elements):
        baked_layer = []
        for el in layer:
            if isinstance(el, RawText):
                # 1. Start with base style
                final_sid = el.sid

                # 2. Check for modifiers 'above' or 'at' this layer
                overlapping_mods = modifiers.query(el.rect)
                if overlapping_mods:
                    # Resolve base style + modifiers into a new unique StyleID
                    # e.g., Style(red) + Modifier(bold) = Style(red_bold)
                    final_sid = style_registry.get_modified(el.sid, overlapping_mods)

                # 3. Bake with the modified style
                baked_layer.append(TextSpan(el.x, el.y, final_sid, ...))
        display_list[z] = baked_layer
    return display_list


from typing import Dict, FrozenSet, NamedTuple


class Style(NamedTuple):
    fg: str
    bg: str
    bold: bool = False
    italic: bool = False


# To prevent the Style Registry from exploding into thousands of unique entries when merging modifiers (like bold or color shifts), you should use an Interning Cache with a Compound Key.
class StyleRegistry:
    def __init__(self):
        self._styles: Dict[int, Style] = {}
        self._next_id = 0

        # Primary lookup: StyleObject -> StyleID
        self._obj_to_id: Dict[Style, int] = {}

        # Merge Cache: (BaseStyleID, ModifierSet) -> ResultStyleID
        # This prevents re-calculating the 'merged' style object
        self._merge_cache: Dict[tuple[int, FrozenSet], int] = {}

    def get_modified(self, base_id: int, modifiers: FrozenSet) -> int:
        """Merges a base style with modifiers and returns a stable ID."""
        cache_key = (base_id, modifiers)
        if cache_key in self._merge_cache:
            return self._merge_cache[cache_key]

        # 1. Fetch base style data
        base_style = self._styles[base_id]

        # 2. Apply modifications to create a new Style object
        # Modifiers can be simple flags or attribute-value pairs
        new_style_dict = base_style._asdict()
        for mod in modifiers:
            # Example: Modifiers could be ('bold', True)
            attr, value = mod
            new_style_dict[attr] = value

        new_style = Style(**new_style_dict)

        # 3. Intern the result so we don't have duplicates
        res_id = self.get_or_create(new_style)
        self._merge_cache[cache_key] = res_id
        return res_id
