from ..util.logger import log
from .vlst_base import SatelliteViewport_DataProtocol


# pylint:disable=too-many-instance-attributes
class SatelliteViewport_StepbyMixin:

    # TBD: def scroll_by(self, advance: int) -> None:

    def jump_to(self: SatelliteViewport_DataProtocol, idx: int) -> None:
        if not self._lst or idx not in (-1, 0):
            raise NotImplementedError("DECI:WiP")

        if idx == 0:
            pos = 0
        elif idx in (-1, len(self._lst) - 1):
            idx = len(self._lst) + idx if idx < 0 else idx
            pos = self._viewport_height_lines - self._itemheight(self._lst[idx])
        else:
            # FAIL: it steps by "steps" inof "items"
            # self.step_by((len(self._lst) + idx if idx < 0 else idx) - self._cursor_item_lstindex)
            raise NotImplementedError(idx)

        self._viewport_followeditem_lstindex = self._cursor_item_lstindex = idx
        self._viewport_followeditem_linesfromtop = pos

    # NOTE: elastic scroll, where "step" is anything between 1 line/word or whole multiline item,
    #   depending on what less disrupts the perception flow
    # TODO:OPT: always step by whole item ~~ should be the same as "step_by(+/-inf)"
    # pylint:disable=too-many-statements,too-many-branches,too-many-locals
    def step_by(self: SatelliteViewport_DataProtocol, steps: int) -> None:
        if steps not in (-1, 1):
            raise NotImplementedError("DECI:WiP")

        if not self._lst:
            log.trace("EMPTY")
            return

        def _fih(i: int) -> int:
            return self._itemheight(self._lst[i])

        idx = self._cursor_item_lstindex
        last = len(self._lst) - 1
        if idx < 0 or idx > last:
            raise IndexError(idx)
        ih = _fih(idx)

        # rng = range(min(idx, newidx), max(idx, newidx))
        # hlines = sum(self._itemheight(self._lst[i]) for i in rng)
        # RENAME: delta/advance/shift
        # offset = -hlines if steps < 0 else hlines
        # TEMP:FAIL: can't scroll, list is limited to the size of viewport
        # TODO: offset=0 if linesfromtop < margin or linesfromtop > vh - margin
        # self._viewport_followeditem_linesfromtop += offset

        vh = self._viewport_height_lines
        bot = vh - 1
        margin = self._viewport_margin_lines
        if vh < 2 * margin + 1:
            raise NotImplementedError(
                "TEMP:(restricted): viewport with enough space inside"
            )
        # NOTE:(pos<0|ih>vh is OK): multiline items can be larger than viewport
        pos = self._viewport_followeditem_linesfromtop

        knock = 0  # <EXPL: amount of unused "steps" (for either switching or scrolling)
        # ALT:IDEA:(step_incr=steps): only allow for "step_by(arg)" to move by one item/index,
        #   and use "arg" to pick speed of scrolling multiline items instead
        step_incr = 2  # <TEMP|RENAME? single_step/step_advance
        advance = int(step_incr * steps)  # OR: math.round(abs())

        # IDEA: visualize all these "margin" and sumheight/etc. to get a visual feedback
        #   from all boundary conditions -- and prevent errors like "off by 1"
        #   ~~ only visualize upp/bot "margin" based on last "steps" direction (i.e. dynamically)

        # IDEA: visualize scroll-animation when scrolling long items at once
        #   << orse they "jump into the face" when you are scrolling around margin

        # RQ: last item can be above bot only if list is shorter than vp
        #   ALT:CHECK:(gravitate): "gradually align of last item to bot" -- will it be *intuitive* ?
        if (
            idx == last
            and pos + ih < bot
            and (last >= bot or sum(_fih(i) for i in range(0, last + 1)) >= bot)
        ):
            raise NotImplementedError(
                "RQ: last item can be above bot only if list is shorter than vp"
            )

        # ARCH:FUT: it's a FSM(sign/idx/vp/size) -- should we make it explicit?
        # pylint:disable=chained-comparison,no-else-raise
        if pos < 0 or pos > bot:
            raise NotImplementedError("TEMP:(restricted): cursor should be visible")
            # raise NotImplementedError("past last multiline item")
            # if steps > 0 and idx < last:
            #     if pos > bot:
            #         raise NotImplementedError("TEMP: restricted; sync lost: vp had drifted above cursor")
            #     elif pos < 0:
            #         if pos <= -ih:
            #             raise NotImplementedError("TEMP: restricted; sync lost: vp had drifted below cursor")
            #         else:
            #             # THINK: actually *any* current item can be larger than vp
            #             raise NotImplementedError("TEMP: restricted; only *last* large multine item can start above vp")
            # elif idx == last:
            #     if pos <= -ih:
            #         # ALT: allow gradual alignment, like in {hidden_part < 0}
            #         raise NotImplementedError(
            #             "TEMP: restricted; last item is far above vp, nothing on the screen"
            #         )
            #     elif pos > bot:
            #         # ALT: scroll drifted vp until item becomes visible -> then show item hidden_part as usual
            #         # OR: imm jump vp to the current cursor position *and* move cursor by one step as usual
            #         # OR: jump cursor to current drifted vp top/bot focuseditem based on if <j/k> was pressed
            #         raise NotImplementedError(
            #             "TEMP: restricted; last item first line should be visible"
            #         )
        elif pos == 0 and steps < 0:
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
        elif pos == bot and steps > 0:
            # NOTE: preserve pos until the last item in list
            #   OR? auto-jump (or gravitate) to either margin or bot, based on how much lst is left
            idx += steps
            if idx > last:
                knock = idx - last
                idx = last
        elif pos <= margin and steps < 0:
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
            if pos > idx:  # <PERF:HACK
                # NOTE: if some item ih=0, then enclosing perf hack will leave gaps till first item
                assert all(_fih(i) >= 1 for i in range(0, idx))
                lines_top = sum(_fih(i) for i in range(0, idx))
                if lines_top < margin:
                    pos = lines_top
        elif pos >= bot - margin and steps > 0:
            ## FAIL: there is no way to detect when to switch to next item,
            #   as "virtual offset inside item" is derived from dynamic "offset from top"
            # MAYBE: we still need a "vp-virtual cursor" to point to "subparts" of the items
            #   ARCH: virt.cursor coincides with user's attention focus and limited by vp boundaries
            #     >> so "lst-cursor" is simply one of possible projections of virt.cursor onto lst
            #       i.e. multiple virt positions match single lst item
            #       HACK:(vice-versa): in zoom-out preview of the list one virt.cursor
            #         may match multiple lst items, which you can expand or zoom-in
            #   NICE? we can make {pos>=0} always positive, and control top item offset by subcursor
            #   IDEA: use 2D(y,x) subcursor to impl a wf-like "row of buttons" under each item
            #     (or to navigate individual words of multiline item/text)

            ## FIXME: {advance<ih} -> {virt.cursor-cursor+advance > ih}
            # if advance < ih:
            #     raise NotImplementedError(
            #         "TBD: scroll large item by steps, keeping virt.pos the same\n" + str(locals())
            #     )

            ## NOTE: jump to next item {if current one is small} and discard residue,
            # [_] OR:TODO: scroll large item by "steps"-proportional advancement
            # [_] ALSO:WF:TRY?(irritating?): transfer advancement residue onto next item scroll
            #   ~~ can be either more intuitive OR more disrupting
            idx += steps
            if idx > last:
                remainder = idx - last
                # BAD:(1): no gradual gravitation: still moves by jumping over whole empty_space
                advance = 1  # int(remainder * step_incr)
                idx = last

            if idx == last:
                ## NOTE: scroll partially shown last item
                # BAD! should also apply to above branch {pos==bot}
                #   ~~ BUT: may occur only if "RND:(invariant): cursor should be on margin" was broken
                # RENAME? visible_{part,below,range,room}/preview_{span,window}
                visible_room = vh - pos - 1
                hidden_part = ih - visible_room
                if hidden_part > 0:
                    # NOTE:(zoom-out): move vp away to make room for the last item to be visible on screen
                    if advance > hidden_part:
                        knock, residue = divmod(advance - hidden_part, step_incr)
                        if residue > 0:
                            knock += 1
                        pos -= hidden_part
                    else:
                        # OK: "pos" may become large negative to fit bot part of large multiline item into vp
                        pos -= advance
                elif hidden_part == 0:
                    # NOTE: detect the attempt to cross the border (bot of last multiline item)
                    # TODO:(animation): visual "knock()" for 100ms when attempting to scroll past top/bot item
                    knock, residue = divmod(advance - hidden_part, step_incr)
                    if residue > 0:
                        knock += 1
                elif hidden_part < 0:
                    # NOTE:(gravitate): gradually align bot of last item with bot of vp
                    #   ~~ may happen if vp was centered around last item first
                    # ALT:OPT: allow scrolling till only last line of last item visible
                    #   i.e. {pos -= advance} until {ih + pos == 1}
                    empty_space = -hidden_part
                    if advance > empty_space:
                        knock, residue = divmod(advance - empty_space, step_incr)
                        if residue > 0:
                            knock += 1
                        pos += min(empty_space, advance)
                    else:
                        pos += min(empty_space, advance)
                        assert pos + ih <= bot
                else:
                    raise ValueError("unexpected")

            # DEBUG: log.trace("".join(f"\n\t{k}={v}" for k,v in locals().items()))

            if bot - pos >= last - idx:  # <PERF:HACK
                assert all(_fih(i) >= 1 for i in range(idx, last + 1))
                ## NOTE: gravitate to bot
                # ALT:NOT? calc from bot-up until found how much items fit under margin
                #   ~~ for i in range(last, max(0,last-margin)); do if ... break; bot_edge_idx = i;
                lines_bot = sum(_fih(i) for i in range(idx, last + 1)) - 1
                if lines_bot <= margin:
                    # NOTE: immediately show last {small} items fully
                    pos = bot - lines_bot
                    assert 0 <= pos <= bot
                elif pos != bot - margin:
                    # ALT:MAYBE? gravitate cursor back to margin
                    raise RuntimeWarning("RND:(invariant): cursor should be on margin")
            elif pos != bot - margin:
                # ALT:MAYBE? gravitate cursor back to margin
                # [_] WTF:ERR:CASE: open "/" and scroll down -- it gravitates :(
                raise RuntimeWarning("RND:(invariant): cursor should be on margin")

        elif steps < 0:
            pidx = idx
            idx += steps
            if idx < 0:
                knock = idx
                idx = 0
            lines_step_up = sum(_fih(i) for i in range(idx, pidx))
            pos -= lines_step_up
            # NOTE: keep at margin, or step over it, or adhere to the end
            if pos <= margin:
                pos = margin
                if idx <= margin:
                    assert all(_fih(i) >= 1 for i in range(0, idx))
                    lines_top = sum(_fih(i) for i in range(0, idx))
                    if lines_top <= margin:
                        pos = lines_top
        elif steps > 0:
            ## NOTE: always jump to next item (whatever item size)
            # TODO: scroll by "steps" if item is larger than e.g. half-viewport
            pidx = idx
            idx += steps
            if idx > last:
                knock = idx - last
                idx = last
            # FUT: allow advancing by partial items
            lines_step_down = sum(_fih(i) for i in range(pidx, idx))
            pos += lines_step_down
            if pos >= bot - margin:
                pos = bot - margin
                if bot - pos >= last - idx:
                    assert all(_fih(i) >= 1 for i in range(idx, last + 1))
                    lines_bot = sum(_fih(i) for i in range(idx, last + 1)) - 1
                    if lines_bot <= margin:
                        pos = bot - lines_bot
                        assert 0 <= pos <= bot
        else:
            raise ValueError("unexpected")

        # FUT: don't assign if the same
        self._cursor_item_lstindex = idx
        self._viewport_followeditem_lstindex = idx
        self._viewport_followeditem_linesfromtop = pos
