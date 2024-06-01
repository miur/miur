#
# SPDX-FileCopyrightText: 2017 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#


class NodeSuperimposeTr(object):
    def __call__(self, g, node_uid, aug):
        conv = {}
        for uid in aug:
            if uid == aug.get_root():
                g[node_uid] = aug[uid]
                conv[uid] = node_uid
            else:
                conv[uid] = g.add_object(aug[uid])
        for uid in aug:
            for edge in aug.neighbors(uid):
                g.add_arrow(conv[uid], conv[edge])
