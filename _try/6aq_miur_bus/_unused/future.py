#
# SPDX-FileCopyrightText: 2016 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: GPL-3.0-only
#

# async def sum(x):
#     await asyncio.sleep(0.1)  # simulates asynchronously
#     return x

# async def consumer(i):
#     print("Consumer {} started".format(i))
#     while True:
#         f, x = await q.get()
#         print("Consumer {} procesing {}".format(i, x))
#         r = await sum(x)
#         f.set_result(r)

# async def producer():
#     consumers = [asyncio.ensure_future(consumer(i)) for i in range(5)]
#     # loop = asyncio.get_event_loop()
#     tasks = [(asyncio.Future(), x) for x in range(10)]
#     for task in tasks:
#         await q.put(task)
#     # wait until all futures are completed
#     results = await asyncio.gather(*[f for f, _ in tasks])
#     assert results == [r for _, r in tasks]
#     # destroy tasks
#     for c in consumers:
#         c.cancel()

# asyncio.get_event_loop().run_until_complete(producer())
