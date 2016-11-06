# import logging

from miur.relay.loop import main_loop

# Quiet poll
# logging.getLogger('asyncio').setLevel(logging.WARNING)


def main(server_address):
    # DEV: pass mod-specific code into main_loop
    try:
        # Serve requests until Ctrl+C is pressed
        main_loop(server_address)
    except KeyboardInterrupt:
        pass
