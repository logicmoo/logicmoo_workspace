# -*- coding: utf-8 -*-
# Copyright (C) 2011 by Florian Mounier, Kozea
# This file is part of brigit, licensed under a 3-clause BSD license.

"""
Log colorizer

"""
import os
import sys
import logging

FORMAT = (
    '\x1b[?77h$COLOR%(asctime)s $BOLD$COLOR%(name)s '
    '$RESET%(message)s at $BOLD$COLOR%(funcName)s:%(lineno)d$RESET\x1b[?77l')
FORMAT = os.getenv('LOG_COLORIZER_FORMAT', FORMAT)


if sys.platform == 'win32':
    try:
        import colorama
    except ImportError:
        raise ImportError(
            "The colorama library must be "
            "installed if you are on Windows")
    else:
        colorama.init()


class ColorFormatter(logging.Formatter):
    """Simple python logging colorizer formatter """

    level_colors = {
        'DEBUG': 32,
        'INFO': 34,
        'WARNING': 33,
        'ERROR': 31,
        'CRITICAL': 35
    }

    def format(self, record):
        """Format the record with colors."""
        message = logging.Formatter.format(self, record)
        if sys.version_info[0] < 3:
            if isinstance(message, str):
                message = message.encode('utf-8')
        return ('\x1b[0m%s\x1b[0m' %
                message
                .replace('$RESET', '\x1b[0m')
                .replace('$BOLD', '\x1b[1m')
                .replace('$COLOR', '\x1b[%dm' %
                         self.level_colors.get(record.levelname, 37)))


def make_colored_stream_handler(
        std=sys.stdout, level=logging.DEBUG, format=FORMAT):
    """Return a colored stream handler"""
    handler = logging.StreamHandler(std)
    handler.setLevel(level)
    try:
        fn = std.fileno()
    except:
        fn = None

    if fn is None or os.isatty(fn):
        handler.setFormatter(ColorFormatter(format))
    return handler


def get_color_logger(name=None, silent=False, **kwargs):
    """Like logging.getLogger but with colors"""
    logger = logging.getLogger(name)
    handler = make_colored_stream_handler(**kwargs)
    logger.addHandler(handler)
    logger.propagate = False
    if not silent:
        logger.setLevel(logging.DEBUG)
    return logger


def colorize(format=FORMAT):
    colorize_default_formatter(format)
    logging.root.handlers = [make_colored_stream_handler()]


def basicConfig(**kwargs):
    handler = make_colored_stream_handler()
    if kwargs.get('handlers'):
        kwargs['handlers'].append(handler)
    else:
        kwargs['handlers'] = [handler]

    return logging.basicConfig(**kwargs)


def colorize_default_formatter(format=FORMAT):
    logging._defaultFormatter = ColorFormatter(format)
