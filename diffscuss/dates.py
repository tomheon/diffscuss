"""
Helper functions for diffscuss's notions of dates.
"""

import calendar
from datetime import datetime, timedelta
import re

DT_FORMAT = "%Y-%m-%dT%H:%M:%S%z"


# this is for internal parsing / formatting only, don't use it.
INTERNAL_DT_FORMAT_NO_TZ = "%Y-%m-%dT%H:%M:%S"


def utc_to_local(utc_dt):
    """
    From http://stackoverflow.com/a/13287083 (thanks J.F. Sebastian).
    """
    # get integer timestamp to avoid precision lost
    timestamp = calendar.timegm(utc_dt.timetuple())
    local_dt = datetime.fromtimestamp(timestamp)
    assert utc_dt.resolution >= timedelta(microseconds=1)
    return local_dt.replace(microsecond=utc_dt.microsecond)



def parse_to_local_dt(dt_s):
    return utc_to_local(parse_to_utc_dt(dt_s))


def parse_to_utc_dt(dt_s):
    dt_no_tz = dt_s[:-6]
    tz = dt_s[-5:]
    dt = datetime.strptime(dt_no_tz, INTERNAL_DT_FORMAT_NO_TZ)
    td, is_neg = _tz_to_timedelta(tz)
    if is_neg:
        dt += td
    else:
        dt -= td
    return dt


TZ_RE = re.compile(r"(\+|-)(\d{2})(\d{2})")

def _tz_to_timedelta(tz):
    match = TZ_RE.match(tz)
    hours = int(match.group(2))
    minutes = int(match.group(3))
    return (timedelta(seconds=((60 * minutes) +
                               (60 * 60 * hours))),
            match.group(1) == "-")
