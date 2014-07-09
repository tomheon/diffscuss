import re

HEADER_LINE_RE = re.compile("^#[*]+ (\w+): (.*)$")


def parse_header(header_line):
    """
    Return (header-name, header-value), or None if the line can't be
    parsed.
    """
    match = HEADER_LINE_RE.match(header_line)
    if not match:
        return None
    return (match.group(1), match.group(2))
