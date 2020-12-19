import sys

from lark import Lark, Transformer, v_args


grammar = """
threads: thread+

thread: comment+

comment: preamble body

preamble: preamble_line+

preamble_line: empty_preamble_line
             | header_line

empty_preamble_line: preamble_prefix

header_line: preamble_prefix header

header: header_key ":" header_value

%import common.WS_INLINE
%ignore WS_INLINE
"""

working_grammar = """
start: header_line

header_line: header_prefix header

header_prefix: /#\*+[ ]?/

?header: header_key ":" header_value -> header

?header_key: /[^:]+/ -> header_key

?header_value: /[^:].+/ -> header_value

%import common.WS
%ignore WS
"""

@v_args(inline=True)    
class ThreadMaker(Transformer):

    def header(self, key, val):
        return (key, val)

    def header_key(self, hk):
        return hk

    def header_value(self, hv):
        return hv


def main():
    parser = Lark(working_grammar, parser='lalr', transformer=ThreadMaker())
    print(parser.parse(open("t.txt").read()))


if __name__ == '__main__':
    main()
