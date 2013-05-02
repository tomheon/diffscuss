from diffscuss.mailbox import check, set_default_inbox, make_inbox, \
    init_mailbox, post, bounce, done


mod_map = {'check': check,
           'set-default-inbox': set_default_inbox,
           'make-inbox': make_inbox,
           'init': init_mailbox,
           'post': post,
           'bounce': bounce,
           'done': done}


def main(args):
    mod_map[args.mailbox_subcommand_name].main(args)
