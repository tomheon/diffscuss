"""
An extremely dumb HTTP server intended to be a target for a Github
webhook and notify people by email when a review is added to their
inbox.

It assumes that the name of the inbox is the same as the first part of
the user's email--i.e., if an inbox is called "bob" and the domain
passed on the command line is "example.com", notifications will be
sent to "bob@example.com."

SMTP settings are read from the following env settings and starttls is
used:

DIFFSCUSS_NOTIFY_SMTP_USER

DIFFSCUSS_NOTIFY_SMTP_PASSWORD

DIFFSCUSS_NOTIFY_FROM

DIFFSCUSS_NOTIFY_SMTP_SERVER

Usage is: <port> <domain> <diffscuss_user_dir>

For example:

python diffscuss-notify.py 80 hut8labs.com diffscussions/users/
"""
import SimpleHTTPServer
import SocketServer
import cgi
import json
import sys
import os
import smtplib


def notify(addy, repo_name, repo_url):
    print "Sending to %s" % addy
    username = os.environ['DIFFSCUSS_NOTIFY_SMTP_USER']
    password = os.environ['DIFFSCUSS_NOTIFY_SMTP_PASSWORD']
    fromaddr = os.environ['DIFFSCUSS_NOTIFY_FROM']
    toaddrs = addy
    msg = \
        ("Subject: New Diffscussion in %s\r\n\r\n"
         "You have new diffscussions in %s (%s).") % (repo_name,
                                                      repo_name,
                                                      repo_url)

    server = smtplib.SMTP(os.environ['DIFFSCUSS_NOTIFY_SMTP_SERVER'])
    server.starttls()
    server.login(username, password)
    server.sendmail(fromaddr, toaddrs, msg)
    server.quit()


class ServerHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    def do_GET(self):
        SimpleHTTPServer.SimpleHTTPRequestHandler.do_GET(self)

    def do_POST(self):
        global diffscuss_user_dir
        global domain

        form = cgi.FieldStorage(
            fp=self.rfile,
            headers=self.headers,
            environ={"REQUEST_METHOD": "POST"})

        json_payload = form["payload"].value
        payload = json.loads(json_payload)
        for commit in payload[u"commits"]:
            for fpath in commit[u"added"]:
                fpath = fpath.encode('utf-8')
                if fpath.startswith(diffscuss_user_dir):
                    user = os.path.split(fpath[len(diffscuss_user_dir):])[0]
                    notify("%s@%s" % (user, domain),
                           payload["repository"]["name"],
                           payload["repository"]["url"])


if __name__ == '__main__':
    port = int(sys.argv[1])
    domain = sys.argv[2]
    diffscuss_user_dir = sys.argv[3]
    if not diffscuss_user_dir.endswith('/'):
        diffscuss_user_dir = diffscuss_user_dir + '/'

    Handler = ServerHandler

    httpd = SocketServer.TCPServer(("", port), Handler)

    print "serving at port", port
    httpd.serve_forever()
