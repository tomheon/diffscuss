from distutils.core import setup

setup(
    name='diffscuss',
    version='2.0.0',
    author='Edmund Jorgensen',
    author_email='edmund@hut8labs.com',
    packages=['diffscuss', 'diffscuss.support',
              'diffscuss.mailbox'],
    scripts=['bin/diffscuss'],
    url='http://github.com/hut8labs/diffscuss/',
    license='LICENSE.txt',
    description='Plain-text code review format and tools.',
    long_description='Version 2 introduces a breaking format change (use # instead of % to introduce diffscuss lines).',
    install_requires=[
        "PyGithub==1.14.2",
        "argparse==1.2.1",
        "requests==1.2.0"
    ],
)
