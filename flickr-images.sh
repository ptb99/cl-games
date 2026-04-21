#! /bin/sh
#
# wrapper to lower GC threshold for sbcl and run flickr-images.lisp
#

sbcl --dynamic-space-size 256 --load flickr-images.lisp --quit
