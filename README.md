# my-bencode

It is just a crude and bad, and you can say wrong too, implementation of netstring and bencode in Clojure.

I implemented this, just as an exercise.

;; TODO: To implement the same set of functions with binary streams

[![my-bencode Circle CI](https://circleci.com/gh/kolharsam/my-bencode.svg?style=svg)](https://app.circleci.com/pipelines/github/kolharsam/my-bencode) ![my-bencode CI](https://github.com/kolharsam/my-bencode/workflows/my-bencode%20CI/badge.svg?branch=master)

## Supported Methods

 - `read-netstring`
 - `write-netstring`
 - `write-bencode`
 - `read-bencode`
 
 - Some method(s) to help users
   - ` gen-byte-seq` - converts a string to a byte-seq

## References

 - Netstrings (http://cr.yp.to/proto/netstrings.txt)
 - Bencode (https://en.wikipedia.org/wiki/Bencode)
