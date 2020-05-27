# my-bencode

It is my own implementation of netstring and bencode in Clojure.

I implemented this, just as an exercise.

[![my-bencode Circle CI](https://circleci.com/gh/kolharsam/my-bencode.svg?style=svg)](https://app.circleci.com/pipelines/github/kolharsam/my-bencode) ![my-bencode CI](https://github.com/kolharsam/my-bencode/workflows/my-bencode%20CI/badge.svg?branch=master)

## Supported Methods

 - `read-netstring`
 - `write-netstring`
 - `write-bencode`
 
 TODO: `read-bencode`
 
 - Some method(s) to help users
   - ` gen-byte-seq` - converts a string to a byte-seq

## References

 - Netstrings (http://cr.yp.to/proto/netstrings.txt)
 - Bencode (https://en.wikipedia.org/wiki/Bencode)
