# my-bencode

It is my own implementation of netstring and bencode in Clojure.

This project was done as an exercise.

![my-bencode CI](https://github.com/kolharsam/my-bencode/workflows/my-bencode%20CI/badge.svg?branch=master)

## Supported Methods

 - `read-netstring`
 - `write-netstring`
 
 TODO: `read-bencode`, `write-bencode`
 
 - Some method(s) to help users
   - ` gen-byte-seq` - converts a string to a byte-seq

## References

 - Netstrings (http://cr.yp.to/proto/netstrings.txt)
 - Bencode (https://en.wikipedia.org/wiki/Bencode)
